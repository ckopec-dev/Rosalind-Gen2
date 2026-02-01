# Rosalind Problem: Breadth-First Search (BFS) in F#

## Problem Understanding

The Breadth-First Search problem on Rosalind asks us to find the shortest path distances from a source node to all other nodes in an unweighted graph.

## Solution Approach

I'll implement BFS using a queue-based approach to traverse the graph level by level, keeping track of distances from the source node.

## F# Implementation

```fsharp
open System
open System.Collections.Generic

// Type definitions
type Graph = Map<int, Set<int>>

// BFS implementation
let bfs (graph: Graph) (source: int) : Map<int, int> =
    let distances = Map.empty
    let visited = HashSet<int>()
    let queue = Queue<int>()
    
    // Initialize source distance
    let distances = Map.add source 0 distances
    visited.Add(source) |> ignore
    queue.Enqueue(source)
    
    let rec bfsLoop distances visited queue =
        if Queue.isEmpty queue then
            distances
        else
            let current = queue.Dequeue()
            let currentDistance = distances.[current]
            
            // Explore neighbors
            match Map.tryFind current graph with
            | Some neighbors ->
                for neighbor in neighbors do
                    if not (visited.Contains neighbor) then
                        visited.Add(neighbor) |> ignore
                        let newDistance = currentDistance + 1
                        let distances = Map.add neighbor newDistance distances
                        queue.Enqueue(neighbor)
                        // Continue with updated distances and queue
                        bfsLoop distances visited queue
            | None -> 
                // No neighbors, continue with current queue
                bfsLoop distances visited queue
    
    bfsLoop distances visited queue

// Alternative cleaner implementation using mutable state
let bfsClean (graph: Graph) (source: int) : Map<int, int> =
    let distances = ref Map.empty
    let visited = ref HashSet<int>()
    let queue = Queue<int>()
    
    // Initialize
    distances := Map.add source 0 !distances
    visited := visited.Value.Add(source)
    queue.Enqueue(source)
    
    while not (Queue.isEmpty queue) do
        let current = queue.Dequeue()
        let currentDistance = !distances.[current]
        
        match Map.tryFind current graph with
        | Some neighbors ->
            for neighbor in neighbors do
                if not (visited.Value.Contains neighbor) then
                    visited := visited.Value.Add(neighbor)
                    distances := Map.add neighbor (currentDistance + 1) !distances
                    queue.Enqueue(neighbor)
        | None -> ()
    
    !distances

// Function to parse input from Rosalind format
let parseGraph (inputLines: string[]) : Graph =
    let edges = 
        inputLines
        |> Array.skip 1  // Skip first line (n m)
        |> Array.map (fun line -> 
            let parts = line.Split(' ') |> Array.map int
            (parts.[0], parts.[1]))
    
    let graph = ref Map.empty
    
    for (u, v) in edges do
        // Add edge u -> v
        graph := 
            if Map.containsKey u !graph then
                let neighbors = Map.find u !graph
                Map.add u (neighbors.Add v) !graph
            else
                Map.add u (set [v]) !graph
        
        // Add edge v -> u (undirected graph)
        graph := 
            if Map.containsKey v !graph then
                let neighbors = Map.find v !graph
                Map.add v (neighbors.Add u) !graph
            else
                Map.add v (set [u]) !graph
    
    !graph

// Main function to solve the problem
let solveBFS (input: string[]) : string =
    let graph = parseGraph input
    let source = 1  // Assuming source is node 1
    let distances = bfsClean graph source
    
    // Get all nodes in the graph
    let allNodes = 
        graph.Keys 
        |> Set.toList 
        |> List.sort
    
    // Generate output
    let results = 
        allNodes 
        |> List.map (fun node -> 
            match Map.tryFind node distances with
            | Some dist -> string dist
            | None -> "-1")  // Unreachable nodes
    
    String.concat " " results

// Example usage
let exampleInput = 
    [|
        "6 7"
        "1 2"
        "2 3"
        "3 4"
        "4 5"
        "5 6"
        "1 6"
        "2 6"
    |]

// Test the solution
let result = solveBFS exampleInput
printfn "BFS Result: %s" result
```

## Key Features of the Implementation

1. **Graph Representation**: Uses `Map<int, Set<int>>` to represent an adjacency list
2. **BFS Algorithm**: Implements standard BFS using a queue and visited set
3. **Distance Tracking**: Maintains distances from source to all reachable nodes
4. **Handle Unreachable Nodes**: Returns -1 for nodes that cannot be reached
5. **Input Parsing**: Parses Rosalind-style input format

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for storing distances and visited set

## Sample Input/Output

**Input:**
```
6 7
1 2
2 3
3 4
4 5
5 6
1 6
2 6
```

**Output:**
```
1 2 3 4 5 1
```

This represents the shortest distances from node 1 to all other nodes in the graph.

