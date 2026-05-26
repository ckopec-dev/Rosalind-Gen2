# Rosalind Problem: Distances in Trees (DIST)

I'll solve this step by step using F#.

## Problem Understanding

The problem asks us to compute the distance matrix for a tree with n nodes. Given a tree represented as an adjacency list, we need to find the shortest path distances between all pairs of nodes.

## Solution Approach

1. Parse the input to build an adjacency list representation of the tree
2. For each pair of nodes, compute the shortest path using BFS
3. Output the distance matrix

## F# Implementation

```fsharp
open System
open System.Collections.Generic

// Type definitions
type Node = int
type Edge = Node * Node
type Graph = Map<Node, Set<Node>>

// Function to add an edge to the graph (undirected)
let addEdge (graph: Graph) (u: Node) (v: Node) : Graph =
    let updateNode node neighbors =
        match Map.tryFind node graph with
        | Some existing -> Map.add node (Set.add v existing) graph
        | None -> Map.add node (set [v]) graph
    
    updateNode u (Set.add v (Map.tryFind u graph |> Option.defaultValue Set.empty))
    |> updateNode v (Set.add u (Map.tryFind v graph |> Option.defaultValue Set.empty))

// Function to build graph from adjacency list
let buildGraph (edges: Edge list) : Graph =
    List.fold (fun graph (u, v) -> addEdge graph u v) Map.empty edges

// Function to perform BFS and find shortest distance between two nodes
let bfsDistance (graph: Graph) (start: Node) (target: Node) : int =
    if start = target then 0
    else
        let visited = HashSet<Node>()
        let queue = Queue<Node>()
        
        visited.Add(start) |> ignore
        queue.Enqueue(start)
        let mutable distance = 0
        
        while queue.Count > 0 do
            let current = queue.Dequeue()
            
            match Map.tryFind current graph with
            | Some neighbors ->
                for neighbor in neighbors do
                    if neighbor = target then
                        return distance + 1
                    if not (visited.Contains neighbor) then
                        visited.Add(neighbor) |> ignore
                        queue.Enqueue(neighbor)
            | None -> ()
            
            distance <- distance + 1
            
        -1 // No path found

// Function to compute distance matrix for all pairs
let computeDistanceMatrix (graph: Graph) : int[,] =
    let nodes = Map.keys graph |> Seq.toList
    let n = List.length nodes
    let matrix = Array2D.create n n 0
    
    for i in 0 .. n - 1 do
        for j in 0 .. n - 1 do
            if i = j then
                matrix.[i, j] <- 0
            else
                let distance = bfsDistance graph nodes.[i] nodes.[j]
                matrix.[i, j] <- distance
    
    matrix

// Main function to solve the problem
let solveDistancesInTrees (input: string list) : string list =
    // Parse input - assuming input is in the format:
    // First line: number of nodes
    // Following lines: adjacency list edges
    let lines = input |> List.filter (fun s -> s.Trim() <> "")
    
    if List.length lines < 2 then
        ["Invalid input"]
    else
        let n = int lines.[0]
        let edges = 
            lines.[1..]
            |> List.map (fun line ->
                let parts = line.Split([|' ' ; '\t'|], StringSplitOptions.RemoveEmptyEntries)
                if Array.length parts >= 2 then
                    let u = int parts.[0]
                    let v = int parts.[1]
                    (u, v)
                else
                    failwith "Invalid edge format")
        
        let graph = buildGraph edges
        let matrix = computeDistanceMatrix graph
        
        // Format output
        let result = ref []
        for i in 0 .. matrix.GetLength(0) - 1 do
            let row = 
                [for j in 0 .. matrix.GetLength(1) - 1 -> string matrix.[i, j]]
                |> String.concat " "
            result := row :: !result
        
        List.rev !result

// Alternative cleaner version for specific input format
let solveDistancesInTreesClean (edges: Edge list) : string list =
    let graph = buildGraph edges
    let nodes = Map.keys graph |> Seq.toList
    let n = List.length nodes
    let matrix = Array2D.create n n 0
    
    // Compute all pairwise distances
    for i in 0 .. n - 1 do
        for j in 0 .. n - 1 do
            if i = j then
                matrix.[i, j] <- 0
            else
                let distance = bfsDistance graph nodes.[i] nodes.[j]
                matrix.[i, j] <- distance
    
    // Format output
    [for i in 0 .. n - 1 ->
        [for j in 0 .. n - 1 -> string matrix.[i, j]]
        |> String.concat " "]

// Example usage with sample input
let sampleInput = [
    "4"
    "1 2"
    "2 3"
    "3 4"
    "4 1"
]

// For a more realistic example with actual tree structure:
let treeEdges = [
    (1, 2)
    (2, 3)
    (3, 4)
    (4, 5)
    (5, 6)
]

// Run the solution
let result = solveDistancesInTreesClean treeEdges
result |> List.iter (printfn "%s")
```

## Key Features of the Solution

1. **Graph Representation**: Uses a Map<Node, Set<Node>> structure to represent the adjacency list
2. **BFS Implementation**: Efficiently computes shortest paths between nodes
3. **Distance Matrix**: Builds a complete matrix of all pairwise distances
4. **Error Handling**: Includes basic validation for input parsing

## Time Complexity
- Building graph: O(E) where E is number of edges
- Computing all distances: O(N × (N + E)) where N is number of nodes
- Overall: O(N × (N + E))

## Space Complexity
- O(N + E) for storing the graph
- O(N²) for the distance matrix

## Example Output
For a tree with edges (1,2), (2,3), (3,4), (4,5), (5,6), the distance matrix would show:
```
0 1 2 3 4 5
1 0 1 2 3 4
2 1 0 1 2 3
3 2 1 0 1 2
4 3 2 1 0 1
5 4 3 2 1 0
```

This solution efficiently handles the distances in trees problem by leveraging BFS for shortest path computation and proper graph representation.

