# Rosalind Problem: Completing a Tree

## Problem Description
In this problem, we need to find the number of edges that must be added to a graph to make it a tree. A tree is a connected graph with no cycles, and it has exactly n-1 edges where n is the number of vertices.

## Solution in F#

```fsharp
open System
open System.IO
open System.Text.RegularExpressions

let parseInput (lines: string[]) =
    let n = int lines.[0]
    let edges = 
        lines.[1..]
        |> Array.map (fun line -> 
            let parts = line.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            (int parts.[0], int parts.[1]))
    (n, edges)

let countConnectedComponents (n: int) (edges: (int * int)[]) =
    // Create adjacency list representation
    let adjList = 
        [for i in 1..n -> i, Set.empty]
        |> Map.ofList
    
    let updatedAdjList = 
        edges
        |> Array.fold (fun acc (u, v) ->
            let uNeighbors = Map.find u acc
            let vNeighbors = Map.find v acc
            Map.add u (Set.add v uNeighbors) (Map.add v (Set.add u vNeighbors) acc)
        ) adjList
    
    // DFS to count connected components
    let rec dfs visited node components =
        let neighbors = Map.find node updatedAdjList
        let newVisited = Set.add node visited
        let unvisitedNeighbors = Set.difference neighbors newVisited
        
        match Set.isEmpty unvisitedNeighbors with
        | true -> components
        | false -> 
            let nextNode = Set.minElement unvisitedNeighbors
            dfs newVisited nextNode (components + 1)
    
    let rec countComponents visited components =
        if Set.count visited = n then components
        else
            let unvisitedNodes = 
                [for i in 1..n -> i]
                |> Set.ofList
                |> Set.difference visited
            let startNode = Set.minElement unvisitedNodes
            countComponents (Set.add startNode visited) (components + 1)
    
    countComponents Set.empty 0

let solve (n: int) (edges: (int * int)[]) =
    let numEdges = edges.Length
    let numVertices = n
    let numRequiredEdges = numVertices - 1
    let numConnectedComponents = countConnectedComponents n edges
    let numDisconnectedComponents = numConnectedComponents - 1
    numDisconnectedComponents

// Alternative simpler approach using Union-Find
let solveSimple (n: int) (edges: (int * int)[]) =
    // Union-Find data structure
    let parent = Array.create (n + 1) 0
    let rank = Array.create (n + 1) 0
    
    // Initialize
    for i in 1..n do
        parent.[i] <- i
        rank.[i] <- 0
    
    let find (x: int) =
        if parent.[x] <> x then
            parent.[x] <- find parent.[x]  // Path compression
        parent.[x]
    
    let union (x: int) (y: int) =
        let xRoot = find x
        let yRoot = find y
        
        if xRoot <> yRoot then
            // Union by rank
            if rank.[xRoot] < rank.[yRoot] then
                parent.[xRoot] <- yRoot
            elif rank.[xRoot] > rank.[yRoot] then
                parent.[yRoot] <- xRoot
            else
                parent.[yRoot] <- xRoot
                rank.[xRoot] <- rank.[xRoot] + 1
            1  // Successfully merged
        else
            0  // Already in same set
    
    // Process all edges
    let mergedComponents = 
        edges
        |> Array.sumBy (fun (u, v) -> union u v)
    
    // Number of components = total vertices - merged components
    let components = n - mergedComponents
    components - 1  // Edges needed to connect all components

// Main execution
let main () =
    try
        // Read input from file or stdin
        let lines = 
            if File.Exists "rosalind_tree.txt" then
                File.ReadAllLines "rosalind_tree.txt"
            else
                Console.ReadLine() |> fun _ -> [|
                    "10"
                    "1 2"
                    "2 8"
                    "4 10"
                    "5 9"
                    "6 10"
                    "7 9"
                    "8 10"
                    "9 10"
                |]
        
        let (n, edges) = parseInput lines
        let result = solveSimple n edges
        printfn "%d" result
        
    with
    | ex -> printfn "Error: %s" ex.Message

// Run the solution
main()
```

## Explanation

The problem asks us to find how many edges need to be added to make a graph into a tree. A tree with n vertices has exactly n-1 edges and is connected.

**Approach:**
1. **Parse Input**: Read the number of vertices and edges from input
2. **Count Connected Components**: Use Union-Find (Disjoint Set Union) to count how many separate components exist
3. **Calculate Result**: The number of edges needed = (number of components - 1)

**Key Points:**
- We use Union-Find data structure for efficient component tracking
- Union-Find with path compression and union by rank gives us O(α(n)) amortized time complexity
- If we have k connected components, we need k-1 edges to connect them all into one tree

**Time Complexity**: O(E × α(V)) where E is number of edges and V is number of vertices
**Space Complexity**: O(V)

The solution handles the case where we might have multiple disconnected components in the graph and calculates exactly how many additional edges are needed to make it a single connected tree.

