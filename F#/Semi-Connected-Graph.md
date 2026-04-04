# Rosalind Problem: Semi-Connected Graph

## Problem Description
A directed graph is semi-connected if for any two vertices u and v, there is either a path from u to v or a path from v to u. Given a directed graph, determine whether it is semi-connected.

## Solution in F#

```fsharp
open System
open System.Collections.Generic

// Type definitions
type Vertex = int
type Graph = Map<Vertex, Set<Vertex>>

// Helper function to add edge to graph
let addEdge (graph: Graph) (from: Vertex) (toVertex: Vertex) : Graph =
    let neighbors = 
        match Map.tryFind from graph with
        | Some existing -> existing |> Set.add toVertex
        | None -> Set.singleton toVertex
    Map.add from neighbors graph

// Function to get all vertices in graph
let getVertices (graph: Graph) : Set<Vertex> =
    Set.union (Set.ofList (Map.keys graph |> Seq.toList)) 
              (graph 
               |> Seq.collect (fun (_, neighbors) -> Set.toList neighbors) 
               |> Set.ofList)

// Function to perform DFS and get reachable vertices
let dfs (graph: Graph) (start: Vertex) : Set<Vertex> =
    let rec dfsHelper (visited: Set<Vertex>) (stack: Stack<Vertex>) (reachable: Set<Vertex>) : Set<Vertex> =
        match stack.Count with
        | 0 -> reachable
        | _ ->
            let current = stack.Pop()
            let newReachable = Set.add current reachable
            let newVisited = Set.add current visited
            let neighbors = 
                match Map.tryFind current graph with
                | Some n -> n
                | None -> Set.empty
            let unvisitedNeighbors = 
                neighbors 
                |> Set.filter (fun v -> not (Set.contains v visited))
            for neighbor in unvisitedNeighbors do
                stack.Push(neighbor)
            dfsHelper newVisited stack newReachable
    
    let stack = new Stack<Vertex>()
    stack.Push(start)
    dfsHelper Set.empty stack Set.empty

// Function to check if graph is semi-connected
let isSemiConnected (graph: Graph) : bool =
    let vertices = getVertices graph
    
    // For each pair of vertices, check if there's a path from u to v or v to u
    let rec checkPairs (remaining: Set<Vertex>) : bool =
        match Set.isEmpty remaining with
        | true -> true
        | false ->
            let u = Set.minElement remaining
            let remainingWithoutU = Set.remove u remaining
            
            // Check if there's a path from u to any other vertex
            let reachableFromU = dfs graph u
            
            // For each other vertex v, check if there's a path from v to u or from u to v
            let rec checkRemaining (remainingVertices: Set<Vertex>) : bool =
                match Set.isEmpty remainingVertices with
                | true -> true
                | false ->
                    let v = Set.minElement remainingVertices
                    let remainingWithoutV = Set.remove v remainingVertices
                    
                    // Check if there's a path from u to v OR from v to u
                    let pathFromUToV = Set.contains v reachableFromU
                    let reachableFromV = dfs graph v
                    let pathFromVToU = Set.contains u reachableFromV
                    
                    if pathFromUToV || pathFromVToU then
                        checkRemaining remainingWithoutV
                    else
                        false
            
            if checkRemaining remainingWithoutU then
                checkPairs remainingWithoutU
            else
                false
    
    // Handle empty graph case
    if Set.isEmpty vertices then
        true
    else
        checkPairs vertices

// Alternative more efficient approach using Kosaraju's algorithm
// First, find strongly connected components
let kosaraju (graph: Graph) : Set<Set<Vertex>> =
    let vertices = getVertices graph
    
    // Helper function to get reverse graph
    let getReverseGraph (g: Graph) : Graph =
        let reverseEdges = ref Map.empty
        for (vertex, neighbors) in g do
            for neighbor in neighbors do
                reverseEdges := 
                    match Map.tryFind neighbor !reverseEdges with
                    | Some existing -> Map.add neighbor (Set.add vertex existing) !reverseEdges
                    | None -> Map.add neighbor (Set.singleton vertex) !reverseEdges
        !reverseEdges
    
    // First DFS to get finishing times
    let rec dfsFirst (visited: Set<Vertex>) (stack: Stack<Vertex>) (finishTimes: Stack<Vertex>) : unit =
        match stack.Count with
        | 0 -> ()
        | _ ->
            let current = stack.Pop()
            if not (Set.contains current visited) then
                let newVisited = Set.add current visited
                let neighbors = 
                    match Map.tryFind current graph with
                    | Some n -> n
                    | None -> Set.empty
                for neighbor in neighbors do
                    stack.Push(neighbor)
                dfsFirst newVisited stack finishTimes
                finishTimes.Push(current)
    
    // Second DFS on reverse graph
    let rec dfsSecond (visited: Set<Vertex>) (component: Set<Vertex>) (stack: Stack<Vertex>) : Set<Vertex> =
        match stack.Count with
        | 0 -> component
        | _ ->
            let current = stack.Pop()
            if not (Set.contains current visited) then
                let newVisited = Set.add current visited
                let neighbors = 
                    match Map.tryFind current (getReverseGraph graph) with
                    | Some n -> n
                    | None -> Set.empty
                for neighbor in neighbors do
                    stack.Push(neighbor)
                dfsSecond newVisited (Set.add current component) stack
            else
                dfsSecond visited component stack
    
    // Get finishing times
    let finishStack = new Stack<Vertex>()
    let visitedFirst = ref Set.empty
    for v in vertices do
        if not (Set.contains v !visitedFirst) then
            let stack = new Stack<Vertex>()
            stack.Push(v)
            dfsFirst !visitedFirst stack finishStack
            visitedFirst := !visitedFirst
    
    // Get strongly connected components
    let visitedSecond = ref Set.empty
    let components = ref Set.empty
    while finishStack.Count > 0 do
        let current = finishStack.Pop()
        if not (Set.contains current !visitedSecond) then
            let stack = new Stack<Vertex>()
            stack.Push(current)
            let component = dfsSecond !visitedSecond Set.empty stack
            components := Set.add component !components
            visitedSecond := Set.union !visitedSecond component
    
    !components

// Simplified approach for semi-connected check
let isSemiConnectedSimple (graph: Graph) : bool =
    let vertices = getVertices graph
    
    if Set.isEmpty vertices then
        true
    else
        // For each pair of vertices, check if there's a path between them
        let rec checkAllPairs (verticesList: Vertex list) : bool =
            match verticesList with
            | [] -> true
            | u :: rest ->
                let rec checkWithRest (restVertices: Vertex list) : bool =
                    match restVertices with
                    | [] -> true
                    | v :: other ->
                        let reachableFromU = dfs graph u
                        let reachableFromV = dfs graph v
                        let pathExists = (Set.contains v reachableFromU) || (Set.contains u reachableFromV)
                        if pathExists then
                            checkWithRest other
                        else
                            false
                if checkWithRest rest then
                    checkAllPairs rest
                else
                    false
        
        checkAllPairs (Set.toList vertices)

// Main function to solve the problem
let solveSemiConnectedGraph (input: string) : bool =
    // Parse input (assuming standard format)
    // This would depend on the actual input format from Rosalind
    let lines = input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    
    // For simplicity, assuming we have a graph in adjacency list format
    let graph = ref Map.empty
    
    for line in lines do
        if line.Trim() <> "" then
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            if parts.Length >= 2 then
                let from = int parts.[0]
                let toVertex = int parts.[1]
                graph := addEdge !graph from toVertex
    
    isSemiConnectedSimple !graph

// Example usage
let example1 = 
    """
    1 2
    2 3
    3 1
    """
    
let example2 = 
    """
    1 2
    2 3
    3 4
    """
    
// Test cases
let test1 = solveSemiConnectedGraph example1  // Should return true (cycle)
let test2 = solveSemiConnectedGraph example2  // Should return false (path)
```

## Explanation

This solution implements a semi-connected graph checker with the following approach:

1. **Graph Representation**: Uses a `Map<Vertex, Set<Vertex>>` to represent the directed graph where each vertex maps to a set of its neighbors.

2. **Key Functions**:
   - `addEdge`: Adds an edge to the graph
   - `dfs`: Performs depth-first search to find all reachable vertices from a starting vertex
   - `isSemiConnectedSimple`: Main function that checks if the graph is semi-connected

3. **Algorithm Logic**:
   - For each pair of vertices (u,v), check if there's a path from u to v OR from v to u
   - If all pairs satisfy this condition, the graph is semi-connected
   - Uses DFS to efficiently compute reachability

4. **Time Complexity**: O(V × (V + E)) where V is vertices and E is edges
5. **Space Complexity**: O(V + E) for storing the graph and visited sets

The solution handles edge cases like empty graphs and provides a clean, functional approach using F#'s immutable data structures and functional programming patterns.

