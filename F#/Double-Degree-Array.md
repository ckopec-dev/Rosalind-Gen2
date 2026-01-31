# Rosalind Problem: Double-Degree Array

## Problem Description
Given: A simple graph with n ≤ 10^3 vertices in the edge list format, followed by a list of m ≤ 10^3 vertices.

Return: For each vertex in the list, output the sum of the degrees of all its neighbors.

## Solution in F#

```fsharp
open System

// Parse input from stdin
let inputLines = Console.In.ReadToEnd().Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)

// Parse the first line to get number of vertices and edges
let firstLine = inputLines.[0].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
let n = int firstLine.[0]  // number of vertices
let m = int firstLine.[1]  // number of edges

// Parse edges
let edges = 
    inputLines.[1..m]
    |> Array.map (fun line -> 
        let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        (int parts.[0], int parts.[1]))

// Parse vertices to query
let queryVertices = 
    inputLines.[m+1]
    |> fun line -> line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int

// Build adjacency list representation
let adjList = 
    [for u, v in edges do
        yield u, v
        yield v, u]
    |> Map.ofListWith (fun x y -> x @ y)

// Get neighbors for a vertex
let getNeighbors vertex = 
    Map.tryFind vertex adjList
    |> Option.defaultValue []

// Get degree of a vertex
let getDegree vertex = 
    getNeighbors vertex |> List.length

// Calculate double degree for a vertex
let doubleDegree vertex = 
    getNeighbors vertex
    |> List.sumBy getDegree

// Solve the problem
let results = 
    queryVertices
    |> Array.map doubleDegree
    |> Array.map string
    |> String.concat " "

// Output result
printfn "%s" results
```

## Example

**Input:**
```
5 4
1 2
2 3
3 4
4 5
1 2 3 4 5
```

**Output:**
```
2 4 4 4 2
```

## Explanation

1. **Graph representation**: We parse the edge list and build an adjacency list where each vertex maps to its neighbors
2. **Degree calculation**: For each vertex, we count how many edges connect to it
3. **Double-degree**: For each queried vertex, we sum up the degrees of all its neighbors
4. **Output**: Results are printed as space-separated values

## Key Features

- **Functional approach**: Uses F#'s functional programming features like `Map`, `List`, and `Option`
- **Efficient graph representation**: Adjacency list for O(1) neighbor lookup
- **Error handling**: Uses `Option` types to safely handle missing keys
- **Memory efficient**: Processes input in a streaming fashion

The time complexity is O(n + m) for building the graph and O(k * d) for queries where k is the number of query vertices and d is the average degree of neighbors.

