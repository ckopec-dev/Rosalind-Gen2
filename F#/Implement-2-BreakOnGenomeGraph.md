# Rosalind Problem: Implement 2-BreakOnGenomeGraph in F#

## Problem Description
Implement the 2-BreakOnGenomeGraph algorithm that performs a 2-break operation on a genome graph.

## Solution

```fsharp
// Function to implement 2-BreakOnGenomeGraph
let twoBreakOnGenomeGraph (genomeGraph: int list list) (i: int) (iu: int) (j: int) (ju: int) : int list list =
    // Convert genome graph to adjacency list representation
    let adjacencies = 
        genomeGraph 
        |> List.collect (fun pair -> [pair.[0], pair.[1]; pair.[1], pair.[0]])
        |> Map.ofList
    
    // Remove the existing edges
    let updatedAdjacencies = 
        adjacencies 
        |> Map.remove i 
        |> Map.remove iu 
        |> Map.remove j 
        |> Map.remove ju
    
    // Add the new edges
    let finalAdjacencies = 
        updatedAdjacencies
        |> Map.add i ju
        |> Map.add iu j
    
    // Convert back to list of pairs
    let edges = 
        finalAdjacencies 
        |> Map.toList
        |> List.groupBy fst
        |> List.map (fun (node, pairs) -> 
            let targets = pairs |> List.map snd |> List.sort
            [node; targets.[0]])
    
    // Handle the case where we need to properly format the result
    let result = 
        edges 
        |> List.sortBy (fun x -> x.[0])
        |> List.collect (fun pair -> [pair.[0]; pair.[1]])
        |> List.chunkBySize 2
        |> List.map (fun chunk -> [chunk.[0]; chunk.[1]])
    
    result

// Alternative cleaner implementation
let twoBreakOnGenomeGraph2 (genomeGraph: int list list) (i: int) (iu: int) (j: int) (ju: int) : int list list =
    // Create a mapping of each node to its neighbors
    let neighbors = 
        genomeGraph 
        |> List.collect (fun [a; b] -> [a, b; b, a])
        |> Map.ofList
    
    // Remove the old edges
    let updatedNeighbors = 
        neighbors 
        |> Map.remove i 
        |> Map.remove iu 
        |> Map.remove j 
        |> Map.remove ju
    
    // Add the new edges
    let finalNeighbors = 
        updatedNeighbors
        |> Map.add i ju
        |> Map.add iu j
    
    // Convert back to adjacency list format
    finalNeighbors 
    |> Map.toList
    |> List.groupBy fst
    |> List.map (fun (node, pairs) -> 
        let neighbor = pairs |> List.map snd |> List.head
        [node; neighbor])
    |> List.sortBy (fun x -> x.[0])

// More robust implementation
let twoBreakOnGenomeGraph3 (genomeGraph: int list list) (i: int) (iu: int) (j: int) (ju: int) : int list list =
    // Create a list of all edges
    let edges = 
        genomeGraph 
        |> List.collect (fun [a; b] -> [a, b; b, a])
    
    // Remove the four edges that are part of the 2-break
    let filteredEdges = 
        edges 
        |> List.filter (fun (a, b) -> 
            not (a = i && b = iu || a = iu && b = i || 
                 a = j && b = ju || a = ju && b = j))
    
    // Add the new edges
    let newEdges = 
        [i, ju; iu, j]
    
    // Combine all edges and convert back to proper format
    let allEdges = filteredEdges @ newEdges
    
    // Group by first element and create pairs
    allEdges 
    |> List.groupBy fst
    |> List.map (fun (node, pairs) -> 
        let neighbor = pairs |> List.map snd |> List.head
        [node; neighbor])
    |> List.sortBy (fun x -> x.[0])

// Simple and clean version
let twoBreakOnGenomeGraphSimple (genomeGraph: int list list) (i: int) (iu: int) (j: int) (ju: int) : int list list =
    // Get all edges from genome graph
    let edges = 
        genomeGraph 
        |> List.collect (fun [a; b] -> [(a, b); (b, a)])
    
    // Remove the edges to be broken
    let edgesWithoutOld = 
        edges 
        |> List.filter (fun (a, b) -> 
            not ((a = i && b = iu) || (a = iu && b = i) || 
                 (a = j && b = ju) || (a = ju && b = j)))
    
    // Add the new edges
    let finalEdges = 
        edgesWithoutOld @ [(i, ju); (iu, j)]
    
    // Group by first element to form pairs
    finalEdges 
    |> List.groupBy fst
    |> List.map (fun (node, pairs) -> 
        let neighbor = pairs |> List.map snd |> List.head
        [node; neighbor])
    |> List.sortBy (fun x -> x.[0])
```

## Example Usage

```fsharp
// Example input
let genomeGraph = [[1; 2]; [3; 4]; [5; 6]; [7; 8]]
let i = 1
let iu = 2
let j = 3
let ju = 4

// Apply 2-break
let result = twoBreakOnGenomeGraphSimple genomeGraph i iu j ju
printfn "%A" result
```

## Explanation

The 2-break operation on a genome graph:
1. Takes a genome graph represented as a list of pairs (edges)
2. Identifies four nodes i, iu, j, ju that form two existing edges
3. Removes the edges (i, iu) and (j, ju)
4. Adds two new edges (i, ju) and (iu, j)
5. Returns the updated genome graph

The implementation:
- Converts the graph to an adjacency representation for easier manipulation
- Removes the specified edges
- Adds the new edges
- Returns the result in the proper format

This is a fundamental operation in genome rearrangement algorithms and is used in the 2-break distance calculation.

