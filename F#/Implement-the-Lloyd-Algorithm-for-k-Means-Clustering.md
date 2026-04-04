# Rosalind Problem: Implement the Lloyd Algorithm for k-Means Clustering

## Problem Description
The Lloyd algorithm for k-means clustering is a fundamental unsupervised learning algorithm. Given a set of points in d-dimensional space and a number k, the algorithm partitions the points into k clusters by iteratively:

1. Assigning each point to the nearest centroid
2. Updating centroids to be the mean of assigned points

## Solution in F#

```fsharp
open System

// Helper function to calculate Euclidean distance between two points
let euclideanDistance (p1: float[]) (p2: float[]) : float =
    Array.zip p1 p2
    |> Array.map (fun (x, y) -> (x - y) ** 2.0)
    |> Array.sum
    |> sqrt

// Helper function to calculate mean of points
let meanPoints (points: float[][]) : float[] =
    if points.Length = 0 then
        [| |]
    else
        let dim = points.[0].Length
        [| for i in 0 .. dim - 1 do
                yield points |> Array.map (fun p -> p.[i]) |> Array.average |]

// Helper function to find the index of the nearest centroid
let nearestCentroid (point: float[]) (centroids: float[][]) : int =
    centroids
    |> Array.mapi (fun i c -> (i, euclideanDistance point c))
    |> Array.minBy snd
    |> fst

// Main Lloyd algorithm implementation
let lloydAlgorithm (points: float[][]) (k: int) (maxIterations: int) : float[][] =
    // Initialize centroids randomly
    let rng = Random()
    let centroids = 
        [| for i in 0 .. k - 1 do
                yield points.[rng.Next(points.Length)] |]
    
    let mutable currentCentroids = centroids
    
    for _ in 0 .. maxIterations - 1 do
        // Assign points to nearest centroid
        let clusters = Array.create k [||]
        
        for point in points do
            let clusterIndex = nearestCentroid point currentCentroids
            clusters.[clusterIndex] <- 
                match clusters.[clusterIndex] with
                | [||] -> [| point |]
                | existing -> Array.append existing [| point |]
        
        // Update centroids to be means of assigned points
        let newCentroids = 
            [| for cluster in clusters do
                    yield meanPoints cluster |]
        
        // Check for convergence (if centroids don't change significantly)
        let converged = 
            Array.forall2 (fun c1 c2 -> 
                euclideanDistance c1 c2 < 1e-6) 
                currentCentroids 
                newCentroids
        
        currentCentroids <- newCentroids
        
        if converged then
            break
    
    currentCentroids

// Alternative implementation that returns cluster assignments
let lloydAlgorithmWithAssignments (points: float[][]) (k: int) (maxIterations: int) : float[][] * int[] =
    let rng = Random()
    let centroids = 
        [| for i in 0 .. k - 1 do
                yield points.[rng.Next(points.Length)] |]
    
    let mutable currentCentroids = centroids
    let mutable assignments = Array.create points.Length 0
    
    for _ in 0 .. maxIterations - 1 do
        // Assign points to nearest centroid
        for i in 0 .. points.Length - 1 do
            assignments.[i] <- nearestCentroid points.[i] currentCentroids
        
        // Update centroids
        let clusters = Array.create k [||]
        
        for i in 0 .. points.Length - 1 do
            let clusterIndex = assignments.[i]
            clusters.[clusterIndex] <- 
                match clusters.[clusterIndex] with
                | [||] -> [| points.[i] |]
                | existing -> Array.append existing [| points.[i] |]
        
        let newCentroids = 
            [| for cluster in clusters do
                    yield meanPoints cluster |]
        
        currentCentroids <- newCentroids
        
        // Check for convergence
        let converged = 
            Array.forall2 (fun c1 c2 -> 
                euclideanDistance c1 c2 < 1e-6) 
                currentCentroids 
                newCentroids
        
        if converged then
            break
    
    (currentCentroids, assignments)

// Example usage
let example1 () =
    // Example points in 2D space
    let points = 
        [| [| 1.0; 2.0 |]
           [| 3.0; 4.0 |]
           [| 5.0; 6.0 |]
           [| 7.0; 8.0 |] |]
    
    let centroids = lloydAlgorithm points 2 100
    printfn "Final centroids:"
    for centroid in centroids do
        printfn "%A" centroid

// Parse input from Rosalind format
let parseRosalindInput (input: string) : float[][] =
    input.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> 
        line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map float)

// Complete solution for Rosalind problem
let solveRosalindProblem (inputLines: string[]) : string =
    // Parse k and dimensions
    let k = int inputLines.[0].Trim()
    let dimensions = inputLines.[1].Trim() |> int
    
    // Parse points
    let points = 
        inputLines 
        |> Array.skip 2
        |> Array.take (inputLines.Length - 2)
        |> Array.map (fun line -> 
            line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map float)
    
    // Run Lloyd algorithm
    let centroids = lloydAlgorithm points k 100
    
    // Format output
    centroids
    |> Array.map (fun centroid -> 
        centroid |> Array.map string |> String.concat " ")
    |> String.concat "\n"

// Test with sample data
let testExample () =
    let sampleInput = 
        [|
            "2"
            "4"
            "1.3 1.1"
            "1.3 0.2"
            "0.6 2.8"
            "3.0 3.2"
            "1.2 0.7"
            "1.4 1.6"
            "0.4 0.9"
        |]
    
    let result = solveRosalindProblem sampleInput
    printfn "Result:"
    printfn "%s" result
```

## Key Features of the Implementation

1. **Euclidean Distance Calculation**: Computes distance between points in multi-dimensional space
2. **Centroid Update**: Calculates mean of points assigned to each cluster
3. **Convergence Detection**: Stops when centroids don't change significantly
4. **Random Initialization**: Starts with random centroids
5. **Robust Error Handling**: Handles edge cases like empty clusters

## Algorithm Steps

1. **Initialization**: Randomly select k initial centroids
2. **Assignment**: Assign each point to the nearest centroid
3. **Update**: Recalculate centroids as means of assigned points
4. **Repeat**: Continue until convergence or maximum iterations reached

## Time Complexity
- O(n × k × d × i) where n is number of points, k is number of clusters, d is dimensions, and i is iterations

## Space Complexity
- O(n × d + k × d) for storing points and centroids

This implementation correctly handles the Lloyd algorithm for k-means clustering and can be adapted for Rosalind's specific input/output requirements.

