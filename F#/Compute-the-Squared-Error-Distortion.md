# Rosalind Problem: Compute the Squared Error Distortion

## Problem Description
Given a set of data points and a set of centers, compute the squared error distortion, which is the sum of squared distances from each data point to its nearest center, divided by the number of data points.

## Solution in F#

```fsharp
open System

// Function to compute Euclidean distance between two points
let euclideanDistance (p1: float[]) (p2: float[]) : float =
    Array.zip p1 p2
    |> Array.map (fun (x, y) -> (x - y) ** 2.0)
    |> Array.sum
    |> sqrt

// Function to find the index of the nearest center
let findNearestCenter (point: float[]) (centers: float[][]) : int =
    centers
    |> Array.mapi (fun i center -> (i, euclideanDistance point center))
    |> Array.minBy snd
    |> fst

// Function to compute squared error distortion
let computeSquaredErrorDistortion (data: float[][]) (centers: float[][]) : float =
    let distances = 
        data
        |> Array.map (fun point -> 
            let nearestCenterIndex = findNearestCenter point centers
            let nearestCenter = centers.[nearestCenterIndex]
            euclideanDistance point nearestCenter ** 2.0)
    
    distances |> Array.sum / float data.Length

// Example usage
let main () =
    // Example data points (3D points)
    let data = 
        [| [| 2.3; 4.5 |]
           [| 1.2; 3.7 |]
           [| 3.1; 2.9 |]
           [| 2.8; 4.1 |] |]
    
    // Example centers
    let centers = 
        [| [| 2.0; 4.0 |]
           [| 3.0; 3.0 |] |]
    
    let distortion = computeSquaredErrorDistortion data centers
    printfn "Squared Error Distortion: %.4f" distortion

// Run the example
main()
```

## Explanation

The solution consists of three main functions:

1. **`euclideanDistance`**: Computes the Euclidean distance between two points in n-dimensional space
2. **`findNearestCenter`**: For a given data point, finds the index of the nearest center
3. **`computeSquaredErrorDistortion`**: Computes the final distortion value by:
   - Finding the nearest center for each data point
   - Computing the squared distance from each point to its nearest center
   - Taking the average of all squared distances

## Key Features

- **Functional approach**: Uses F#'s functional programming features like `Array.map`, `Array.mapi`, and `Array.minBy`
- **Generic dimensionality**: Works with points of any dimension
- **Efficient computation**: Uses array operations for optimal performance
- **Clear separation of concerns**: Each function has a single, well-defined responsibility

## Time Complexity
- O(n × k × d) where n is the number of data points, k is the number of centers, and d is the dimensionality

## Space Complexity
- O(n) for storing the distances

This solution handles the core requirements of computing squared error distortion efficiently and correctly.

