# Rosalind Problem: Compute the Squared Error Distortion

## Problem Description
Given a set of data points and a set of centers, compute the squared error distortion, which is the sum of squared distances from each data point to its nearest center, normalized by the number of data points.

## Solution in Go

```go
package main

import (
    "fmt"
    "math"
)

// euclideanDistance calculates the Euclidean distance between two points
func euclideanDistance(point1, point2 []float64) float64 {
    sum := 0.0
    for i := 0; i < len(point1); i++ {
        diff := point1[i] - point2[i]
        sum += diff * diff
    }
    return math.Sqrt(sum)
}

// findClosestCenter finds the index of the closest center to a point
func findClosestCenter(point []float64, centers [][]float64) int {
    minDistance := math.Inf(1)
    closestIndex := 0
    
    for i, center := range centers {
        distance := euclideanDistance(point, center)
        if distance < minDistance {
            minDistance = distance
            closestIndex = i
        }
    }
    return closestIndex
}

// computeSquaredErrorDistortion calculates the squared error distortion
func computeSquaredErrorDistortion(points [][]float64, centers [][]float64) float64 {
    totalError := 0.0
    n := len(points)
    
    for _, point := range points {
        closestCenterIndex := findClosestCenter(point, centers)
        closestCenter := centers[closestCenterIndex]
        distance := euclideanDistance(point, closestCenter)
        totalError += distance * distance
    }
    
    return totalError / float64(n)
}

func main() {
    // Example usage
    // Points: 2D coordinates
    points := [][]float64{
        {1.0, 2.0},
        {3.0, 4.0},
        {5.0, 6.0},
        {7.0, 8.0},
    }
    
    // Centers: 2D coordinates
    centers := [][]float64{
        {2.0, 3.0},
        {6.0, 7.0},
    }
    
    distortion := computeSquaredErrorDistortion(points, centers)
    fmt.Printf("Squared Error Distortion: %.6f\n", distortion)
    
    // Example from Rosalind problem format
    // Sample data format would be:
    // points = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
    // centers = [[2.0, 3.0], [4.0, 5.0]]
    // Expected output: 2.000000
    
    samplePoints := [][]float64{
        {1.0, 2.0},
        {3.0, 4.0},
        {5.0, 6.0},
    }
    
    sampleCenters := [][]float64{
        {2.0, 3.0},
        {4.0, 5.0},
    }
    
    sampleDistortion := computeSquaredErrorDistortion(samplePoints, sampleCenters)
    fmt.Printf("Sample Distortion: %.6f\n", sampleDistortion)
}
```

## Explanation

The solution works as follows:

1. **euclideanDistance**: Calculates the Euclidean distance between two points in n-dimensional space using the formula √(Σ(xi - yi)²)

2. **findClosestCenter**: For each data point, finds the nearest center by calculating distances to all centers and returning the index of the minimum distance

3. **computeSquaredErrorDistortion**: 
   - For each data point, finds its closest center
   - Calculates the squared distance from the point to its closest center
   - Sums all squared distances
   - Divides by the number of points to get the mean squared error

## Time Complexity
- O(n × k × d) where n is the number of points, k is the number of centers, and d is the dimensionality of the points

## Space Complexity
- O(1) additional space (excluding input storage)

## Sample Input/Output
For the sample data:
- Points: [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
- Centers: [[2.0, 3.0], [4.0, 5.0]]
- Output: 2.000000

This represents the average of squared distances from each point to its nearest center.

