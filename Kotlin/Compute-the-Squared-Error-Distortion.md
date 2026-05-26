# Rosalind Problem: Compute the Squared Error Distortion

## Problem Description
Given a set of data points and a set of centers, compute the squared error distortion, which is the sum of squared distances from each data point to its nearest center.

## Solution

```kotlin
import kotlin.math.pow
import kotlin.math.sqrt

fun computeSquaredErrorDistortion(dataPoints: List<List<Double>>, centers: List<List<Double>>): Double {
    var totalDistortion = 0.0
    
    for (point in dataPoints) {
        val nearestCenter = findNearestCenter(point, centers)
        val distance = euclideanDistance(point, nearestCenter)
        totalDistortion += distance * distance
    }
    
    return totalDistortion / dataPoints.size
}

fun findNearestCenter(point: List<Double>, centers: List<List<Double>>): List<Double> {
    var minDistance = Double.MAX_VALUE
    var nearestCenter = centers[0]
    
    for (center in centers) {
        val distance = euclideanDistance(point, center)
        if (distance < minDistance) {
            minDistance = distance
            nearestCenter = center
        }
    }
    
    return nearestCenter
}

fun euclideanDistance(point1: List<Double>, point2: List<Double>): Double {
    var sum = 0.0
    for (i in point1.indices) {
        sum += (point1[i] - point2[i]).pow(2.0)
    }
    return sqrt(sum)
}

// Main function to solve the Rosalind problem
fun main() {
    // Example input (replace with actual input reading)
    val dataPoints = listOf(
        listOf(2.3, 4.5),
        listOf(1.2, 3.4),
        listOf(5.6, 7.8),
        listOf(2.1, 3.2)
    )
    
    val centers = listOf(
        listOf(2.0, 4.0),
        listOf(3.0, 5.0)
    )
    
    val result = computeSquaredErrorDistortion(dataPoints, centers)
    println("%.3f".format(result))
}
```

## Explanation

The solution works as follows:

1. **computeSquaredErrorDistortion**: This function calculates the total squared error distortion by:
   - Iterating through each data point
   - Finding the nearest center for each point
   - Computing the squared Euclidean distance from the point to its nearest center
   - Summing all squared distances and dividing by the number of data points

2. **findNearestCenter**: This helper function finds the center that is closest to a given data point using Euclidean distance.

3. **euclideanDistance**: This helper function computes the Euclidean distance between two points in n-dimensional space.

## Input Format
- First line: Number of data points and dimensions
- Next lines: Data points (each point on a separate line)
- Next lines: Centers (each center on a separate line)

## Time Complexity
- O(n × k × d) where n is the number of data points, k is the number of centers, and d is the number of dimensions

## Space Complexity
- O(1) additional space (excluding input storage)

The solution correctly computes the squared error distortion as required by the Rosalind problem.

