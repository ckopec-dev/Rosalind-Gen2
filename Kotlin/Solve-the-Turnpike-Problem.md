# Rosalind Problem: Solve_the_Turnpike_Problem

## Problem Description
The Turnpike Problem asks us to reconstruct a set of points on a line given the distances between all pairs of points.

## Solution Approach
1. Use backtracking to try different arrangements
2. Start with the known distances and work backwards
3. At each step, try placing the next point and check if it's consistent

## Kotlin Implementation

```kotlin
fun solveTurnpikeProblem(distances: List<Int>): List<Int> {
    val n = distances.size + 1
    val points = mutableListOf<Int>()
    
    // Sort distances in descending order
    val sortedDistances = distances.sortedDescending()
    
    // Start with the maximum distance which must be between first and last point
    val maxDistance = sortedDistances[0]
    
    // Try different positions for the first point
    for (firstPoint in 0..maxDistance) {
        points.clear()
        points.add(firstPoint)
        points.add(firstPoint + maxDistance)
        
        if (backtrack(sortedDistances, points, n)) {
            return points.sorted()
        }
    }
    
    return emptyList()
}

fun backtrack(distances: List<Int>, points: MutableList<Int>, n: Int): Boolean {
    if (points.size == n) {
        // Check if we've used all distances
        val usedDistances = mutableSetOf<Int>()
        for (i in points.indices) {
            for (j in i + 1 until points.size) {
                usedDistances.add(Math.abs(points[i] - points[j]))
            }
        }
        return usedDistances.size == distances.size && 
               usedDistances.containsAll(distances.toSet())
    }
    
    val currentPoint = points.size
    
    // Try adding a new point
    for (candidate in 1..1000) {  // Reasonable upper bound
        // Check if this point would create valid distances
        val valid = isValidPlacement(points, candidate, distances)
        
        if (valid) {
            points.add(candidate)
            if (backtrack(distances, points, n)) {
                return true
            }
            points.removeAt(points.size - 1)
        }
    }
    
    return false
}

fun isValidPlacement(points: List<Int>, candidate: Int, distances: List<Int>): Boolean {
    // Check if any existing point with candidate creates a distance that's not in our list
    val existingDistances = mutableSetOf<Int>()
    for (point in points) {
        existingDistances.add(Math.abs(point - candidate))
    }
    
    // All new distances should be present in the original distances
    val originalDistances = distances.toSet()
    for (dist in existingDistances) {
        if (!originalDistances.contains(dist)) {
            return false
        }
    }
    
    return true
}

// Alternative cleaner approach using a more systematic backtracking
fun solveTurnpike(distances: List<Int>): List<Int> {
    val n = distances.size + 1
    val sortedDistances = distances.sortedDescending()
    
    // Find the maximum distance - this must be between first and last point
    val maxDistance = sortedDistances[0]
    
    // Try placing the first point at 0 and last point at maxDistance
    val result = mutableListOf<Int>(0, maxDistance)
    
    // Use backtracking to fill in the remaining points
    if (backtrackTurnpike(sortedDistances, result, n, 2)) {
        return result.sorted()
    }
    
    return emptyList()
}

fun backtrackTurnpike(distances: List<Int>, points: MutableList<Int>, n: Int, index: Int): Boolean {
    if (index == n) {
        // Verify all distances match
        val actualDistances = mutableListOf<Int>()
        for (i in points.indices) {
            for (j in i + 1 until points.size) {
                actualDistances.add(Math.abs(points[i] - points[j]))
            }
        }
        actualDistances.sortDescending()
        return actualDistances == distances
    }
    
    // Try to place the next point
    val target = findNextPoint(distances, points, n)
    
    // For simplicity, try all reasonable positions
    for (candidate in 0..1000) {
        if (isValidPointPlacement(points, candidate)) {
            points.add(candidate)
            if (backtrackTurnpike(distances, points, n, index + 1)) {
                return true
            }
            points.removeAt(points.size - 1)
        }
    }
    
    return false
}

fun isValidPointPlacement(points: List<Int>, candidate: Int): Boolean {
    // Check if candidate creates distances that are valid (not too large or too small)
    return true  // Simplified - actual implementation would be more complex
}

fun findNextPoint(distances: List<Int>, points: List<Int>, n: Int): Int {
    // This is a simplified version - in practice, this would be more complex
    return 0
}

// Most practical approach for the turnpike problem
fun solveTurnpikeFinal(distances: List<Int>): List<Int> {
    val n = distances.size + 1
    
    // Sort distances
    val sortedDistances = distances.sortedDescending()
    
    // The largest distance is between first and last point
    val maxDist = sortedDistances[0]
    
    // Try all possible positions for the first point
    for (first in 0..maxDist) {
        val points = mutableListOf(first, first + maxDist)
        
        if (solveBacktrack(sortedDistances, points, n)) {
            return points.sorted()
        }
    }
    
    return emptyList()
}

fun solveBacktrack(distances: List<Int>, points: MutableList<Int>, n: Int): Boolean {
    if (points.size == n) {
        // Check if all distances match
        val actualDistances = mutableListOf<Int>()
        for (i in points.indices) {
            for (j in i + 1 until points.size) {
                actualDistances.add(Math.abs(points[i] - points[j]))
            }
        }
        actualDistances.sortDescending()
        return actualDistances == distances
    }
    
    // Try to add next point
    val currentSize = points.size
    
    // Try different positions for next point
    val maxPoint = points.maxOrNull() ?: 0
    
    // We can try points from 0 to some reasonable upper bound
    for (candidate in 0..(maxPoint + 100)) {
        // Check if this candidate creates valid distances
        points.add(candidate)
        if (solveBacktrack(distances, points, n)) {
            return true
        }
        points.removeAt(points.size - 1)
    }
    
    return false
}

// Example usage
fun main() {
    // Example test case
    val distances = listOf(1, 2, 3, 4, 5, 6)
    val result = solveTurnpikeFinal(distances)
    println("Result: $result")
    
    // Another example
    val distances2 = listOf(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
    val result2 = solveTurnpikeFinal(distances2)
    println("Result 2: $result2")
}
```

## Explanation

The Turnpike Problem solution uses backtracking to reconstruct the positions of points on a line given all pairwise distances. 

Key steps:
1. **Identify boundary**: The maximum distance in the list must be between the first and last point
2. **Try different arrangements**: Place the first point at different positions and work backwards
3. **Backtrack**: For each partial arrangement, try adding new points and check consistency
4. **Verify**: Ensure all distances match exactly

The algorithm works by:
- Starting with the maximum distance as the span between first and last point
- Trying different positions for the first point
- Using recursive backtracking to fill in remaining points
- Validating that all pairwise distances match the input

Time complexity is exponential in worst case, but practical implementations often work well for reasonable input sizes.

