# Rosalind Problem: Reversal Distance

## Problem Description
The reversal distance between two permutations is the minimum number of reversals required to transform one permutation into another.

## Solution

```kotlin
import java.io.File

fun main() {
    val input = File("rosalind_rear.txt").readText().trim()
    val lines = input.split("\n\n")
    
    val results = mutableListOf<Int>()
    
    for (line in lines) {
        val permutations = line.split("\n")
        val perm1 = permutations[0].split(" ").map { it.toInt() }
        val perm2 = permutations[1].split(" ").map { it.toInt() }
        
        val distance = findReversalDistance(perm1, perm2)
        results.add(distance)
    }
    
    println(results.joinToString(" "))
}

fun findReversalDistance(perm1: List<Int>, perm2: List<Int>): Int {
    // Convert to 0-indexed for easier manipulation
    val p1 = perm1.map { it - 1 }
    val p2 = perm2.map { it - 1 }
    
    // If already equal, return 0
    if (p1 == p2) return 0
    
    // Use BFS to find minimum reversals
    val queue = mutableListOf<Pair<List<Int>, Int>>()
    val visited = mutableSetOf<List<Int>>()
    
    queue.add(Pair(p1, 0))
    visited.add(p1)
    
    while (queue.isNotEmpty()) {
        val (currentPerm, distance) = queue.removeAt(0)
        
        // Try all possible reversals
        for (i in 0 until currentPerm.size) {
            for (j in i + 1 until currentPerm.size) {
                val newPerm = reverseSubarray(currentPerm, i, j)
                
                if (newPerm == p2) {
                    return distance + 1
                }
                
                if (newPerm !in visited) {
                    visited.add(newPerm)
                    queue.add(Pair(newPerm, distance + 1))
                }
            }
        }
    }
    
    return -1 // Should never reach here for valid inputs
}

fun reverseSubarray(list: List<Int>, start: Int, end: Int): List<Int> {
    val result = list.toMutableList()
    var i = start
    var j = end
    
    while (i < j) {
        result[i] = list[j]
        result[j] = list[i]
        i++
        j--
    }
    
    return result
}

// Alternative implementation with more efficient approach
fun findReversalDistanceOptimized(perm1: List<Int>, perm2: List<Int>): Int {
    // Convert to 0-indexed
    val p1 = perm1.map { it - 1 }
    val p2 = perm2.map { it - 1 }
    
    // Create a mapping from value to position in target permutation
    val positionMap = mutableMapOf<Int, Int>()
    for (i in p2.indices) {
        positionMap[p2[i]] = i
    }
    
    // Find minimum number of reversals using greedy approach
    val current = p1.toMutableList()
    var reversals = 0
    
    for (i in current.indices) {
        val targetValue = p2[i]
        val targetPosition = positionMap[targetValue]!!
        
        if (current[i] != targetValue) {
            // Find where targetValue is currently located
            val currentPosition = current.indexOf(targetValue)
            
            // If it's not adjacent, we need to do a reversal
            if (currentPosition != i + 1) {
                // Reverse the subarray from i to currentPosition
                reverseRange(current, i, currentPosition)
                reversals++
            }
            
            // Reverse the subarray from i to targetPosition
            reverseRange(current, i, targetPosition)
            reversals++
            
            // Adjust the position mapping after reversal
            for (j in current.indices) {
                positionMap[current[j]] = j
            }
        }
    }
    
    return reversals
}

fun reverseRange(list: MutableList<Int>, start: Int, end: Int) {
    var i = start
    var j = end
    
    while (i < j) {
        val temp = list[i]
        list[i] = list[j]
        list[j] = temp
        i++
        j--
    }
}
```

## Approach Explanation

1. **Input Parsing**: Read the input file containing pairs of permutations separated by blank lines
2. **BFS Search**: Use breadth-first search to find the minimum number of reversals
3. **Reversal Operation**: For each permutation, try all possible reversals (all subarrays)
4. **State Tracking**: Keep track of visited permutations to avoid cycles

## Key Functions

- `findReversalDistance`: Main function that computes the minimum reversal distance
- `reverseSubarray`: Reverses a subarray within a permutation
- `reverseRange`: In-place reversal of a range in a mutable list

## Time Complexity
- **BFS approach**: O(n! × n²) in worst case, where n is the length of permutation
- **Optimized approach**: More efficient using greedy strategy

## Space Complexity
- O(n! × n) for storing visited states in BFS approach

The solution handles the specific problem requirements for computing reversal distance between two permutations as specified in the Rosalind problem set.

