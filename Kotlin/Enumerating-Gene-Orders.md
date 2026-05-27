# Rosalind Problem: Enumerating Gene Orders (PERM)

## Problem Description
Given a positive integer n ≤ 7, return the total number of permutations of length n, followed by a list of all such permutations (in any order).

## Solution

```kotlin
import java.io.File

fun main() {
    val n = 3  // Example input
    val permutations = generatePermutations(n)
    
    println(permutations.size)
    permutations.forEach { perm ->
        println(perm.joinToString(" "))
    }
}

fun generatePermutations(n: Int): List<List<Int>> {
    val numbers = (1..n).toList()
    val result = mutableListOf<List<Int>>()
    
    // Generate all permutations using backtracking
    generatePermutationsHelper(numbers, mutableListOf(), result)
    
    return result
}

fun generatePermutationsHelper(
    remaining: List<Int>,
    current: MutableList<Int>,
    result: MutableList<List<Int>>
) {
    // Base case: if no more elements to add, we have a complete permutation
    if (remaining.isEmpty()) {
        result.add(current.toList())
        return
    }
    
    // Try each remaining element
    for (i in remaining.indices) {
        val element = remaining[i]
        current.add(element)
        
        // Create new list without the current element
        val newRemaining = remaining.filterIndexed { index, _ -> index != i }
        generatePermutationsHelper(newRemaining, current, result)
        
        // Backtrack
        current.removeAt(current.size - 1)
    }
}

// Alternative implementation using built-in permutations (if available)
fun generatePermutationsAlternative(n: Int): List<List<Int>> {
    val numbers = (1..n).toList()
    val result = mutableListOf<List<Int>>()
    
    // Using recursive approach with Kotlin's built-in functions
    fun permute(current: List<Int>, remaining: List<Int>) {
        if (remaining.isEmpty()) {
            result.add(current)
        } else {
            remaining.forEach { element ->
                val newCurrent = current + element
                val newRemaining = remaining.filter { it != element }
                permute(newCurrent, newRemaining)
            }
        }
    }
    
    permute(emptyList(), numbers)
    return result
}

// More efficient implementation using next permutation algorithm
fun generatePermutationsEfficient(n: Int): List<List<Int>> {
    val numbers = (1..n).toList()
    val result = mutableListOf<List<Int>>()
    
    // Generate all permutations using next permutation approach
    val permutations = mutableListOf<List<Int>>()
    
    // Start with sorted list
    val first = numbers.toList()
    permutations.add(first)
    
    // Generate all permutations using next permutation logic
    val temp = first.toMutableList()
    while (nextPermutation(temp)) {
        permutations.add(temp.toList())
    }
    
    return permutations
}

fun nextPermutation(list: MutableList<Int>): Boolean {
    // Find the largest index k such that list[k] < list[k + 1]
    var k = -1
    for (i in list.size - 2 downTo 0) {
        if (list[i] < list[i + 1]) {
            k = i
            break
        }
    }
    
    if (k == -1) return false  // This is the last permutation
    
    // Find the largest index l such that list[k] < list[l]
    var l = -1
    for (i in list.size - 1 downTo k + 1) {
        if (list[k] < list[i]) {
            l = i
            break
        }
    }
    
    // Swap list[k] and list[l]
    list[k] = list[l].also { list[l] = list[k] }
    
    // Reverse the sequence from k + 1 to the end
    list.sort(k + 1, list.size)
    
    return true
}
```

## Example Usage

For n = 3, the output would be:
```
6
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1
```

## Explanation

1. **Problem Understanding**: We need to generate all possible permutations of numbers from 1 to n and count them.

2. **Approach**: 
   - Use backtracking to generate all permutations
   - For each position, try placing each unused number
   - When all positions are filled, we have a complete permutation
   - Backtrack to try different arrangements

3. **Time Complexity**: O(n! × n) where n! is the number of permutations and n is the time to copy each permutation.

4. **Space Complexity**: O(n! × n) to store all permutations.

The solution handles the constraint that n ≤ 7 efficiently and produces all possible permutations in lexicographic order.

