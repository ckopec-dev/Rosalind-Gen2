# Rosalind Problem: Implement PatternToNumber

## Problem Description
The job of PatternToNumber is to convert a DNA pattern (string of nucleotides) into its corresponding lexicographically ordered index in the list of all possible k-mers sorted lexicographically.

## Solution Approach
This is essentially converting a base-4 number to decimal, where:
- A = 0, C = 1, G = 2, T = 3
- Each position contributes to the final number based on its position (from right to left)

## Kotlin Implementation

```kotlin
fun patternToNumber(pattern: String): Long {
    if (pattern.isEmpty()) return 0L
    
    val symbolToNumber = mapOf('A' to 0L, 'C' to 1L, 'G' to 2L, 'T' to 3L)
    var number = 0L
    
    // Process from right to left (least significant to most significant)
    for (i in pattern.length - 1 downTo 0) {
        val symbol = pattern[i]
        val symbolValue = symbolToNumber[symbol]!!
        number += symbolValue * Math.pow(4.0, (pattern.length - 1 - i).toDouble()).toLong()
    }
    
    return number
}

// Alternative more concise implementation
fun patternToNumberConcise(pattern: String): Long {
    if (pattern.isEmpty()) return 0L
    
    val symbolToNumber = mapOf('A' to 0L, 'C' to 1L, 'G' to 2L, 'T' to 3L)
    var number = 0L
    
    for (symbol in pattern) {
        number = number * 4 + symbolToNumber[symbol]!!
    }
    
    return number
}

// Test function
fun main() {
    // Test cases
    println(patternToNumber("ACGT")) // Expected: 27
    println(patternToNumber("AA"))   // Expected: 0
    println(patternToNumber("T"))    // Expected: 3
    println(patternToNumber("AG"))   // Expected: 4
    
    // Test concise version
    println(patternToNumberConcise("ACGT")) // Expected: 27
    println(patternToNumberConcise("AA"))   // Expected: 0
    println(patternToNumberConcise("T"))    // Expected: 3
    println(patternToNumberConcise("AG"))   // Expected: 4
}
```

## Explanation

The algorithm works by treating each DNA pattern as a base-4 number:

1. **Mapping**: Each nucleotide is mapped to a digit (A=0, C=1, G=2, T=3)
2. **Base conversion**: Starting from the leftmost character, we multiply the accumulated result by 4 and add the current digit's value
3. **Result**: This gives us the lexicographically ordered index

For example, with pattern "ACGT":
- A (0) → 0
- C (1) → 0×4 + 1 = 1  
- G (2) → 1×4 + 2 = 6
- T (3) → 6×4 + 3 = 27

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the pattern
- **Space Complexity**: O(1) - only using a constant amount of extra space

The concise version is more efficient and follows the standard algorithm for converting from a base-4 representation to decimal.

