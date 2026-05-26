# Rosalind Problem: Implement_NumberToPattern

## Problem Description
Implement the NumberToPattern function that converts a number to its corresponding DNA pattern of a given length.

## Solution

```kotlin
fun numberToPattern(number: Long, k: Int): String {
    if (k == 1) {
        return numberToSymbol(number)
    }
    
    val prefixIndex = number / 4
    val remainder = number % 4
    val prefix = numberToPattern(prefixIndex, k - 1)
    val symbol = numberToSymbol(remainder)
    
    return prefix + symbol
}

fun numberToSymbol(number: Long): String {
    return when (number) {
        0L -> "A"
        1L -> "C"
        2L -> "G"
        3L -> "T"
        else -> throw IllegalArgumentException("Invalid number: $number")
    }
}

// Alternative iterative implementation
fun numberToPatternIterative(number: Long, k: Int): String {
    val symbols = arrayOf("A", "C", "G", "T")
    val pattern = StringBuilder()
    
    var num = number
    for (i in 0 until k) {
        pattern.insert(0, symbols[(num % 4).toInt()])
        num /= 4
    }
    
    return pattern.toString()
}

// Example usage
fun main() {
    // Test cases
    println(numberToPattern(45, 4))  // Should output "ACGT"
    println(numberToPattern(0, 1))   // Should output "A"
    println(numberToPattern(15, 2))  // Should output "TT"
    
    // Test iterative version
    println(numberToPatternIterative(45, 4))  // Should output "ACGT"
}
```

## Explanation

The solution implements two approaches to convert a number to a DNA pattern:

### Recursive Approach (`numberToPattern`)
1. **Base case**: If k = 1, directly convert the number to its corresponding DNA symbol
2. **Recursive case**: 
   - Calculate the prefix index by dividing the number by 4
   - Calculate the remainder to determine the last symbol
   - Recursively get the prefix pattern
   - Combine the prefix and the symbol

### Iterative Approach (`numberToPatternIterative`)
1. Uses a loop to build the pattern from right to left
2. For each position, determines the symbol using modulo operation
3. Divides the number by 4 to move to the next position
4. Inserts each symbol at the beginning of the result

## Time and Space Complexity
- **Time Complexity**: O(k) where k is the length of the pattern
- **Space Complexity**: O(k) for the recursive approach (due to call stack), O(1) for iterative approach

## DNA Mapping
- 0 → 'A'
- 1 → 'C' 
- 2 → 'G'
- 3 → 'T'

The algorithm works by treating the DNA pattern as a base-4 number system where each position represents a different nucleotide.

