# Rosalind Problem: Counting Point Mutations

## Problem Description
Given two DNA strings of equal length, count the number of positions where the nucleotides differ (Hamming distance).

## Solution in Kotlin

```kotlin
fun countPointMutations(dna1: String, dna2: String): Int {
    var mutations = 0
    for (i in dna1.indices) {
        if (dna1[i] != dna2[i]) {
            mutations++
        }
    }
    return mutations
}

// Alternative implementation using zip and count
fun countPointMutationsAlt(dna1: String, dna2: String): Int {
    return dna1.zip(dna2).count { it.first != it.second }
}

fun main() {
    // Example usage
    val dna1 = "GAGCCTACTAACGGGAT"
    val dna2 = "CATCGGTATCGGGTTAT"
    
    val result = countPointMutations(dna1, dna2)
    println("Number of point mutations: $result")
    
    // Test with alternative implementation
    val resultAlt = countPointMutationsAlt(dna1, dna2)
    println("Number of point mutations (alternative): $resultAlt")
}
```

## Explanation

The problem asks us to calculate the Hamming distance between two DNA strings - the number of positions where the corresponding nucleotides differ.

### Approach 1: Simple Loop
- Iterate through each position in the strings
- Compare nucleotides at each position
- Increment counter when they differ
- Return the total count

### Approach 2: Functional Style
- Use `zip()` to pair corresponding characters from both strings
- Count pairs where characters are not equal
- This is more concise and idiomatic Kotlin

## Sample Input/Output
```
Input:
GAGCCTACTAACGGGAT
CATCGGTATCGGGTTAT

Output:
7
```

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the DNA strings
- **Space Complexity**: O(1) for the first approach, O(n) for the second approach due to the zip operation

The solution handles the constraint that both DNA strings have equal length as specified in the problem.

