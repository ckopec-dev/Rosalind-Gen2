# Rosalind Problem: Transitions and Transversions

## Problem Description
Given two DNA strings s and t of equal length, return the transition/transversion ratio.

## Solution in Kotlin

```kotlin
fun transitionsAndTransversions(s: String, t: String): Double {
    var transitions = 0
    var transversions = 0
    
    for (i in s.indices) {
        val base1 = s[i]
        val base2 = t[i]
        
        // Skip if either base is 'N' or ambiguous
        if (base1 == 'N' || base2 == 'N') continue
        
        // Check if bases are different
        if (base1 != base2) {
            // Check for transition (purine ↔ purine or pyrimidine ↔ pyrimidine)
            when {
                (base1 == 'A' && base2 == 'G') || (base1 == 'G' && base2 == 'A') -> transitions++
                (base1 == 'C' && base2 == 'T') || (base1 == 'T' && base2 == 'C') -> transitions++
                // Check for transversion (purine ↔ pyrimidine)
                else -> transversions++
            }
        }
    }
    
    return if (transversions == 0.0) 0.0 else transitions / transversions
}

// Alternative implementation with more explicit transition checking
fun transitionsAndTransversionsAlt(s: String, t: String): Double {
    var transitions = 0
    var transversions = 0
    
    val purines = setOf('A', 'G')
    val pyrimidines = setOf('C', 'T')
    
    for (i in s.indices) {
        val base1 = s[i]
        val base2 = t[i]
        
        if (base1 == 'N' || base2 == 'N') continue
        
        if (base1 != base2) {
            // Check if it's a transition
            val isTransition = when {
                (base1 in purines && base2 in purines) -> true
                (base1 in pyrimidines && base2 in pyrimidines) -> true
                else -> false
            }
            
            if (isTransition) {
                transitions++
            } else {
                transversions++
            }
        }
    }
    
    return if (transversions == 0.0) 0.0 else transitions.toDouble() / transversions.toDouble()
}

// Example usage
fun main() {
    val s = "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGAAGTACGGGCATCAACCCAGTT"
    val t = "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGAAGTACGGGCATCAACCCAGTT"
    
    val ratio = transitionsAndTransversions(s, t)
    println("Transition/Transversion ratio: $ratio")
    
    // Test with example from Rosalind
    val s2 = "AAAACCCGGT"
    val t2 = "AAAACCCGGT"
    val ratio2 = transitionsAndTransversions(s2, t2)
    println("Example ratio: $ratio2")
}
```

## Explanation

The solution works by:

1. **Iterating through each position** in both DNA strings
2. **Skipping ambiguous bases** ('N') that can't be classified
3. **Identifying transitions vs transversions**:
   - **Transitions**: Purine ↔ Purine (A ↔ G) or Pyrimidine ↔ Pyrimidine (C ↔ T)
   - **Transversions**: Purine ↔ Pyrimidine (A ↔ C, A ↔ T, G ↔ C, G ↔ T)
4. **Calculating the ratio** of transitions to transversions

## Key Points

- **Transitions** are more common than transversions in DNA sequences
- The ratio is typically > 1.0 for most DNA sequences
- We handle edge cases where transversions might be 0 to avoid division by zero
- The solution assumes input strings are of equal length

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the DNA strings
- **Space Complexity**: O(1) - only using a constant amount of extra space

This implementation correctly handles the Rosalind problem requirements and can be used to solve the specific test cases given in the Rosalind platform.

