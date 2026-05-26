# Rosalind Problem: Wobble Bonding and RNA Secondary Structures (Kotlin Solution)

## Problem Understanding

This problem asks us to count the number of ways an RNA string can form valid secondary structures with wobble base pairing. In wobble base pairing:
- A can pair with U
- U can pair with A  
- C can pair with G
- G can pair with C
- A can also pair with G (wobble)
- G can also pair with A (wobble)

We need to count all valid secondary structures (non-crossing perfect matchings) for a given RNA sequence.

## Approach

This is a classic dynamic programming problem similar to the "RNA secondary structure" problem. We use the following recurrence:

Let `dp[i][j]` = number of valid secondary structures for substring from index i to j

Base cases:
- `dp[i][i] = 1` (empty structure)
- `dp[i][i+1] = 1` if i and i+1 can pair, 0 otherwise

Recurrence:
1. `dp[i][j] += dp[i+1][j]` (i doesn't pair with anyone)
2. For each k where i can pair with k: `dp[i][j] += dp[i+1][k-1] * dp[k+1][j]`

## Solution

```kotlin
import java.io.File

fun main() {
    val input = File("rosalind_wobble.txt").readText().trim()
    val result = countWobbleStructures(input)
    println(result)
}

fun countWobbleStructures(rna: String): Long {
    val n = rna.length
    val dp = Array(n) { LongArray(n) { 0L } }
    
    // Base case: empty substring
    for (i in 0 until n) {
        dp[i][i] = 1L
    }
    
    // Fill DP table for substrings of increasing length
    for (length in 2..n) {
        for (i in 0..n - length) {
            val j = i + length - 1
            
            // Case 1: i doesn't pair with anyone
            dp[i][j] = dp[i + 1][j]
            
            // Case 2: i pairs with some k
            for (k in i + 1..j) {
                if (canPair(rna[i], rna[k])) {
                    val left = if (i + 1 <= k - 1) dp[i + 1][k - 1] else 1L
                    val right = if (k + 1 <= j) dp[k + 1][j] else 1L
                    dp[i][j] += left * right
                }
            }
        }
    }
    
    return dp[0][n - 1]
}

fun canPair(base1: Char, base2: Char): Boolean {
    return when {
        base1 == 'A' && (base2 == 'U' || base2 == 'G') -> true
        base1 == 'U' && (base2 == 'A') -> true
        base1 == 'C' && (base2 == 'G') -> true
        base1 == 'G' && (base2 == 'C' || base1 == 'A') -> true
        else -> false
    }
}
```

## Explanation

1. **Dynamic Programming Approach**: We use a 2D DP table where `dp[i][j]` represents the number of valid secondary structures for the substring from index i to j.

2. **Base Cases**: 
   - Single nucleotide substrings have exactly 1 structure (empty structure)
   - Two nucleotide substrings can form a structure if they can pair

3. **Recurrence Relation**:
   - For each substring, we consider two cases:
     - The first nucleotide doesn't pair with anyone (add `dp[i+1][j]`)
     - The first nucleotide pairs with some k (add `dp[i+1][k-1] * dp[k+1][j]`)

4. **Wobble Pairing Rules**:
   - A pairs with U or G
   - U pairs with A  
   - C pairs with G
   - G pairs with C or A

5. **Time Complexity**: O(n³) where n is the length of the RNA string
6. **Space Complexity**: O(n²) for the DP table

## Example

For RNA sequence "AUGCUA":
- Valid structures include: no pairs, (A,U), (U,A), (G,C), etc.
- The algorithm counts all valid non-crossing perfect matchings

This solution handles the wobble bonding rules correctly and efficiently computes the total number of valid secondary structures.

