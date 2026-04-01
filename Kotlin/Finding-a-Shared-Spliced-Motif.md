# Finding a Shared Spliced Motif - Rosalind Problem Solution in Kotlin

## Problem Understanding

The problem asks us to find the longest common subsequence (LCS) between two DNA strings. A subsequence is a sequence that can be derived from another sequence by deleting some or no elements without changing the order of the remaining elements.

## Approach

We'll use dynamic programming to solve this problem:
1. Create a 2D DP table where `dp[i][j]` represents the length of LCS for the first `i` characters of string 1 and first `j` characters of string 2
2. Fill the table using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. Backtrack through the table to reconstruct the actual LCS

## Solution

```kotlin
import java.io.File

fun findSharedSplicedMotif(s1: String, s2: String): String {
    val m = s1.length
    val n = s2.length
    
    // Create DP table
    val dp = Array(m + 1) { Array(n + 1) { 0 } }
    
    // Fill the DP table
    for (i in 1..m) {
        for (j in 1..n) {
            if (s1[i - 1] == s2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1
            } else {
                dp[i][j] = maxOf(dp[i - 1][j], dp[i][j - 1])
            }
        }
    }
    
    // Backtrack to find the actual LCS
    val result = StringBuilder()
    var i = m
    var j = n
    
    while (i > 0 && j > 0) {
        if (s1[i - 1] == s2[j - 1]) {
            result.append(s1[i - 1])
            i--
            j--
        } else if (dp[i - 1][j] > dp[i][j - 1]) {
            i--
        } else {
            j--
        }
    }
    
    return result.reverse().toString()
}

fun main() {
    // Read input from file
    val input = File("rosalind_lcsq.txt").readText()
    
    // Parse the input to get two DNA strings
    val lines = input.trim().split("\n")
    val s1 = lines[1]
    val s2 = lines[3]
    
    // Find and print the shared spliced motif
    val result = findSharedSplicedMotif(s1, s2)
    println(result)
}

// Alternative implementation using more concise Kotlin syntax
fun findSharedSplicedMotifConcise(s1: String, s2: String): String {
    val m = s1.length
    val n = s2.length
    
    // Create DP table
    val dp = Array(m + 1) { Array(n + 1) { 0 } }
    
    // Fill the DP table
    for (i in 1..m) {
        for (j in 1..n) {
            dp[i][j] = if (s1[i - 1] == s2[j - 1]) {
                dp[i - 1][j - 1] + 1
            } else {
                maxOf(dp[i - 1][j], dp[i][j - 1])
            }
        }
    }
    
    // Reconstruct the LCS
    val result = StringBuilder()
    var i = m
    var j = n
    
    while (i > 0 && j > 0) {
        when {
            s1[i - 1] == s2[j - 1] -> {
                result.append(s1[i - 1])
                i--
                j--
            }
            dp[i - 1][j] > dp[i][j - 1] -> i--
            else -> j--
        }
    }
    
    return result.reverse().toString()
}
```

## Example Usage

For input:
```
>Rosalind_23
AACCTTGG
>Rosalind_64
ACACTGTGA
```

The algorithm would:
1. Create a DP table of size (8+1) × (9+1) = 9 × 10
2. Fill the table using dynamic programming
3. Backtrack to find the actual LCS: "ACCTGG"

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Key Points

1. The solution uses dynamic programming to efficiently find the longest common subsequence
2. The backtracking step is crucial for reconstructing the actual subsequence, not just its length
3. The algorithm handles edge cases where one or both strings are empty
4. The approach works for any characters, not just DNA nucleotides

This solution efficiently solves the Rosalind problem "Finding a Shared Spliced Motif" by finding the longest common subsequence between two DNA strings.

