# Rosalind Problem: Find a Longest Common Subsequence of Two Strings

## Problem Description
Given two strings, find the length of their longest common subsequence (LCS) and one possible LCS.

## Solution Approach
We'll use dynamic programming to solve this classic problem:
1. Create a 2D DP table where `dp[i][j]` represents the length of LCS for first `i` characters of first string and first `j` characters of second string
2. Fill the table using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. Backtrack through the table to reconstruct one possible LCS

## Kotlin Implementation

```kotlin
fun longestCommonSubsequence(s1: String, s2: String): String {
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
    
    // Backtrack to find one LCS
    val lcs = StringBuilder()
    var i = m
    var j = n
    
    while (i > 0 && j > 0) {
        if (s1[i - 1] == s2[j - 1]) {
            lcs.append(s1[i - 1])
            i--
            j--
        } else if (dp[i - 1][j] > dp[i][j - 1]) {
            i--
        } else {
            j--
        }
    }
    
    return lcs.reverse().toString()
}

fun main() {
    // Example usage
    val s1 = "AACCTTGG"
    val s2 = "ACACTGTGA"
    
    val result = longestCommonSubsequence(s1, s2)
    println("Length of LCS: ${result.length}")
    println("LCS: $result")
    
    // For the actual Rosalind problem, you would read from input
    // val input = readLine()!!.trim()
    // val s1 = readLine()!!.trim()
    // val s2 = readLine()!!.trim()
    // println(longestCommonSubsequence(s1, s2))
}
```

## Explanation

### Step 1: DP Table Construction
We create a table `dp` of size `(m+1) × (n+1)` where:
- `dp[i][j]` = length of LCS for `s1[0..i-1]` and `s2[0..j-1]`
- Base case: `dp[0][j] = 0` and `dp[i][0] = 0` (empty string LCS)

### Step 2: Fill DP Table
For each cell `dp[i][j]`:
- If `s1[i-1] == s2[j-1]`: Characters match, so `dp[i][j] = dp[i-1][j-1] + 1`
- Else: Take maximum from left or top cell, so `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

### Step 3: Backtracking
Starting from `dp[m][n]`, we trace back to reconstruct the actual LCS:
- If characters match, include in result and move diagonally
- Otherwise, move toward the cell with larger value

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Input/Output
```
Input:
AACCTTGG
ACACTGTGA

Output:
ACCTGG
```

This solution efficiently finds one possible longest common subsequence of two given strings.

