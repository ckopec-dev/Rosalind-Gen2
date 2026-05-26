# Rosalind Problem: Find a Longest Common Subsequence of Two Strings

## Problem Statement
Given two strings s and t, find a longest common subsequence of s and t.

## Solution in Scala

```scala
object LCS {
  def longestCommonSubsequence(s: String, t: String): String = {
    val m = s.length
    val n = t.length
    
    // Create a 2D DP table
    val dp = Array.ofDim[Int](m + 1, n + 1)
    
    // Fill the DP table
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (s(i - 1) == t(j - 1)) {
          dp(i)(j) = dp(i - 1)(j - 1) + 1
        } else {
          dp(i)(j) = math.max(dp(i - 1)(j), dp(i)(j - 1))
        }
      }
    }
    
    // Reconstruct the LCS
    val lcs = new StringBuilder()
    var i = m
    var j = n
    
    while (i > 0 && j > 0) {
      if (s(i - 1) == t(j - 1)) {
        lcs.append(s(i - 1))
        i -= 1
        j -= 1
      } else if (dp(i - 1)(j) > dp(i)(j - 1)) {
        i -= 1
      } else {
        j -= 1
      }
    }
    
    lcs.reverse.toString
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val s = "AACCTTGG"
    val t = "ACACTGTGA"
    val result = longestCommonSubsequence(s, t)
    println(result)
  }
}
```

## Explanation

### Approach
This solution uses dynamic programming to find the longest common subsequence (LCS):

1. **DP Table Construction**: Create a 2D table `dp` where `dp(i)(j)` represents the length of LCS for the first `i` characters of string `s` and first `j` characters of string `t`.

2. **Fill DP Table**: 
   - If characters match: `dp(i)(j) = dp(i-1)(j-1) + 1`
   - If characters don't match: `dp(i)(j) = max(dp(i-1)(j), dp(i)(j-1))`

3. **Reconstruction**: Backtrack through the DP table to construct the actual LCS string.

### Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

### Example
For strings `s = "AACCTTGG"` and `t = "ACACTGTGA"`:
- The LCS is `"AACCCTGA"` or similar valid subsequence
- The algorithm finds the maximum length subsequence that appears in both strings in the same relative order

### Key Points
- The solution handles edge cases where one or both strings are empty
- The reconstruction process traces back through the DP table to build the actual subsequence
- The algorithm correctly implements the classic LCS dynamic programming approach

