# Rosalind Problem: Interleaving Two Motifs (INTER)

## Problem Description
Given two DNA strings s and t, find the number of ways to interleave the strings to form a given string u, where each character in u comes from either s or t in order.

## Solution Approach
We'll use dynamic programming to count the number of valid interleavings. The idea is to build a 2D DP table where `dp[i][j]` represents the number of ways to form the first `i+j` characters of the target string using the first `i` characters of string `s` and first `j` characters of string `t`.

## Scala Implementation

```scala
object INTER {
  def countInterleavings(s: String, t: String, u: String): Long = {
    val m = s.length
    val n = t.length
    val p = u.length
    
    // Check if lengths are compatible
    if (m + n != p) return 0
    
    // Create DP table
    val dp = Array.ofDim[Long](m + 1, n + 1)
    
    // Base case: empty strings
    dp(0)(0) = 1
    
    // Fill the DP table
    for (i <- 0 to m) {
      for (j <- 0 to n) {
        if (i == 0 && j == 0) {
          // Already initialized
        } else if (i == 0) {
          // Only t contributes
          if (t(j - 1) == u(j - 1)) {
            dp(i)(j) = dp(i)(j - 1)
          }
        } else if (j == 0) {
          // Only s contributes
          if (s(i - 1) == u(i - 1)) {
            dp(i)(j) = dp(i - 1)(j)
          }
        } else {
          // Both s and t can contribute
          var ways = 0L
          
          // If s[i-1] matches u[i+j-1], we can take from s
          if (s(i - 1) == u(i + j - 1)) {
            ways += dp(i - 1)(j)
          }
          
          // If t[j-1] matches u[i+j-1], we can take from t
          if (t(j - 1) == u(i + j - 1)) {
            ways += dp(i)(j - 1)
          }
          
          dp(i)(j) = ways
        }
      }
    }
    
    dp(m)(n)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val s = "ABCD"
    val t = "EFGH"
    val u = "AEBFCGDH"
    
    val result = countInterleavings(s, t, u)
    println(s"Number of interleavings: $result")
    
    // Read from file (if needed)
    // val lines = scala.io.Source.fromFile("rosalind_inter.txt").getLines().toList
    // val s = lines(0)
    // val t = lines(1)
    // val u = lines(2)
    // println(countInterleavings(s, t, u))
  }
}
```

## Alternative Implementation (More Concise)

```scala
object INTER {
  def countInterleavings(s: String, t: String, u: String): Long = {
    val m = s.length
    val n = t.length
    val p = u.length
    
    // Check if lengths are compatible
    if (m + n != p) return 0
    
    // Create DP table
    val dp = Array.ofDim[Long](m + 1, n + 1)
    dp(0)(0) = 1
    
    // Fill DP table
    for (i <- 0 to m; j <- 0 to n) {
      if (!(i == 0 && j == 0)) {
        val fromS = if (i > 0 && s(i - 1) == u(i + j - 1)) dp(i - 1)(j) else 0L
        val fromT = if (j > 0 && t(j - 1) == u(i + j - 1)) dp(i)(j - 1) else 0L
        dp(i)(j) = fromS + fromT
      }
    }
    
    dp(m)(n)
  }
}
```

## Example Usage

```scala
// Test case
val s = "ACGT"
val t = "ACGT"
val u = "AACGTTCCGG"

val result = INTER.countInterleavings(s, t, u)
println(s"Number of interleavings: $result")
```

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of strings s and t
- **Space Complexity**: O(m × n) for the DP table

## Key Points

1. **Base Case**: `dp[0][0] = 1` (one way to form empty string)
2. **Transition**: For each cell `dp[i][j]`, we can come from:
   - `dp[i-1][j]` if `s[i-1] == u[i+j-1]` (take from s)
   - `dp[i][j-1]` if `t[j-1] == u[i+j-1]` (take from t)
3. **Edge Cases**: Handle when one string is empty
4. **Validation**: Check that total length matches `|s| + |t|`

This solution efficiently counts all possible interleavings using dynamic programming principles, making it suitable for the constraints of Rosalind problems.

