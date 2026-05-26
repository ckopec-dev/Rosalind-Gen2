# Rosalind Problem: Maximizing the Gap Symbols of an Optimal Alignment (Scala Solution)

## Problem Understanding

This problem asks us to find an optimal global alignment between two strings that maximizes the number of gap symbols (hyphens) in the alignment. This is a variation of the classic sequence alignment problem where we need to modify the scoring function to penalize matches/mismatches less and penalize gaps more heavily.

## Solution Approach

We'll use dynamic programming with a modified scoring scheme:
- Match: score +1 (or 0 if using penalty-based scoring)
- Mismatch: score -1 (or 0 if using penalty-based scoring)  
- Gap: score -2 (or -2 if using penalty-based scoring)

## Scala Implementation

```scala
object MaximizingGapSymbols {
  
  def maxGapAlignment(s1: String, s2: String): Int = {
    val m = s1.length
    val n = s2.length
    
    // Create DP table
    val dp = Array.ofDim[Int](m + 1, n + 1)
    
    // Initialize base cases
    for (i <- 0 to m) dp(i)(0) = -i * 2  // All gaps in first sequence
    for (j <- 0 to n) dp(0)(j) = -j * 2  // All gaps in second sequence
    
    // Fill the DP table
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        val matchScore = if (s1(i - 1) == s2(j - 1)) 1 else -1
        val diagonal = dp(i - 1)(j - 1) + matchScore
        val up = dp(i - 1)(j) - 2  // Gap in second sequence
        val left = dp(i)(j - 1) - 2  // Gap in first sequence
        
        dp(i)(j) = math.max(math.max(diagonal, up), left)
      }
    }
    
    dp(m)(n)
  }
  
  // Function to reconstruct alignment and count gaps
  def reconstructAlignment(s1: String, s2: String): (String, String, Int) = {
    val m = s1.length
    val n = s2.length
    
    // Create DP table
    val dp = Array.ofDim[Int](m + 1, n + 1)
    
    // Initialize base cases
    for (i <- 0 to m) dp(i)(0) = -i * 2
    for (j <- 0 to n) dp(0)(j) = -j * 2
    
    // Fill the DP table
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        val matchScore = if (s1(i - 1) == s2(j - 1)) 1 else -1
        val diagonal = dp(i - 1)(j - 1) + matchScore
        val up = dp(i - 1)(j) - 2
        val left = dp(i)(j - 1) - 2
        
        dp(i)(j) = math.max(math.max(diagonal, up), left)
      }
    }
    
    // Reconstruct alignment
    var i = m
    var j = n
    var align1 = ""
    var align2 = ""
    
    while (i > 0 || j > 0) {
      if (i > 0 && j > 0 && dp(i)(j) == dp(i - 1)(j - 1) + (if (s1(i - 1) == s2(j - 1)) 1 else -1)) {
        align1 = s1(i - 1) + align1
        align2 = s2(j - 1) + align2
        i -= 1
        j -= 1
      } else if (i > 0 && dp(i)(j) == dp(i - 1)(j) - 2) {
        align1 = s1(i - 1) + align1
        align2 = "-" + align2
        i -= 1
      } else {
        align1 = "-" + align1
        align2 = s2(j - 1) + align2
        j -= 1
      }
    }
    
    // Count gaps in alignment
    val gapCount = align1.count(_ == '-') + align2.count(_ == '-')
    
    (align1, align2, gapCount)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val s1 = "ACGTACGT"
    val s2 = "ACGTACGT"
    
    val maxScore = maxGapAlignment(s1, s2)
    println(s"Maximum gap score: $maxScore")
    
    val (alignment1, alignment2, gapCount) = reconstructAlignment(s1, s2)
    println(s"Alignment 1: $alignment1")
    println(s"Alignment 2: $alignment2")
    println(s"Gap count: $gapCount")
  }
}
```

## Key Points

1. **Modified Scoring Scheme**: 
   - Match: +1 (encourages matches)
   - Mismatch: -1 (penalizes mismatches)
   - Gap: -2 (strongly penalizes gaps, encouraging fewer gaps)

2. **Dynamic Programming Approach**:
   - `dp[i][j]` represents the maximum score for aligning `s1[0...i-1]` with `s2[0...j-1]`
   - Base cases: Initialize first row and column with gap penalties
   - Recurrence relation: Choose maximum among diagonal (match/mismatch), up (gap in second), left (gap in first)

3. **Time Complexity**: O(m×n) where m and n are the lengths of the sequences
4. **Space Complexity**: O(m×n) for the DP table

## Sample Input/Output

For input strings "ACGT" and "ACGT":
- The alignment would be optimal with minimal gaps
- For strings with different lengths, gaps are introduced to maximize the gap count in the optimal alignment

This solution handles the core requirement of maximizing gap symbols in an optimal alignment by using a scoring system that heavily penalizes gaps, thus naturally leading to alignments with more gaps when that's the optimal strategy.

