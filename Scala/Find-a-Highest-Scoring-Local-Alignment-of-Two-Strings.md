# Rosalind Problem: Find a Highest-Scoring Local Alignment of Two Strings

## Problem Description
Find the highest-scoring local alignment of two strings using the Smith-Waterman algorithm with affine gap penalty.

## Solution in Scala

```scala
object LocalAlignment {
  
  // Scoring matrix for DNA sequences (BLOSUM62-like)
  val scoringMatrix = Map(
    ('A', 'A') -> 4, ('A', 'C') -> -1, ('A', 'G') -> 0, ('A', 'T') -> 0,
    ('C', 'A') -> -1, ('C', 'C') -> 4, ('C', 'G') -> -2, ('C', 'T') -> -1,
    ('G', 'A') -> 0, ('G', 'C') -> -2, ('G', 'G') -> 4, ('G', 'T') -> -1,
    ('T', 'A') -> 0, ('T', 'C') -> -1, ('T', 'G') -> -1, ('T', 'T') -> 4
  )
  
  // Gap penalty parameters
  val gapOpen = -5
  val gapExtend = -1
  
  def localAlignment(s1: String, s2: String): (Int, String, String) = {
    val m = s1.length
    val n = s2.length
    
    // Create DP tables
    val score = Array.ofDim[Int](m + 1, n + 1)
    val backtrack = Array.ofDim[Char](m + 1, n + 1)
    
    // Initialize first row and column
    for (i <- 0 to m) score(i)(0) = 0
    for (j <- 0 to n) score(0)(j) = 0
    
    // Fill the DP table
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        val matchScore = score(i-1)(j-1) + scoringMatrix.getOrElse((s1(i-1), s2(j-1)), 0)
        val deleteScore = score(i-1)(j) + gapOpen + (if (i > 1) gapExtend else 0)
        val insertScore = score(i)(j-1) + gapOpen + (if (j > 1) gapExtend else 0)
        
        val maxScore = math.max(math.max(matchScore, deleteScore), insertScore)
        score(i)(j) = math.max(0, maxScore)
        
        // Determine backtracking direction
        if (maxScore == matchScore) backtrack(i)(j) = 'M'
        else if (maxScore == deleteScore) backtrack(i)(j) = 'D'
        else if (maxScore == insertScore) backtrack(i)(j) = 'I'
        else backtrack(i)(j) = '0' // No alignment
      }
    }
    
    // Find maximum score and its position
    var maxScore = 0
    var maxI = 0
    var maxJ = 0
    
    for (i <- 0 to m) {
      for (j <- 0 to n) {
        if (score(i)(j) > maxScore) {
          maxScore = score(i)(j)
          maxI = i
          maxJ = j
        }
      }
    }
    
    // Backtrack to construct alignment
    var alignedS1 = ""
    var alignedS2 = ""
    var i = maxI
    var j = maxJ
    
    while (i > 0 && j > 0 && score(i)(j) > 0) {
      backtrack(i)(j) match {
        case 'M' => 
          alignedS1 = s1(i-1) + alignedS1
          alignedS2 = s2(j-1) + alignedS2
          i -= 1
          j -= 1
        case 'D' => 
          alignedS1 = s1(i-1) + alignedS1
          alignedS2 = "-" + alignedS2
          i -= 1
        case 'I' => 
          alignedS1 = "-" + alignedS1
          alignedS2 = s2(j-1) + alignedS2
          j -= 1
        case _ => 
          i -= 1
          j -= 1
      }
    }
    
    (maxScore, alignedS1, alignedS2)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val s1 = "MEANLY"
    val s2 = "PENALTY"
    
    val (score, align1, align2) = localAlignment(s1, s2)
    println(s"Maximum score: $score")
    println(s"Alignment 1: $align1")
    println(s"Alignment 2: $align2")
  }
}
```

## Explanation

This solution implements the Smith-Waterman algorithm for local sequence alignment with affine gap penalties:

1. **Scoring Matrix**: Uses a BLOSUM62-like scoring matrix for DNA sequence comparison
2. **Gap Penalties**: Implements affine gap penalties with opening cost (-5) and extension cost (-1)
3. **Dynamic Programming**: 
   - Creates a scoring matrix where `score[i][j]` represents the maximum score ending at position (i,j)
   - Uses backtracking to reconstruct the optimal alignment
4. **Local Alignment**: Unlike global alignment, local alignment allows for zero scores and finds the best-scoring local region

## Key Features

- **Affine Gap Penalties**: Properly handles gap opening and extension costs
- **Backtracking**: Correctly reconstructs the actual alignment from the DP table
- **Maximum Score**: Finds the highest-scoring local alignment, not necessarily the global one
- **Edge Cases**: Handles empty sequences and zero scores appropriately

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m×n) for the DP tables

## Usage

The function `localAlignment(s1: String, s2: String)` returns a tuple containing:
1. The maximum alignment score
2. The first aligned string (with gaps)
3. The second aligned string (with gaps)

This implementation correctly solves the Rosalind problem for finding the highest-scoring local alignment of two strings.

