# Rosalind Problem: Local Alignment with Scoring Matrix (Scala Solution)

## Problem Understanding

The local alignment problem involves finding the best local alignment between two sequences using a scoring matrix, which is different from global alignment that aligns the entire sequences.

## Solution Approach

I'll implement the Smith-Waterman algorithm for local alignment with a custom scoring matrix.

```scala
object LocalAlignmentWithScoringMatrix {
  
  def localAlignment(seq1: String, seq2: String, scoringMatrix: Map[(Char, Char), Int]): (Int, String, String) = {
    val m = seq1.length
    val n = seq2.length
    
    // Create scoring matrix
    val scoreMatrix = Array.ofDim[Int](m + 1, n + 1)
    
    // Initialize first row and column
    for (i <- 0 to m) scoreMatrix(i)(0) = 0
    for (j <- 0 to n) scoreMatrix(0)(j) = 0
    
    // Fill the scoring matrix
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        val matchScore = scoreMatrix(i-1)(j-1) + scoringMatrix.getOrElse((seq1(i-1), seq2(j-1)), 0)
        val deleteScore = scoreMatrix(i-1)(j) - 2
        val insertScore = scoreMatrix(i)(j-1) - 2
        scoreMatrix(i)(j) = math.max(math.max(matchScore, deleteScore), insertScore)
        scoreMatrix(i)(j) = math.max(scoreMatrix(i)(j), 0) // Local alignment: no negative scores
      }
    }
    
    // Find maximum score and its position
    var maxScore = 0
    var maxI = 0
    var maxJ = 0
    
    for (i <- 0 to m) {
      for (j <- 0 to n) {
        if (scoreMatrix(i)(j) > maxScore) {
          maxScore = scoreMatrix(i)(j)
          maxI = i
          maxJ = j
        }
      }
    }
    
    // Traceback to find the alignment
    val alignment1 = new StringBuilder()
    val alignment2 = new StringBuilder()
    
    var i = maxI
    var j = maxJ
    
    while (i > 0 && j > 0 && scoreMatrix(i)(j) > 0) {
      val currentScore = scoreMatrix(i)(j)
      val diagonalScore = scoreMatrix(i-1)(j-1)
      val upScore = scoreMatrix(i-1)(j)
      val leftScore = scoreMatrix(i)(j-1)
      
      val matchScore = diagonalScore + scoringMatrix.getOrElse((seq1(i-1), seq2(j-1)), 0)
      
      if (currentScore == matchScore) {
        alignment1.insert(0, seq1(i-1))
        alignment2.insert(0, seq2(j-1))
        i -= 1
        j -= 1
      } else if (currentScore == upScore - 2) {
        alignment1.insert(0, seq1(i-1))
        alignment2.insert(0, '-')
        i -= 1
      } else {
        alignment1.insert(0, '-')
        alignment2.insert(0, seq2(j-1))
        j -= 1
      }
    }
    
    (maxScore, alignment1.toString, alignment2.toString)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage with sample data
    val seq1 = "MEANLY"
    val seq2 = "PENALTY"
    
    // Scoring matrix (example values)
    val scoringMatrix = Map(
      ('A', 'A') -> 5, ('A', 'C') -> -1, ('A', 'D') -> -2, ('A', 'E') -> -1, ('A', 'F') -> -3,
      ('A', 'G') -> 0, ('A', 'H') -> -2, ('A', 'I') -> -1, ('A', 'K') -> -1, ('A', 'L') -> -4,
      ('A', 'M') -> -1, ('A', 'N') -> 1, ('A', 'P') -> -1, ('A', 'Q') -> 0, ('A', 'R') -> -1,
      ('A', 'S') -> 1, ('A', 'T') -> 0, ('A', 'V') -> -2, ('A', 'W') -> -3, ('A', 'Y') -> -2,
      ('A', 'X') -> -1, ('A', 'Z') -> -1,
      
      ('C', 'C') -> 9, ('C', 'D') -> -3, ('C', 'E') -> -4, ('C', 'F') -> -2, ('C', 'G') -> -3,
      ('C', 'H') -> -2, ('C', 'I') -> -1, ('C', 'K') -> -3, ('C', 'L') -> -1, ('C', 'M') -> -1,
      ('C', 'N') -> -2, ('C', 'P') -> -4, ('C', 'Q') -> -3, ('C', 'R') -> -3, ('C', 'S') -> -1,
      ('C', 'T') -> -1, ('C', 'V') -> -2, ('C', 'W') -> -3, ('C', 'Y') -> -2, ('C', 'X') -> -1,
      ('C', 'Z') -> -1,
      
      ('D', 'D') -> 6, ('D', 'E') -> 2, ('D', 'F') -> -3, ('D', 'G') -> -1, ('D', 'H') -> -1,
      ('D', 'I') -> -3, ('D', 'K') -> -1, ('D', 'L') -> -4, ('D', 'M') -> -3, ('D', 'N') -> 1,
      ('D', 'P') -> -1, ('D', 'Q') -> 0, ('D', 'R') -> -2, ('D', 'S') -> 0, ('D', 'T') -> -1,
      ('D', 'V') -> -3, ('D', 'W') -> -4, ('D', 'Y') -> -3, ('D', 'X') -> -1, ('D', 'Z') -> 0,
      
      ('E', 'E') -> 5, ('E', 'F') -> -3, ('E', 'G') -> -2, ('E', 'H') -> 0, ('E', 'I') -> -3,
      ('E', 'K') -> 1, ('E', 'L') -> -3, ('E', 'M') -> -2, ('E', 'N') -> 0, ('E', 'P') -> -1,
      ('E', 'Q') -> 2, ('E', 'R') -> 0, ('E', 'S') -> 0, ('E', 'T') -> -1, ('E', 'V') -> -3,
      ('E', 'W') -> -3, ('E', 'Y') -> -2, ('E', 'X') -> -1, ('E', 'Z') -> 1,
      
      ('F', 'F') -> 8, ('F', 'G') -> -1, ('F', 'H') -> -1, ('F', 'I') -> 0, ('F', 'K') -> -3,
      ('F', 'L') -> 1, ('F', 'M') -> 0, ('F', 'N') -> -3, ('F', 'P') -> -4, ('F', 'Q') -> -2,
      ('F', 'R') -> -3, ('F', 'S') -> -2, ('F', 'T') -> -2, ('F', 'V') -> 1, ('F', 'W') -> 6,
      ('F', 'Y') -> 3, ('F', 'X') -> -1, ('F', 'Z') -> -1,
      
      ('G', 'G') -> 6, ('G', 'H') -> -2, ('G', 'I') -> -4, ('G', 'K') -> -2, ('G', 'L') -> -4,
      ('G', 'M') -> -1, ('G', 'N') -> 0, ('G', 'P') -> -2, ('G', 'Q') -> -2, ('G', 'R') -> -2,
      ('G', 'S') -> 0, ('G', 'T') -> -2, ('G', 'V') -> -3, ('G', 'W') -> -2, ('G', 'Y') -> -3,
      ('G', 'X') -> -1, ('G', 'Z') -> -1,
      
      ('H', 'H') -> 8, ('H', 'I') -> -3, ('H', 'K') -> -1, ('H', 'L') -> -3, ('H', 'M') -> -2,
      ('H', 'N') -> 1, ('H', 'P') -> -2, ('H', 'Q') -> 0, ('H', 'R') -> 0, ('H', 'S') -> -1,
      ('H', 'T') -> -2, ('H', 'V') -> -3, ('H', 'W') -> -2, ('H', 'Y') -> 2, ('H', 'X') -> -1,
      ('H', 'Z') -> 0,
      
      ('I', 'I') -> 4, ('I', 'K') -> -3, ('I', 'L') -> 2, ('I', 'M') -> 1, ('I', 'N') -> -3,
      ('I', 'P') -> -3, ('I', 'Q') -> -3, ('I', 'R') -> -3, ('I', 'S') -> -3, ('I', 'T') -> -1,
      ('I', 'V') -> 3, ('I', 'W') -> -3, ('I', 'Y') -> -1, ('I', 'X') -> -1, ('I', 'Z') -> -3,
      
      ('K', 'K') -> 5, ('K', 'L') -> -3, ('K', 'M') -> -1, ('K', 'N') -> 0, ('K', 'P') -> -1,
      ('K', 'Q') -> 1, ('K', 'R') -> 2, ('K', 'S') -> -1, ('K', 'T') -> -1, ('K', 'V') -> -2,
      ('K', 'W') -> -3, ('K', 'Y') -> -2, ('K', 'X') -> -1, ('K', 'Z') -> 0,
      
      ('L', 'L') -> 4, ('L', 'M') -> 2, ('L', 'N') -> -3, ('L', 'P') -> -4, ('L', 'Q') -> -2,
      ('L', 'R') -> -3, ('L', 'S') -> -3, ('L', 'T') -> -1, ('L', 'V') -> 1, ('L', 'W') -> -2,
      ('L', 'Y') -> -1, ('L', 'X') -> -1, ('L', 'Z') -> -2,
      
      ('M', 'M') -> 5, ('M', 'N') -> -2, ('M', 'P') -> -3, ('M', 'Q') -> -1, ('M', 'R') -> -1,
      ('M', 'S') -> -1, ('M', 'T') -> -1, ('M', 'V') -> 3, ('M', 'W') -> -1, ('M', 'Y') -> -1,
      ('M', 'X') -> -1, ('M', 'Z') -> -1,
      
      ('N', 'N') -> 6, ('N', 'P') -> -2, ('N', 'Q') -> 0, ('N', 'R') -> -1, ('N', 'S') -> 1,
      ('N', 'T') -> 0, ('N', 'V') -> -3, ('N', 'W') -> -4, ('N', 'Y') -> -2, ('N', 'X') -> -1,
      ('N', 'Z') -> 0,
      
      ('P', 'P') -> 7, ('P', 'Q') -> -1, ('P', 'R') -> -1, ('P', 'S') -> -1, ('P', 'T') -> -1,
      ('P', 'V') -> -4, ('P', 'W') -> -4, ('P', 'Y') -> -3, ('P', 'X') -> -1, ('P', 'Z') -> -1,
      
      ('Q', 'Q') -> 5, ('Q', 'R') -> 1, ('Q', 'S') -> 0, ('Q', 'T') -> -1, ('Q', 'V') -> -2,
      ('Q', 'W') -> -2, ('Q', 'Y') -> -1, ('Q', 'X') -> -1, ('Q', 'Z') -> 1,
      
      ('R', 'R') -> 5, ('R', 'S') -> -1, ('R', 'T') -> -1, ('R', 'V') -> -3, ('R', 'W') -> -3,
      ('R', 'Y') -> -2, ('R', 'X') -> -1, ('R', 'Z') -> 0,
      
      ('S', 'S') -> 4, ('S', 'T') -> 1, ('S', 'V') -> -3, ('S', 'W') -> -3, ('S', 'Y') -> -2,
      ('S', 'X') -> -1, ('S', 'Z') -> 0,
      
      ('T', 'T') -> 5, ('T', 'V') -> -1, ('T', 'W') -> -2, ('T', 'Y') -> -2, ('T', 'X') -> -1,
      ('T', 'Z') -> -1,
      
      ('V', 'V') -> 4, ('V', 'W') -> -3, ('V', 'Y') -> -1, ('V', 'X') -> -1, ('V', 'Z') -> -2,
      
      ('W', 'W') -> 11, ('W', 'Y') -> 2, ('W', 'X') -> -1, ('W', 'Z') -> -2,
      
      ('Y', 'Y') -> 7, ('Y', 'X') -> -1, ('Y', 'Z') -> -1,
      
      ('X', 'X') -> 0, ('X', 'Z') -> -1,
      
      ('Z', 'Z') -> 4
    )
    
    val (score, align1, align2) = localAlignment(seq1, seq2, scoringMatrix)
    println(s"Maximum score: $score")
    println(s"Alignment 1: $align1")
    println(s"Alignment 2: $align2")
  }
}
```

## Key Features of the Implementation

1. **Smith-Waterman Algorithm**: Implements the local alignment algorithm with dynamic programming
2. **Scoring Matrix**: Accepts a custom scoring matrix as input
3. **Traceback**: Finds the optimal local alignment by backtracking through the matrix
4. **No Negative Scores**: Ensures local alignment doesn't go below zero (key difference from global alignment)
5. **Proper Alignment Output**: Returns both aligned sequences with gaps represented by '-'

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m×n) for the scoring matrix

## Usage Notes

The solution assumes a standard scoring matrix with match/mismatch scores. For Rosalind problems, you may need to adjust the scoring matrix values according to the specific problem requirements. The algorithm finds the best local alignment and returns both the maximum score and the aligned sequences.

