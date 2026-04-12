# Rosalind Problem: Semiglobal Alignment in Kotlin

## Problem Understanding

Semiglobal alignment is a variant of sequence alignment where we allow gaps at the beginning and end of sequences, but not in the middle. This is useful for finding local similarities between sequences where we don't care about the alignment of the flanking regions.

## Solution Approach

I'll implement the semiglobal alignment algorithm using dynamic programming, similar to the global alignment but with modified boundary conditions.

```kotlin
import java.io.File

fun semiglobalAlignment(seq1: String, seq2: String, matchScore: Int = 2, mismatchScore: Int = -1, gapPenalty: Int = -1): Pair<String, String> {
    val n = seq1.length
    val m = seq2.length
    
    // Create DP table
    val dp = Array(n + 1) { Array(m + 1) { 0 } }
    
    // Initialize first row and column (but not the first cell)
    for (i in 1..n) {
        dp[i][0] = dp[i-1][0] + gapPenalty
    }
    
    for (j in 1..m) {
        dp[0][j] = dp[0][j-1] + gapPenalty
    }
    
    // Fill the DP table
    for (i in 1..n) {
        for (j in 1..m) {
            val match = if (seq1[i-1] == seq2[j-1]) matchScore else mismatchScore
            dp[i][j] = maxOf(
                dp[i-1][j] + gapPenalty,      // deletion
                dp[i][j-1] + gapPenalty,      // insertion
                dp[i-1][j-1] + match          // match/mismatch
            )
        }
    }
    
    // Traceback to construct alignment
    val alignment1 = StringBuilder()
    val alignment2 = StringBuilder()
    
    var i = n
    var j = m
    
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && dp[i][j] == dp[i-1][j-1] + (if (seq1[i-1] == seq2[j-1]) matchScore else mismatchScore)) {
            // Match/mismatch
            alignment1.insert(0, seq1[i-1])
            alignment2.insert(0, seq2[j-1])
            i--
            j--
        } else if (i > 0 && dp[i][j] == dp[i-1][j] + gapPenalty) {
            // Deletion
            alignment1.insert(0, seq1[i-1])
            alignment2.insert(0, '-')
            i--
        } else {
            // Insertion
            alignment1.insert(0, '-')
            alignment2.insert(0, seq2[j-1])
            j--
        }
    }
    
    return Pair(alignment1.toString(), alignment2.toString())
}

fun semiglobalAlignmentOptimized(seq1: String, seq2: String): Pair<String, String> {
    val n = seq1.length
    val m = seq2.length
    
    // Create DP table
    val dp = Array(n + 1) { Array(m + 1) { 0 } }
    
    // Initialize first row and column
    for (i in 0..n) {
        dp[i][0] = i * -1  // Allow gaps at beginning
    }
    
    for (j in 0..m) {
        dp[0][j] = j * -1  // Allow gaps at beginning
    }
    
    // Fill the DP table
    for (i in 1..n) {
        for (j in 1..m) {
            val match = if (seq1[i-1] == seq2[j-1]) 2 else -1
            dp[i][j] = maxOf(
                dp[i-1][j] - 1,      // deletion
                dp[i][j-1] - 1,      // insertion
                dp[i-1][j-1] + match // match/mismatch
            )
        }
    }
    
    // Traceback to construct alignment
    val alignment1 = StringBuilder()
    val alignment2 = StringBuilder()
    
    var i = n
    var j = m
    
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && dp[i][j] == dp[i-1][j-1] + (if (seq1[i-1] == seq2[j-1]) 2 else -1)) {
            alignment1.insert(0, seq1[i-1])
            alignment2.insert(0, seq2[j-1])
            i--
            j--
        } else if (i > 0 && dp[i][j] == dp[i-1][j] - 1) {
            alignment1.insert(0, seq1[i-1])
            alignment2.insert(0, '-')
            i--
        } else {
            alignment1.insert(0, '-')
            alignment2.insert(0, seq2[j-1])
            j--
        }
    }
    
    return Pair(alignment1.toString(), alignment2.toString())
}

fun main() {
    // Example usage with sample data
    val seq1 = "GAGT"
    val seq2 = "GAT"
    
    val (alignment1, alignment2) = semiglobalAlignmentOptimized(seq1, seq2)
    
    println("Sequence 1: $seq1")
    println("Sequence 2: $seq2")
    println("Alignment 1: $alignment1")
    println("Alignment 2: $alignment2")
    
    // Read from file (if needed for Rosalind)
    /*
    val lines = File("input.txt").readLines()
    val seq1 = lines[0]
    val seq2 = lines[1]
    
    val (alignment1, alignment2) = semiglobalAlignmentOptimized(seq1, seq2)
    println(alignment1)
    println(alignment2)
    */
}

// Alternative implementation for Rosalind specific format
fun solveSemiglobalAlignment(inputFile: String): Pair<String, String> {
    val lines = File(inputFile).readLines()
    val seq1 = lines[0]
    val seq2 = lines[1]
    
    return semiglobalAlignmentOptimized(seq1, seq2)
}
```

## Key Features of the Solution

1. **Dynamic Programming Table**: Creates a 2D array to store optimal alignment scores
2. **Boundary Conditions**: Allows gaps at the beginning and end of sequences
3. **Traceback**: Reconstructs the actual alignment by backtracking through the DP table
4. **Scoring System**: Uses standard scoring (match = 2, mismatch = -1, gap = -1)

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the DP table

## Usage for Rosalind

For the actual Rosalind problem, you would typically:
1. Read input sequences from a file
2. Call the `semiglobalAlignmentOptimized` function
3. Output the resulting alignments

The solution handles the semiglobal alignment requirement where gaps are allowed at the beginning and end but not in the middle of the alignment, making it suitable for finding local similarities between sequences.

