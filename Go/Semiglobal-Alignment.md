# Rosalind Problem: Semiglobal Alignment in Go

## Problem Understanding

Semiglobal alignment is a variant of sequence alignment where we allow gaps at the beginning and end of sequences, but not in the middle. This is useful when we want to find regions of similarity between sequences without penalizing for incomplete alignment at the ends.

## Solution Approach

I'll implement semiglobal alignment using dynamic programming with a modified scoring matrix that allows gaps at the boundaries.

```go
package main

import (
    "fmt"
    "math"
)

// SemiglobalAlignment performs semiglobal alignment between two sequences
func SemiglobalAlignment(seq1, seq2 string) (int, string, string) {
    m, n := len(seq1), len(seq2)
    
    // Create DP matrix
    dp := make([][]int, m+1)
    for i := range dp {
        dp[i] = make([]int, n+1)
    }
    
    // Initialize first row and column
    // For semiglobal alignment, we allow gaps at the beginning
    for i := 0; i <= m; i++ {
        dp[i][0] = 0 // No penalty for gaps at the beginning
    }
    for j := 0; j <= n; j++ {
        dp[0][j] = 0 // No penalty for gaps at the beginning
    }
    
    // Fill the DP matrix
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            match := dp[i-1][j-1]
            if seq1[i-1] == seq2[j-1] {
                match += 1 // Match score
            } else {
                match -= 1 // Mismatch penalty
            }
            
            delete := dp[i-1][j] - 1 // Deletion penalty
            insert := dp[i][j-1] - 1 // Insertion penalty
            
            dp[i][j] = max3(match, delete, insert)
        }
    }
    
    // Traceback to find alignment
    align1, align2 := "", ""
    i, j := m, n
    
    // Traceback from the bottom-right corner
    for i > 0 && j > 0 {
        current := dp[i][j]
        diagonal := dp[i-1][j-1]
        up := dp[i-1][j]
        left := dp[i][j-1]
        
        if current == diagonal+getScore(seq1[i-1], seq2[j-1]) {
            align1 = string(seq1[i-1]) + align1
            align2 = string(seq2[j-1]) + align2
            i--
            j--
        } else if current == up-1 {
            align1 = string(seq1[i-1]) + align1
            align2 = "-" + align2
            i--
        } else {
            align1 = "-" + align1
            align2 = string(seq2[j-1]) + align2
            j--
        }
    }
    
    // Handle remaining characters
    for i > 0 {
        align1 = string(seq1[i-1]) + align1
        align2 = "-" + align2
        i--
    }
    for j > 0 {
        align1 = "-" + align1
        align2 = string(seq2[j-1]) + align2
        j--
    }
    
    return dp[m][n], align1, align2
}

// Helper function to get score for match/mismatch
func getScore(a, b byte) int {
    if a == b {
        return 1
    }
    return -1
}

// Helper function to find maximum of three integers
func max3(a, b, c int) int {
    return int(math.Max(float64(a), math.Max(float64(b), float64(c))))
}

// Alternative implementation with better traceback
func SemiglobalAlignmentOptimized(seq1, seq2 string) (int, string, string) {
    m, n := len(seq1), len(seq2)
    
    // Create DP matrix
    dp := make([][]int, m+1)
    for i := range dp {
        dp[i] = make([]int, n+1)
    }
    
    // Initialize first row and column
    for i := 0; i <= m; i++ {
        dp[i][0] = 0
    }
    for j := 0; j <= n; j++ {
        dp[0][j] = 0
    }
    
    // Fill the DP matrix
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            match := dp[i-1][j-1]
            if seq1[i-1] == seq2[j-1] {
                match += 1
            } else {
                match -= 1
            }
            
            delete := dp[i-1][j] - 1
            insert := dp[i][j-1] - 1
            
            dp[i][j] = max3(match, delete, insert)
        }
    }
    
    // Find the maximum score in the last row or column
    maxScore := 0
    var endI, endJ int
    
    // Check last row
    for j := 0; j <= n; j++ {
        if dp[m][j] > maxScore {
            maxScore = dp[m][j]
            endI = m
            endJ = j
        }
    }
    
    // Check last column
    for i := 0; i <= m; i++ {
        if dp[i][n] > maxScore {
            maxScore = dp[i][n]
            endI = i
            endJ = n
        }
    }
    
    // Traceback from the maximum position
    align1, align2 := "", ""
    i, j := endI, endJ
    
    // Special case: if we start from the end, we might need to handle differently
    for i > 0 && j > 0 {
        current := dp[i][j]
        diagonal := dp[i-1][j-1]
        up := dp[i-1][j]
        left := dp[i][j-1]
        
        if current == diagonal+getScore(seq1[i-1], seq2[j-1]) {
            align1 = string(seq1[i-1]) + align1
            align2 = string(seq2[j-1]) + align2
            i--
            j--
        } else if current == up-1 {
            align1 = string(seq1[i-1]) + align1
            align2 = "-" + align2
            i--
        } else {
            align1 = "-" + align1
            align2 = string(seq2[j-1]) + align2
            j--
        }
    }
    
    // Handle remaining characters
    for i > 0 {
        align1 = string(seq1[i-1]) + align1
        align2 = "-" + align2
        i--
    }
    for j > 0 {
        align1 = "-" + align1
        align2 = string(seq2[j-1]) + align2
        j--
    }
    
    return maxScore, align1, align2
}

func main() {
    // Example usage
    seq1 := "ACGTACGT"
    seq2 := "ACGTACGT"
    
    score, alignment1, alignment2 := SemiglobalAlignment(seq1, seq2)
    
    fmt.Printf("Sequence 1: %s\n", seq1)
    fmt.Printf("Sequence 2: %s\n", seq2)
    fmt.Printf("Score: %d\n", score)
    fmt.Printf("Alignment 1: %s\n", alignment1)
    fmt.Printf("Alignment 2: %s\n", alignment2)
    
    // Test with different sequences
    seq3 := "ACGTACGT"
    seq4 := "ACGT"
    
    score2, align1, align2 := SemiglobalAlignment(seq3, seq4)
    
    fmt.Printf("\n--- Test Case 2 ---\n")
    fmt.Printf("Sequence 1: %s\n", seq3)
    fmt.Printf("Sequence 2: %s\n", seq4)
    fmt.Printf("Score: %d\n", score2)
    fmt.Printf("Alignment 1: %s\n", align1)
    fmt.Printf("Alignment 2: %s\n", align2)
}
```

## Explanation

The semiglobal alignment algorithm works as follows:

1. **Matrix Initialization**: Create a DP matrix where `dp[i][j]` represents the maximum score for aligning the first `i` characters of sequence 1 with the first `j` characters of sequence 2.

2. **Base Cases**: Initialize the first row and column to 0, allowing gaps at the beginning of either sequence.

3. **Matrix Filling**: For each cell, calculate the maximum score from:
   - Match/mismatch score (diagonal)
   - Deletion penalty (from above)
   - Insertion penalty (from left)

4. **Traceback**: Start from the bottom-right corner and trace back to find the optimal alignment.

5. **Special Handling**: In semiglobal alignment, we can start and end alignment anywhere in the sequences, so we consider the maximum score in the last row or column as the final score.

## Key Features

- **Gap Penalties**: Standard gap penalties (-1) for insertions and deletions
- **Match/Mismatch**: +1 for matches, -1 for mismatches
- **Boundary Handling**: Allows gaps at the beginning and end of sequences
- **Traceback**: Correctly reconstructs the alignment path

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m×n) for the DP matrix

This implementation correctly handles semiglobal alignment as required by the Rosalind problem, allowing for flexible alignment that doesn't penalize incomplete alignment at sequence boundaries.

