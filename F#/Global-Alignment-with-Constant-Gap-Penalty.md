# Rosalind Problem: Global Alignment with Constant Gap Penalty

I'll solve this step-by-step using F# to find the optimal global alignment of two sequences with a constant gap penalty.

## Problem Understanding

We need to find the optimal global alignment between two sequences using dynamic programming with a constant gap penalty. This is a classic sequence alignment problem.

## Solution Approach

I'll implement the Needleman-Wunsch algorithm with constant gap penalty:

1. Create a dynamic programming matrix
2. Fill the matrix using the recurrence relation
3. Trace back to find the optimal alignment
4. Return the alignment score and aligned sequences

## F# Implementation

```fsharp
open System

// Function to compute global alignment with constant gap penalty
let globalAlignmentWithGapPenalty (seq1: string) (seq2: string) (gapPenalty: int) (matchScore: int) (mismatchScore: int) =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create DP matrix
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize first row and column
    for i in 0 .. m do
        dp.[i, 0] <- i * gapPenalty
    
    for j in 0 .. n do
        dp.[0, j] <- j * gapPenalty
    
    // Fill the DP matrix
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScoreVal = 
                if seq1.[i-1] = seq2.[j-1] then matchScore 
                else mismatchScore
            
            dp.[i, j] <- max [
                dp.[i-1, j] + gapPenalty  // deletion
                dp.[i, j-1] + gapPenalty  // insertion
                dp.[i-1, j-1] + matchScoreVal  // match/mismatch
            ]
    
    // Traceback to find alignment
    let rec traceback i j align1 align2 =
        if i = 0 && j = 0 then
            (align1, align2)
        elif i = 0 then
            traceback i (j-1) ( "-" + align1) (seq2.[j-1] + align2)
        elif j = 0 then
            traceback (i-1) j (seq1.[i-1] + align1) ( "-" + align2)
        else
            let matchScoreVal = 
                if seq1.[i-1] = seq2.[j-1] then matchScore 
                else mismatchScore
            
            let current = dp.[i, j]
            let fromDiag = dp.[i-1, j-1] + matchScoreVal
            let fromUp = dp.[i-1, j] + gapPenalty
            let fromLeft = dp.[i, j-1] + gapPenalty
            
            if current = fromDiag then
                traceback (i-1) (j-1) (seq1.[i-1] + align1) (seq2.[j-1] + align2)
            elif current = fromUp then
                traceback (i-1) j (seq1.[i-1] + align1) ( "-" + align2)
            else
                traceback i (j-1) ( "-" + align1) (seq2.[j-1] + align2)
    
    let (aligned1, aligned2) = traceback m n "" ""
    (dp.[m, n], aligned1, aligned2)

// Alternative implementation with cleaner traceback
let globalAlignmentWithGapPenalty2 (seq1: string) (seq2: string) (gapPenalty: int) (matchScore: int) (mismatchScore: int) =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create DP matrix
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize first row and column
    for i in 0 .. m do
        dp.[i, 0] <- i * gapPenalty
    
    for j in 0 .. n do
        dp.[0, j] <- j * gapPenalty
    
    // Fill the DP matrix
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScoreVal = 
                if seq1.[i-1] = seq2.[j-1] then matchScore 
                else mismatchScore
            
            dp.[i, j] <- max [
                dp.[i-1, j] + gapPenalty  // deletion
                dp.[i, j-1] + gapPenalty  // insertion
                dp.[i-1, j-1] + matchScoreVal  // match/mismatch
            ]
    
    // Traceback
    let mutable align1 = ""
    let mutable align2 = ""
    let mutable i = m
    let mutable j = n
    
    while i > 0 || j > 0 do
        if i > 0 && j > 0 then
            let matchScoreVal = 
                if seq1.[i-1] = seq2.[j-1] then matchScore 
                else mismatchScore
            
            let current = dp.[i, j]
            let fromDiag = dp.[i-1, j-1] + matchScoreVal
            let fromUp = dp.[i-1, j] + gapPenalty
            let fromLeft = dp.[i, j-1] + gapPenalty
            
            if current = fromDiag then
                align1 <- seq1.[i-1] + align1
                align2 <- seq2.[j-1] + align2
                i <- i - 1
                j <- j - 1
            elif current = fromUp then
                align1 <- seq1.[i-1] + align1
                align2 <- "-" + align2
                i <- i - 1
            else
                align1 <- "-" + align1
                align2 <- seq2.[j-1] + align2
                j <- j - 1
        elif i > 0 then
            align1 <- seq1.[i-1] + align1
            align2 <- "-" + align2
            i <- i - 1
        else
            align1 <- "-" + align1
            align2 <- seq2.[j-1] + align2
            j <- j - 1
    
    (dp.[m, n], align1, align2)

// Example usage
let solveRosalindProblem () =
    // Example sequences from Rosalind
    let seq1 = "ACGTACGT"
    let seq2 = "ACGTACGT"
    
    // Parameters
    let gapPenalty = -2
    let matchScore = 1
    let mismatchScore = -1
    
    let (score, aligned1, aligned2) = 
        globalAlignmentWithGapPenalty2 seq1 seq2 gapPenalty matchScore mismatchScore
    
    printfn "Alignment Score: %d" score
    printfn "Sequence 1: %s" aligned1
    printfn "Sequence 2: %s" aligned2
    
    // Return just the score for Rosalind submission
    score

// Test with sample data
let testWithSampleData () =
    let seq1 = "ACGTACGT"
    let seq2 = "ACGTACGT"
    
    let gapPenalty = -2
    let matchScore = 1
    let mismatchScore = -1
    
    let (score, aligned1, aligned2) = 
        globalAlignmentWithGapPenalty2 seq1 seq2 gapPenalty matchScore mismatchScore
    
    printfn "Score: %d" score
    printfn "Aligned 1: %s" aligned1
    printfn "Aligned 2: %s" aligned2

// Run the test
testWithSampleData ()
```

## Explanation

This solution implements the global sequence alignment algorithm with:

1. **Dynamic Programming Matrix**: Creates an (m+1) × (n+1) matrix where m and n are the lengths of the sequences
2. **Initialization**: First row and column are filled with gap penalties
3. **Matrix Filling**: Each cell is filled using the recurrence relation:
   ```
   dp[i,j] = max(dp[i-1,j] + gapPenalty, dp[i,j-1] + gapPenalty, dp[i-1,j-1] + matchScore)
   ```
4. **Traceback**: Reconstructs the alignment by following the path that led to the optimal score

## Key Features

- **Constant Gap Penalty**: All gaps (insertions/deletions) have the same penalty
- **Flexible Scoring**: Supports different match/mismatch scores
- **Optimal Alignment**: Returns both the score and the actual aligned sequences
- **Efficient**: O(mn) time complexity and O(mn) space complexity

## Usage for Rosalind

For Rosalind submission, you would typically:
1. Parse input sequences from file
2. Set appropriate scoring parameters
3. Return only the alignment score (or the aligned sequences as required)

The function `globalAlignmentWithGapPenalty2` provides the complete solution that can be adapted for specific Rosalind test cases.

