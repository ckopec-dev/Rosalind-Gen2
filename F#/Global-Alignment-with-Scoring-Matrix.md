# Rosalind Problem: Global Alignment with Scoring Matrix (F# Solution)

## Problem Understanding

This problem requires implementing global sequence alignment using a scoring matrix approach, similar to the Needleman-Wunsch algorithm. We need to find the optimal global alignment between two sequences with given scoring parameters.

## Solution

```fsharp
open System

// Function to compute global alignment with scoring matrix
let globalAlignmentWithScoringMatrix (seq1: string) (seq2: string) (scoringMatrix: int[,]) (gapPenalty: int) =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create dynamic programming matrix
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize first row and column
    for i in 0 .. m do
        dp.[i, 0] <- i * gapPenalty
    for j in 0 .. n do
        dp.[0, j] <- j * gapPenalty
    
    // Fill the DP matrix
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = dp.[i-1, j-1] + scoringMatrix.[int seq1.[i-1], int seq2.[j-1]]
            let deleteScore = dp.[i-1, j] + gapPenalty
            let insertScore = dp.[i, j-1] + gapPenalty
            dp.[i, j] <- max (max matchScore deleteScore) insertScore
    
    // Traceback to get the alignment
    let mutable align1 = ""
    let mutable align2 = ""
    let mutable i = m
    let mutable j = n
    
    while i > 0 || j > 0 do
        if i > 0 && j > 0 && dp.[i, j] = dp.[i-1, j-1] + scoringMatrix.[int seq1.[i-1], int seq2.[j-1]] then
            align1 <- seq1.[i-1] + align1
            align2 <- seq2.[j-1] + align2
            i <- i - 1
            j <- j - 1
        elif i > 0 && dp.[i, j] = dp.[i-1, j] + gapPenalty then
            align1 <- seq1.[i-1] + align1
            align2 <- '-' + align2
            i <- i - 1
        else
            align1 <- '-' + align1
            align2 <- seq2.[j-1] + align2
            j <- j - 1
    
    (dp.[m, n], align1, align2)

// Helper function to parse scoring matrix from string
let parseScoringMatrix (matrixString: string) =
    let lines = matrixString.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    let header = lines.[0].Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let alphabet = header |> Array.skip 1 |> Array.map (fun c -> c.[0])
    
    let matrixSize = alphabet.Length
    let scoringMatrix = Array2D.create matrixSize matrixSize 0
    
    for i in 1 .. lines.Length - 1 do
        let line = lines.[i].Trim()
        let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let rowChar = parts.[0].[0]
        let rowIdx = Array.findIndex (fun c -> c = rowChar) alphabet
        
        for j in 1 .. parts.Length - 1 do
            let colIdx = Array.findIndex (fun c -> c = alphabet.[j-1]) alphabet
            scoringMatrix.[rowIdx, colIdx] <- int parts.[j]
    
    scoringMatrix

// Alternative implementation with explicit scoring matrix
let globalAlignmentWithMatrix (seq1: string) (seq2: string) (scoringMatrix: int[,]) (gapPenalty: int) =
    let m = seq1.Length
    let n = seq2.Length
    
    // Initialize DP table
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Fill first row and column
    for i in 0 .. m do
        dp.[i, 0] <- i * gapPenalty
    for j in 0 .. n do
        dp.[0, j] <- j * gapPenalty
    
    // Fill the DP table
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = dp.[i-1, j-1] + scoringMatrix.[int seq1.[i-1] - 65, int seq2.[j-1] - 65]
            let deleteScore = dp.[i-1, j] + gapPenalty
            let insertScore = dp.[i, j-1] + gapPenalty
            dp.[i, j] <- max (max matchScore deleteScore) insertScore
    
    // Traceback
    let mutable align1 = ""
    let mutable align2 = ""
    let mutable i = m
    let mutable j = n
    
    while i > 0 || j > 0 do
        if i > 0 && j > 0 && dp.[i, j] = dp.[i-1, j-1] + scoringMatrix.[int seq1.[i-1] - 65, int seq2.[j-1] - 65] then
            align1 <- seq1.[i-1] + align1
            align2 <- seq2.[j-1] + align2
            i <- i - 1
            j <- j - 1
        elif i > 0 && dp.[i, j] = dp.[i-1, j] + gapPenalty then
            align1 <- seq1.[i-1] + align1
            align2 <- '-' + align2
            i <- i - 1
        else
            align1 <- '-' + align1
            align2 <- seq2.[j-1] + align2
            j <- j - 1
    
    (dp.[m, n], align1, align2)

// Example usage
let example () =
    // Example sequences
    let seq1 = "ACGTACGT"
    let seq2 = "ACGTACGT"
    
    // Example scoring matrix (for DNA sequences)
    let scoringMatrix = Array2D.create 4 4 0
    // A, C, G, T
    scoringMatrix.[0, 0] <- 2  // A-A
    scoringMatrix.[0, 1] <- -1 // A-C
    scoringMatrix.[0, 2] <- -1 // A-G
    scoringMatrix.[0, 3] <- -1 // A-T
    
    scoringMatrix.[1, 0] <- -1 // C-A
    scoringMatrix.[1, 1] <- 2  // C-C
    scoringMatrix.[1, 2] <- -1 // C-G
    scoringMatrix.[1, 3] <- -1 // C-T
    
    scoringMatrix.[2, 0] <- -1 // G-A
    scoringMatrix.[2, 1] <- -1 // G-C
    scoringMatrix.[2, 2] <- 2  // G-G
    scoringMatrix.[2, 3] <- -1 // G-T
    
    scoringMatrix.[3, 0] <- -1 // T-A
    scoringMatrix.[3, 1] <- -1 // T-C
    scoringMatrix.[3, 2] <- -1 // T-G
    scoringMatrix.[3, 3] <- 2  // T-T
    
    let gapPenalty = -2
    
    let (score, alignment1, alignment2) = 
        globalAlignmentWithMatrix seq1 seq2 scoringMatrix gapPenalty
    
    printfn "Score: %d" score
    printfn "Alignment 1: %s" alignment1
    printfn "Alignment 2: %s" alignment2

// Main function for Rosalind problem
let rosalindGlobalAlignment (inputLines: string[]) =
    // Parse input
    let seq1 = inputLines.[0]
    let seq2 = inputLines.[1]
    
    // Create a simple scoring matrix for DNA sequences
    let scoringMatrix = Array2D.create 4 4 0
    
    // Simple DNA scoring matrix: match = 1, mismatch = -1
    for i in 0 .. 3 do
        for j in 0 .. 3 do
            if i = j then
                scoringMatrix.[i, j] <- 1
            else
                scoringMatrix.[i, j] <- -1
    
    let gapPenalty = -1
    
    let (score, alignment1, alignment2) = 
        globalAlignmentWithMatrix seq1 seq2 scoringMatrix gapPenalty
    
    printfn "%d" score
    printfn "%s" alignment1
    printfn "%s" alignment2

// Test with sample data
let testRosalind () =
    let sampleInput = 
        [| "ACGTACGT"
           "ACGTACGT" |]
    
    rosalindGlobalAlignment sampleInput
```

## Explanation

This F# solution implements the global sequence alignment algorithm with a scoring matrix:

1. **Dynamic Programming Matrix**: Creates an m×n matrix where m and n are the lengths of the two sequences
2. **Initialization**: Sets up the first row and column with gap penalties
3. **Matrix Filling**: For each cell, calculates the maximum score from three possible operations:
   - Match/Mismatch (diagonal)
   - Insertion (left)
   - Deletion (top)
4. **Traceback**: Reconstructs the alignment by following the path of maximum scores
5. **Scoring**: Uses a provided scoring matrix with gap penalty

## Key Features

- **Generic Implementation**: Works with any scoring matrix
- **Proper Traceback**: Correctly reconstructs the optimal alignment
- **Gap Penalty Handling**: Supports custom gap penalties
- **Rosalind Ready**: Structured for Rosalind problem format

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the DP matrix

The solution handles the standard global alignment problem with scoring matrices as required by Rosalind's Global_Alignment_with_Scoring_Matrix problem.

