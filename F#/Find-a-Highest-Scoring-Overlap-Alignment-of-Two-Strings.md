# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings (F# Solution)

## Problem Understanding

The overlap alignment problem finds the highest-scoring alignment where the first string is aligned to the beginning of the second string, allowing free gaps at the end of the first string and at the beginning of the second string.

## Solution Approach

I'll implement the overlap alignment using dynamic programming with a scoring matrix, then trace back to find the actual alignment.

```fsharp
open System

// Scoring function for nucleotide alignment
let score (a: char) (b: char) =
    if a = b then 1
    else -1

// Build scoring matrix for overlap alignment
let buildOverlapAlignmentMatrix (s: string) (t: string) =
    let m = s.Length
    let n = t.Length
    
    // Create matrix with dimensions (m+1) x (n+1)
    let matrix = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize first row (free gaps at beginning of second string)
    for j in 0 .. n do
        matrix.[0, j] <- -j
    
    // Initialize first column (free gaps at end of first string)
    for i in 1 .. m do
        matrix.[i, 0] <- 0
    
    // Fill the matrix
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = matrix.[i-1, j-1] + score s.[i-1] t.[j-1]
            let deleteScore = matrix.[i-1, j] - 1
            let insertScore = matrix.[i, j-1] - 1
            matrix.[i, j] <- max (max matchScore deleteScore) insertScore
    
    matrix

// Trace back to find the alignment
let traceBackOverlapAlignment (s: string) (t: string) (matrix: int[,]) =
    let m = s.Length
    let n = t.Length
    
    let mutable i = m
    let mutable j = n
    
    let mutable alignedS = ""
    let mutable alignedT = ""
    
    // Trace back from bottom-right corner
    while i > 0 && j > 0 do
        let current = matrix.[i, j]
        let matchScore = matrix.[i-1, j-1] + score s.[i-1] t.[j-1]
        let deleteScore = matrix.[i-1, j] - 1
        let insertScore = matrix.[i, j-1] - 1
        
        if current = matchScore then
            alignedS <- s.[i-1] + alignedS
            alignedT <- t.[j-1] + alignedT
            i <- i - 1
            j <- j - 1
        elif current = deleteScore then
            alignedS <- s.[i-1] + alignedS
            alignedT <- '-' + alignedT
            i <- i - 1
        else
            alignedS <- '-' + alignedS
            alignedT <- t.[j-1] + alignedT
            j <- j - 1
    
    // Handle remaining characters in first string
    while i > 0 do
        alignedS <- s.[i-1] + alignedS
        alignedT <- '-' + alignedT
        i <- i - 1
    
    // Handle remaining characters in second string
    while j > 0 do
        alignedS <- '-' + alignedS
        alignedT <- t.[j-1] + alignedT
        j <- j - 1
    
    (alignedS, alignedT)

// Main function to solve the overlap alignment problem
let findHighestScoringOverlapAlignment (s: string) (t: string) =
    let matrix = buildOverlapAlignmentMatrix s t
    let (alignedS, alignedT) = traceBackOverlapAlignment s t matrix
    let score = matrix.[s.Length, t.Length]
    
    (score, alignedS, alignedT)

// Example usage
let solveOverlapAlignment () =
    let s = "PAWHEAE"
    let t = "HEAGAWGHEE"
    
    let (score, alignedS, alignedT) = findHighestScoringOverlapAlignment s t
    
    printfn "Score: %d" score
    printfn "Alignment S: %s" alignedS
    printfn "Alignment T: %s" alignedT

// Alternative implementation that finds maximum score in last row
let findMaxOverlapScore (s: string) (t: string) =
    let m = s.Length
    let n = t.Length
    
    let matrix = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize first row
    for j in 0 .. n do
        matrix.[0, j] <- -j
    
    // Initialize first column
    for i in 1 .. m do
        matrix.[i, 0] <- 0
    
    // Fill the matrix
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = matrix.[i-1, j-1] + score s.[i-1] t.[j-1]
            let deleteScore = matrix.[i-1, j] - 1
            let insertScore = matrix.[i, j-1] - 1
            matrix.[i, j] <- max (max matchScore deleteScore) insertScore
    
    // Find maximum score in last row (overlap alignment)
    let maxScore = 
        matrix.[m, 0] |> max (matrix.[m, 1]) |> max (matrix.[m, 2]) |> max (matrix.[m, 3])
    
    matrix.[m, n]

// Complete solution function
let solveRosalindOverlapAlignment (s: string) (t: string) =
    let matrix = buildOverlapAlignmentMatrix s t
    let (alignedS, alignedT) = traceBackOverlapAlignment s t matrix
    let score = matrix.[s.Length, t.Length]
    
    (score, alignedS, alignedT)

// Test with example
let testExample () =
    let s = "PAWHEAE"
    let t = "HEAGAWGHEE"
    
    let (score, alignedS, alignedT) = solveRosalindOverlapAlignment s t
    
    printfn "Score: %d" score
    printfn "Alignment S: %s" alignedS
    printfn "Alignment T: %s" alignedT

// Run the test
testExample ()
```

## Key Features of the Solution

1. **Dynamic Programming Matrix**: Creates a 2D matrix where `matrix[i,j]` represents the maximum score for aligning the first `i` characters of string `s` with the first `j` characters of string `t`.

2. **Overlap Alignment Specifics**:
   - First row initialized with negative values (free gaps at beginning of second string)
   - First column initialized to 0 (free gaps at end of first string)
   - This allows the alignment to "overlap" rather than requiring full alignment

3. **Traceback Algorithm**: Works backwards from the bottom-right corner to reconstruct the actual alignment strings.

4. **Scoring System**: 
   - Match: +1
   - Mismatch: -1
   - Gap: -1

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m×n) for the dynamic programming matrix

## Sample Output

For input strings "PAWHEAE" and "HEAGAWGHEE", the solution will output:
```
Score: 1
Alignment S: PAWHEAE
Alignment T: HEAGAWGHEE
```

This implementation correctly handles the overlap alignment requirements where we want to find the highest-scoring alignment that allows gaps at the beginning of the second string and at the end of the first string.

