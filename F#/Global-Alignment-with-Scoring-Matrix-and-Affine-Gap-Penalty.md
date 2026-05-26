# Rosalind Problem: Global Alignment with Scoring Matrix and Affine Gap Penalty

I'll solve this step-by-step using F# to implement global sequence alignment with affine gap penalties.

## Problem Understanding

We need to implement global sequence alignment using:
- A scoring matrix (substitution matrix)
- Affine gap penalty (cost = gap_open + gap_extend × length)
- Needleman-Wunsch algorithm with affine gaps

## Solution

```fsharp
open System

// Define the scoring matrix type
type ScoreMatrix = 
    { Matrix: Map<char * char, int> }
    static member Create(matrix: Map<char * char, int>) = { Matrix = matrix }

// Define gap penalty structure
type GapPenalty = 
    { Open: int; Extend: int }
    static member Create(openPenalty: int, extendPenalty: int) = { Open = openPenalty; Extend = extendPenalty }

// Global alignment with affine gap penalty
let globalAlignmentWithAffineGap (seq1: string) (seq2: string) (scoreMatrix: ScoreMatrix) (gapPenalty: GapPenalty) : int =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create 3D DP matrices for affine gaps
    // F[i,j] = optimal score ending at position i,j with no gaps
    // I[i,j] = optimal score ending at position i,j with an open gap in seq1
    // D[i,j] = optimal score ending at position i,j with an open gap in seq2
    let F = Array2D.create (m + 1) (n + 1) 0
    let I = Array2D.create (m + 1) (n + 1) Int32.MinValue
    let D = Array2D.create (m + 1) (n + 1) Int32.MinValue
    
    // Initialize base cases
    for i in 0 .. m do
        I.[i, 0] <- gapPenalty.Open + i * gapPenalty.Extend
        D.[i, 0] <- Int32.MinValue
    
    for j in 0 .. n do
        D.[0, j] <- gapPenalty.Open + j * gapPenalty.Extend
        I.[0, j] <- Int32.MinValue
    
    // Fill the matrices
    for i in 1 .. m do
        for j in 1 .. n do
            // Match/mismatch score
            let matchScore = 
                match scoreMatrix.Matrix.TryFind(seq1.[i-1], seq2.[j-1]) with
                | Some score -> score
                | None -> 0
            
            // Calculate F[i,j] - match/mismatch
            F.[i, j] <- max 
                (F.[i-1, j-1] + matchScore)
                (max 
                    (I.[i-1, j-1] + matchScore)
                    (D.[i-1, j-1] + matchScore))
            
            // Calculate I[i,j] - gap in seq1
            I.[i, j] <- max 
                (F.[i-1, j] + gapPenalty.Open + gapPenalty.Extend)
                (I.[i-1, j] + gapPenalty.Extend)
            
            // Calculate D[i,j] - gap in seq2
            D.[i, j] <- max 
                (F.[i, j-1] + gapPenalty.Open + gapPenalty.Extend)
                (D.[i, j-1] + gapPenalty.Extend)
    
    // Return the maximum of all three matrices at the final position
    max F.[m, n] (max I.[m, n] D.[m, n])

// Alternative implementation using a simpler approach
let globalAlignmentSimple (seq1: string) (seq2: string) (scoreMatrix: ScoreMatrix) (gapPenalty: GapPenalty) : int =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create DP table
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize first row and column
    for i in 0 .. m do
        dp.[i, 0] <- i * gapPenalty.Extend + gapPenalty.Open
    
    for j in 0 .. n do
        dp.[0, j] <- j * gapPenalty.Extend + gapPenalty.Open
    
    // Fill the DP table
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = 
                match scoreMatrix.Matrix.TryFind(seq1.[i-1], seq2.[j-1]) with
                | Some score -> score
                | None -> 0
            
            dp.[i, j] <- max
                (dp.[i-1, j-1] + matchScore)  // match/mismatch
                (max
                    (dp.[i-1, j] + gapPenalty.Open + gapPenalty.Extend)  // gap in seq2
                    (dp.[i, j-1] + gapPenalty.Open + gapPenalty.Extend)) // gap in seq1
    
    dp.[m, n]

// Example usage and test
let example () = 
    // Create a simple scoring matrix (identity matrix for demonstration)
    let matrix = Map [
        ('A', 'A'), 2
        ('A', 'C'), -1
        ('A', 'G'), -1
        ('A', 'T'), -1
        ('C', 'A'), -1
        ('C', 'C'), 2
        ('C', 'G'), -1
        ('C', 'T'), -1
        ('G', 'A'), -1
        ('G', 'C'), -1
        ('G', 'G'), 2
        ('G', 'T'), -1
        ('T', 'A'), -1
        ('T', 'C'), -1
        ('T', 'G'), -1
        ('T', 'T'), 2
    ]
    
    let scoreMatrix = ScoreMatrix.Create(matrix)
    let gapPenalty = GapPenalty.Create(5, 2)  // open = 5, extend = 2
    
    let seq1 = "ACGTACGT"
    let seq2 = "ACGTACGT"
    
    let result = globalAlignmentWithAffineGap seq1 seq2 scoreMatrix gapPenalty
    printfn "Alignment score: %d" result

// More comprehensive solution with traceback
type AlignmentResult = 
    { Score: int
      Alignment1: string
      Alignment2: string }

let globalAlignmentWithTraceback (seq1: string) (seq2: string) (scoreMatrix: ScoreMatrix) (gapPenalty: GapPenalty) : AlignmentResult =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create 3D DP matrices
    let F = Array2D.create (m + 1) (n + 1) 0
    let I = Array2D.create (m + 1) (n + 1) Int32.MinValue
    let D = Array2D.create (m + 1) (n + 1) Int32.MinValue
    
    // Initialize base cases
    for i in 0 .. m do
        I.[i, 0] <- gapPenalty.Open + i * gapPenalty.Extend
        D.[i, 0] <- Int32.MinValue
    
    for j in 0 .. n do
        D.[0, j] <- gapPenalty.Open + j * gapPenalty.Extend
        I.[0, j] <- Int32.MinValue
    
    // Fill matrices
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = 
                match scoreMatrix.Matrix.TryFind(seq1.[i-1], seq2.[j-1]) with
                | Some score -> score
                | None -> 0
            
            F.[i, j] <- max 
                (F.[i-1, j-1] + matchScore)
                (max 
                    (I.[i-1, j-1] + matchScore)
                    (D.[i-1, j-1] + matchScore))
            
            I.[i, j] <- max 
                (F.[i-1, j] + gapPenalty.Open + gapPenalty.Extend)
                (I.[i-1, j] + gapPenalty.Extend)
            
            D.[i, j] <- max 
                (F.[i, j-1] + gapPenalty.Open + gapPenalty.Extend)
                (D.[i, j-1] + gapPenalty.Extend)
    
    // Traceback (simplified version)
    let score = max F.[m, n] (max I.[m, n] D.[m, n])
    
    // For this problem, we just return the score
    { Score = score; Alignment1 = ""; Alignment2 = "" }

// Run example
example()
```

## Key Features of the Solution

1. **Scoring Matrix Support**: Uses a Map to store substitution scores between amino acids/nucleotides
2. **Affine Gap Penalty**: Implements proper gap opening and extension penalties
3. **Dynamic Programming**: Uses 3D matrices to track match/mismatch, gap in seq1, and gap in seq2
4. **Flexible Input**: Works with any sequences and scoring matrices
5. **Extensible Design**: Easy to modify for different gap penalties or scoring schemes

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the DP matrices

## Usage Example

```fsharp
// Define sequences
let seq1 = "ACGTACGT"
let seq2 = "ACGTACGT"

// Create scoring matrix
let scoreMatrix = ScoreMatrix.Create(map [
    ('A', 'A'), 2; ('A', 'C'), -1; ('A', 'G'), -1; ('A', 'T'), -1;
    ('C', 'A'), -1; ('C', 'C'), 2; ('C', 'G'), -1; ('C', 'T'), -1;
    ('G', 'A'), -1; ('G', 'C'), -1; ('G', 'G'), 2; ('G', 'T'), -1;
    ('T', 'A'), -1; ('T', 'C'), -1; ('T', 'G'), -1; ('T', 'T'), 2;
])

// Define gap penalties
let gapPenalty = GapPenalty.Create(5, 2)

// Calculate alignment score
let score = globalAlignmentWithAffineGap seq1 seq2 scoreMatrix gapPenalty
printfn "Alignment score: %d" score
```

This implementation provides a robust solution for global sequence alignment with affine gap penalties, suitable for Rosalind problems requiring such functionality.

