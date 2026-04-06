# Rosalind Problem: Consensus and Profile in F#

## Problem Description
Given a collection of DNA strings, we need to find the consensus string and profile matrix.

## Solution

```fsharp
open System
open System.IO

// Function to calculate profile matrix and consensus string
let consensusAndProfile (dnaStrings: string list) : string * int[,][] =
    if List.isEmpty dnaStrings then
        failwith "Empty DNA strings list"
    
    let length = dnaStrings.[0].Length
    let seqCount = List.length dnaStrings
    
    // Initialize profile matrix: rows = A, C, G, T; columns = positions
    let profile = Array2D.create 4 length 0
    
    // Process each DNA string
    for dna in dnaStrings do
        for i = 0 to length - 1 do
            match dna.[i] with
            | 'A' -> profile.[0, i] <- profile.[0, i] + 1
            | 'C' -> profile.[1, i] <- profile.[1, i] + 1
            | 'G' -> profile.[2, i] <- profile.[2, i] + 1
            | 'T' -> profile.[3, i] <- profile.[3, i] + 1
            | _ -> failwith "Invalid DNA character"
    
    // Generate consensus string
    let consensus = 
        [0 .. length - 1]
        |> List.map (fun i -> 
            let maxIndex = 
                [0 .. 3]
                |> List.maxBy (fun j -> profile.[j, i])
            match maxIndex with
            | 0 -> 'A'
            | 1 -> 'C'
            | 2 -> 'G'
            | 3 -> 'T'
            | _ -> failwith "Unexpected index")
        |> List.toArray
        |> System.String.Concat
    
    (consensus, profile)

// Alternative implementation with more functional approach
let consensusAndProfileFunctional (dnaStrings: string list) : string * int[,][] =
    if List.isEmpty dnaStrings then
        failwith "Empty DNA strings list"
    
    let length = dnaStrings.[0].Length
    
    // Create profile matrix using fold
    let profile = 
        dnaStrings
        |> List.fold (fun acc dna ->
            for i = 0 to length - 1 do
                match dna.[i] with
                | 'A' -> acc.[0, i] <- acc.[0, i] + 1
                | 'C' -> acc.[1, i] <- acc.[1, i] + 1
                | 'G' -> acc.[2, i] <- acc.[2, i] + 1
                | 'T' -> acc.[3, i] <- acc.[3, i] + 1
                | _ -> failwith "Invalid DNA character"
            acc
        ) (Array2D.create 4 length 0)
    
    // Generate consensus string
    let consensus = 
        [0 .. length - 1]
        |> List.map (fun i -> 
            let maxIndex = 
                [0 .. 3]
                |> List.maxBy (fun j -> profile.[j, i])
            match maxIndex with
            | 0 -> 'A'
            | 1 -> 'C'
            | 2 -> 'G'
            | 3 -> 'T'
            | _ -> failwith "Unexpected index")
        |> List.toArray
        |> System.String.Concat
    
    (consensus, profile)

// Helper function to print profile matrix in required format
let printProfile (profile: int[,]) : unit =
    let nucleotides = ['A'; 'C'; 'G'; 'T']
    for i = 0 to 3 do
        printfn "%c: %s" nucleotides.[i] (profile.[i, 0 ..] |> Array.map string |> String.concat " ")

// Main function to solve the problem
let solveConsensusAndProfile (input: string) : unit =
    let lines = input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    
    // Parse FASTA format to get DNA strings
    let dnaStrings = 
        lines
        |> Array.toList
        |> List.windowed 2
        |> List.filter (fun w -> w.[0].StartsWith(">"))
        |> List.map (fun w -> w.[1])
    
    let (consensus, profile) = consensusAndProfile dnaStrings
    
    // Print results
    printfn "%s" consensus
    printProfile profile

// Example usage with sample data
let sampleData = 
    ">Rosalind_1
ATCCAGCT
>Rosalind_2
GGGCAACT
>Rosalind_3
ATGGATCT
>Rosalind_4
AAGCAACC
>Rosalind_5
TTGGAACT
>Rosalind_6
ATGCCATT
>Rosalind_7
ATGGCACT"

// Run with sample data
solveConsensusAndProfile sampleData
```

## Explanation

This F# solution implements the consensus and profile problem in the following way:

### Key Components:

1. **Profile Matrix Calculation**: 
   - Creates a 4×n matrix where rows represent nucleotides (A, C, G, T) and columns represent positions
   - Counts occurrences of each nucleotide at each position across all DNA strings

2. **Consensus String Generation**:
   - For each position, finds the nucleotide with maximum count
   - Constructs the consensus string by taking the most frequent nucleotide at each position

3. **Data Processing**:
   - Handles FASTA format input
   - Processes DNA strings to extract sequences
   - Returns both consensus string and profile matrix

### Time Complexity:
- **Time**: O(n × m) where n is the number of DNA strings and m is the length of each string
- **Space**: O(1) for the profile matrix (fixed 4 rows) plus O(m) for the output

### Sample Output Format:
```
ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6
```

The solution handles the standard Rosalind problem requirements and can be easily adapted for different input formats or additional processing needs.

