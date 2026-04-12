# Rosalind Problem: Find_a_Median_String Solution in F#

## Problem Understanding

The median string problem asks us to find a k-mer (substring of length k) that minimizes the total Hamming distance to all strings in a given collection of DNA strings.

## Solution Approach

1. Generate all possible k-mers of length k from the DNA alphabet
2. For each k-mer, calculate the total Hamming distance to all input strings
3. Return the k-mer with minimum total distance

## F# Implementation

```fsharp
open System

// Calculate Hamming distance between two DNA strings
let hammingDistance (s1: string) (s2: string) : int =
    Seq.zip s1 s2
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.length

// Generate all possible k-mers of given length from DNA alphabet
let rec generateKMers (k: int) : string list =
    if k = 0 then [""] 
    else
        let prev = generateKMers (k - 1)
        ["A"; "C"; "G"; "T"]
        |> List.collect (fun base -> prev |> List.map (fun kmer -> base + kmer))

// Find the median string for a collection of DNA strings
let findMedianString (k: int) (dnaStrings: string list) : string =
    let allKMers = generateKMers k
    
    allKMers
    |> List.minBy (fun kmer ->
        dnaStrings
        |> List.sumBy (fun dnaString ->
            let motifs = 
                dnaString
                |> Seq.windowed k
                |> Seq.map (fun window -> System.String.Join("", window))
                |> Seq.toList
            
            motifs
            |> List.minBy (fun motif -> hammingDistance kmer motif)
            |> hammingDistance kmer
        )
    )

// Alternative approach: more direct implementation
let findMedianStringDirect (k: int) (dnaStrings: string list) : string =
    let allKMers = generateKMers k
    
    let totalDistance (kmer: string) : int =
        dnaStrings
        |> List.sumBy (fun dnaString ->
            let motifs = 
                dnaString
                |> Seq.windowed k
                |> Seq.map (fun window -> System.String.Join("", window))
                |> Seq.toList
            
            motifs
            |> List.map (fun motif -> hammingDistance kmer motif)
            |> List.min
        )
    
    allKMers
    |> List.minBy totalDistance

// Helper function to find median string for each DNA string
let findMedianStringForAll (k: int) (dnaStrings: string list) : string =
    let allKMers = generateKMers k
    
    let calculateTotalDistance (kmer: string) : int =
        dnaStrings
        |> List.sumBy (fun dnaString ->
            let kmersInString = 
                dnaString
                |> Seq.windowed k
                |> Seq.map (fun window -> System.String.Join("", window))
                |> Seq.toList
            
            kmersInString
            |> List.minBy (fun motif -> hammingDistance kmer motif)
            |> hammingDistance kmer
        )
    
    allKMers
    |> List.minBy calculateTotalDistance

// Main solution function
let solveMedianString (k: int) (dnaStrings: string list) : string =
    findMedianStringForAll k dnaStrings

// Example usage
let exampleDNAStrings = [
    "AAATTGACGCAT"
    "GACGACCAGGTT"
    "GGATATTTCGCT"
    "GTCTTCAGCGAC"
    "GTTTTAGAGGAC"
]

// Test the solution
let result = solveMedianString 3 exampleDNAStrings
printfn "Median string: %s" result
```

## Explanation

The solution works as follows:

1. **hammingDistance**: Calculates the Hamming distance between two strings by counting differing positions.

2. **generateKMers**: Recursively generates all possible k-mers of a given length using the DNA alphabet {A, C, G, T}.

3. **findMedianStringForAll**: 
   - For each k-mer, calculates the total Hamming distance to all DNA strings
   - For each DNA string, finds the k-mer within it that has minimum distance to our current k-mer
   - Sums up all these minimum distances
   - Returns the k-mer with the overall minimum total distance

4. **solveMedianString**: The main function that calls the median string finding algorithm.

## Time Complexity
- Generating all k-mers: O(4^k)
- For each k-mer, calculating distances to all strings: O(n × m × k) where n is number of strings, m is average string length
- Overall: O(4^k × n × m × k)

## Space Complexity
O(4^k) for storing all k-mers

The algorithm finds the k-mer that minimizes the sum of Hamming distances to all input DNA strings, which is the definition of a median string.

