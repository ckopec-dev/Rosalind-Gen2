# Rosalind Problem: Implement_RandomizedMotifSearch in F#

## Problem Description
Implement the randomized motif search algorithm to find the best motifs in a set of DNA sequences.

## Solution

```fsharp
open System
open System.Collections.Generic

// Helper functions for DNA operations
let nucleotides = ['A'; 'C'; 'G'; 'T']

let random = Random()

let getRandomIndex max = random.Next(max)

let getNucleotideProbability profile i nucleotide =
    match nucleotide with
    | 'A' -> profile.[0].[i]
    | 'C' -> profile.[1].[i]
    | 'G' -> profile.[2].[i]
    | 'T' -> profile.[3].[i]
    | _ -> 0.0

let getProfileMatrix (motifs: string list) =
    let k = motifs.[0].Length
    let profile = Array2D.zeroCreate 4 k
    
    // Count nucleotides at each position
    for motif in motifs do
        for i = 0 to k - 1 do
            let nucleotide = motif.[i]
            match nucleotide with
            | 'A' -> profile.[0].[i] <- profile.[0].[i] + 1.0
            | 'C' -> profile.[1].[i] <- profile.[1].[i] + 1.0
            | 'G' -> profile.[2].[i] <- profile.[2].[i] + 1.0
            | 'T' -> profile.[3].[i] <- profile.[3].[i] + 1.0
            | _ -> ()
    
    // Convert counts to probabilities
    let motifCount = float motifs.Length
    for i = 0 to 3 do
        for j = 0 to k - 1 do
            profile.[i].[j] <- profile.[i].[j] / motifCount
    
    profile

let getConsensusString profile =
    let k = profile.[0].Length
    let consensus = new System.Text.StringBuilder(k)
    
    for i = 0 to k - 1 do
        let maxProb = ref 0.0
        let maxNucleotide = 'A'
        
        for nucleotide in nucleotides do
            let prob = getNucleotideProbability profile i nucleotide
            if prob > !maxProb then
                maxProb := prob
                maxNucleotide := nucleotide
        
        consensus.Append(maxNucleotide) |> ignore
    
    consensus.ToString()

let getScore motifs =
    let profile = getProfileMatrix motifs
    let consensus = getConsensusString profile
    let k = motifs.[0].Length
    let score = ref 0
    
    for i = 0 to k - 1 do
        let consensusNucleotide = consensus.[i]
        for motif in motifs do
            if motif.[i] <> consensusNucleotide then
                score := !score + 1
    
    !score

let getRandomMotif (dna: string) k =
    let start = getRandomIndex (dna.Length - k + 1)
    dna.Substring(start, k)

let getRandomMotifs (dnaList: string list) k =
    dnaList |> List.map (fun dna -> getRandomMotif dna k)

let getMotifFromProfile (profile: float[,]) (dna: string) k =
    let maxScore = ref -1.0
    let bestMotif = ""
    
    for i = 0 to dna.Length - k do
        let motif = dna.Substring(i, k)
        let score = ref 1.0
        
        for j = 0 to k - 1 do
            let nucleotide = motif.[j]
            score := !score * getNucleotideProbability profile j nucleotide
        
        if !score > !maxScore then
            maxScore := !score
            bestMotif <- motif
    
    bestMotif

let getMotifsFromProfile (profile: float[,]) (dnaList: string list) k =
    dnaList |> List.map (fun dna -> getMotifFromProfile profile dna k)

let randomizedMotifSearch (dnaList: string list) k iterations =
    let bestMotifs = getRandomMotifs dnaList k
    let bestScore = getScore bestMotifs
    
    let rec search iterations =
        if iterations <= 0 then
            bestMotifs
        else
            let motifs = getRandomMotifs dnaList k
            let profile = getProfileMatrix motifs
            let newMotifs = getMotifsFromProfile profile dnaList k
            let newScore = getScore newMotifs
            
            if newScore < bestScore then
                search (iterations - 1)
            else
                search (iterations - 1)
    
    search iterations

// Main function to solve the problem
let solveRandomizedMotifSearch (dnaList: string list) k iterations =
    let bestMotifs = getRandomMotifs dnaList k
    let bestScore = getScore bestMotifs
    
    let rec search iterations currentMotifs currentScore =
        if iterations <= 0 then
            currentMotifs
        else
            let motifs = getRandomMotifs dnaList k
            let profile = getProfileMatrix motifs
            let newMotifs = getMotifsFromProfile profile dnaList k
            let newScore = getScore newMotifs
            
            if newScore < currentScore then
                search (iterations - 1) newMotifs newScore
            else
                search (iterations - 1) currentMotifs currentScore
    
    search iterations bestMotifs bestScore

// Alternative cleaner implementation
let randomizedMotifSearchClean (dnaList: string list) k iterations =
    let rec run iterations bestMotifs =
        if iterations <= 0 then
            bestMotifs
        else
            // Generate random motifs
            let motifs = getRandomMotifs dnaList k
            let profile = getProfileMatrix motifs
            let newMotifs = getMotifsFromProfile profile dnaList k
            let newScore = getScore newMotifs
            
            // Compare with best score
            let bestScore = getScore bestMotifs
            
            if newScore < bestScore then
                run (iterations - 1) newMotifs
            else
                run (iterations - 1) bestMotifs
    
    // Initialize with random motifs
    let initialMotifs = getRandomMotifs dnaList k
    run iterations initialMotifs

// Example usage
let exampleDNA = [
    "ttaccttacaa"
    "ggcgtctggaa"
    "ttgggactgaa"
    "agagaaatgaa"
    "cagagactgaa"
]

let result = randomizedMotifSearchClean exampleDNA 3 1000

// Print results
printfn "Best motifs found:"
for motif in result do
    printfn "%s" motif

let score = getScore result
printfn "Score: %d" score
```

## Explanation

This implementation solves the Randomized Motif Search problem by:

1. **Random Motif Generation**: Starting with random motifs from each DNA sequence
2. **Profile Construction**: Building a profile matrix from current motifs
3. **Motif Selection**: Finding the best motif from each sequence based on the profile
4. **Score Comparison**: Comparing the new score with the best score found so far
5. **Iteration**: Repeating the process for a specified number of iterations

## Key Functions

- `getRandomMotifs`: Generates random motifs of length k from each DNA sequence
- `getProfileMatrix`: Creates a profile matrix from motifs
- `getMotifsFromProfile`: Finds best motifs from each sequence using the profile
- `getScore`: Calculates the score of a set of motifs
- `randomizedMotifSearchClean`: Main algorithm implementation

## Time Complexity
- O(iterations × n × k) where n is the number of DNA sequences and k is the motif length

## Space Complexity
- O(n × k) for storing motifs and profile matrices

The algorithm uses randomization to escape local optima and find better motif sets than deterministic approaches like Greedy Motif Search.

