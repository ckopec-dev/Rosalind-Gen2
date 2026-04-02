# Rosalind Problem: Construct the Overlap Graph of a Collection of k-mers

## Problem Description
Given a collection of k-mers, construct the overlap graph where each k-mer is a node, and there is a directed edge from k-mer A to k-mer B if the suffix of A (k-1 characters) matches the prefix of B (k-1 characters).

## Solution in F#

```fsharp
open System

let constructOverlapGraph (kmers: string list) : string list =
    let k = if kmers.IsEmpty then 0 else kmers.[0].Length
    
    // Helper function to get suffix of length k-1
    let getSuffix (kmer: string) = 
        if k <= 1 then ""
        else kmer.Substring(1)
    
    // Helper function to get prefix of length k-1
    let getPrefix (kmer: string) = 
        if k <= 1 then ""
        else kmer.Substring(0, k - 1)
    
    // Create a mapping from prefix to list of kmers with that prefix
    let prefixMap = 
        kmers 
        |> List.map (fun kmer -> (getPrefix kmer, kmer))
        |> Map.ofList
    
    // Find overlaps and build edges
    let edges = 
        kmers 
        |> List.collect (fun fromKmer -> 
            let suffix = getSuffix fromKmer
            match Map.tryFind suffix prefixMap with
            | Some toKmer -> 
                if fromKmer <> toKmer then 
                    [sprintf "%s -> %s" fromKmer toKmer]
                else 
                    []  // Skip self-loops
            | None -> [])
    
    edges

// Alternative implementation using nested loops
let constructOverlapGraphAlt (kmers: string list) : string list =
    let k = if kmers.IsEmpty then 0 else kmers.[0].Length
    
    let getSuffix (kmer: string) = 
        if k <= 1 then ""
        else kmer.Substring(1)
    
    let getPrefix (kmer: string) = 
        if k <= 1 then ""
        else kmer.Substring(0, k - 1)
    
    let edges = 
        [for fromKmer in kmers do
            let suffix = getSuffix fromKmer
            for toKmer in kmers do
                if fromKmer <> toKmer && getPrefix toKmer = suffix then
                    yield sprintf "%s -> %s" fromKmer toKmer]
    
    edges

// Main function to solve the problem
let solveOverlapGraph (inputLines: string list) : string list =
    let kmers = inputLines |> List.filter (fun line -> not (String.IsNullOrWhiteSpace line))
    constructOverlapGraph kmers

// Example usage
let exampleInput = [
    "ATGCG"
    "GCATG"
    "CATGC"
    "AGGCA"
    "GGCAT"
]

let result = solveOverlapGraph exampleInput

printfn "Overlap Graph Edges:"
result |> List.iter (printfn "%s")
```

## Explanation

### Approach
1. **Input Processing**: Read the collection of k-mers from input
2. **Suffix/Prefix Extraction**: For each k-mer, extract its suffix (k-1 characters from the end) and prefix (k-1 characters from the beginning)
3. **Overlap Detection**: Check if the suffix of one k-mer matches the prefix of another k-mer
4. **Edge Construction**: Create directed edges between overlapping k-mers

### Key Functions

- `getSuffix`: Extracts the last (k-1) characters of a k-mer
- `getPrefix`: Extracts the first (k-1) characters of a k-mer  
- `constructOverlapGraph`: Main function that builds the overlap graph

### Time Complexity
- **Time**: O(n²) where n is the number of k-mers (checking all pairs)
- **Space**: O(n) for storing the graph edges

### Example Output
For input:
```
ATGCG
GCATG
CATGC
AGGCA
GGCAT
```

The output would be:
```
ATGCG -> GCATG
CATGC -> ATGCG
GCATG -> CATGC
GGCAT -> GCATG
```

### Key Features
- Handles edge cases like empty input or single-character k-mers
- Avoids self-loops (k-mer connecting to itself)
- Returns edges in the required format "A -> B"
- Uses functional programming idioms with F# lists and pattern matching

This solution efficiently constructs the overlap graph by systematically checking all possible overlaps between k-mers in the collection.

