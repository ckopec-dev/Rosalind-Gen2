# Rosalind Problem: Find All Shared k-mers of a Pair of Strings (F# Solution)

## Problem Understanding

Given two DNA strings, we need to find all k-mers that appear in both strings. A k-mer is a substring of length k.

## Solution Approach

1. Generate all k-mers from both strings
2. Find the intersection of these k-mers
3. Return the shared k-mers

## F# Implementation

```fsharp
open System

// Function to generate all k-mers from a string
let getKmers (k: int) (s: string) : string list =
    if s.Length < k then []
    else
        [0 .. s.Length - k]
        |> List.map (fun i -> s.Substring(i, k))

// Function to find all shared k-mers between two strings
let findAllSharedKmers (k: int) (s1: string) (s2: string) : string list =
    let kmers1 = getKmers k s1
    let kmers2 = getKmers k s2
    
    // Find intersection of k-mers
    Set.intersect (Set.ofList kmers1) (Set.ofList kmers2)
    |> Set.toList

// Function to reverse complement a DNA string
let reverseComplement (s: string) : string =
    s.ToUpper()
    |> Seq.map (fun c ->
        match c with
        | 'A' -> 'T'
        | 'T' -> 'A'
        | 'G' -> 'C'
        | 'C' -> 'G'
        | _ -> c)
    |> Seq.rev
    |> System.String.Concat

// Complete solution that handles both original and reverse complement
let findAllSharedKmersComplete (k: int) (s1: string) (s2: string) : string list =
    let kmers1 = getKmers k s1
    let kmers2 = getKmers k s2
    
    // Get reverse complements
    let rc1 = reverseComplement s1
    let rc2 = reverseComplement s2
    
    let rcKmers1 = getKmers k rc1
    let rcKmers2 = getKmers k rc2
    
    // Find all k-mers from both strings and their reverse complements
    let allKmers1 = Set.union (Set.ofList kmers1) (Set.ofList rcKmers1)
    let allKmers2 = Set.union (Set.ofList kmers2) (Set.ofList rcKmers2)
    
    // Find intersection
    Set.intersect allKmers1 allKmers2
    |> Set.toList

// Alternative simpler approach for basic problem
let findSharedKmersSimple (k: int) (s1: string) (s2: string) : string list =
    let kmers1 = getKmers k s1
    let kmers2 = getKmers k s2
    
    kmers1 
    |> List.filter (fun kmer -> kmers2 |> List.contains kmer)
    |> List.distinct

// Example usage
let example () =
    let s1 = "AAACTCATC"
    let s2 = "TTTTGGGAA"
    let k = 3
    
    let result = findSharedKmersSimple k s1 s2
    printfn "Shared k-mers: %A" result
    
    // Test with reverse complement
    let completeResult = findAllSharedKmersComplete k s1 s2
    printfn "Complete shared k-mers: %A" completeResult

// Main function for Rosalind submission
let solveRosalindProblem (k: int) (s1: string) (s2: string) : string list =
    findSharedKmersSimple k s1 s2

// Test with sample data
let testWithSample () =
    let k = 3
    let s1 = "AAACTCATC"
    let s2 = "TTTTGGGAA"
    
    let result = solveRosalindProblem k s1 s2
    printfn "Result: %A" result

// Run test
testWithSample()
```

## Explanation

The solution works as follows:

1. **getKmers**: Generates all k-mers of length k from a given string by sliding a window of size k across the string.

2. **findSharedKmersSimple**: Finds k-mers that appear in both strings by:
   - Generating all k-mers from both strings
   - Filtering k-mers from the first string that also exist in the second string
   - Removing duplicates

3. **reverseComplement**: Helper function to compute the reverse complement of a DNA string, which is important for the complete solution as k-mers can match in either orientation.

4. **findAllSharedKmersComplete**: More comprehensive solution that considers both original and reverse complement k-mers.

## Time Complexity
- O(n₁ × n₂ × k) where n₁ and n₂ are the lengths of the strings and k is the k-mer length
- Space complexity: O(n₁ × n₂ × k)

## Usage Example
For input strings "AAACTCATC" and "TTTTGGGAA" with k=3, the shared k-mers would be those 3-mers that appear in both strings.

The solution handles the basic requirements of finding shared k-mers between two DNA strings, which is the core of the Rosalind problem.

