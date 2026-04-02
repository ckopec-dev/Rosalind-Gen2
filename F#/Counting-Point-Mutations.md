# Rosalind Problem: Counting Point Mutations (F# Solution)

## Problem Description
Given two DNA strings of equal length, count the number of positions where the nucleotides differ (Hamming distance).

## Solution

```fsharp
let countPointMutations (strand1: string) (strand2: string) : int =
    strand1
    |> Seq.zip strand2
    |> Seq.filter (fun (nuc1, nuc2) -> nuc1 <> nuc2)
    |> Seq.length

// Alternative implementation using List.zipWith
let countPointMutations2 (strand1: string) (strand2: string) : int =
    List.zip (strand1 |> Seq.toList) (strand2 |> Seq.toList)
    |> List.filter (fun (nuc1, nuc2) -> nuc1 <> nuc2)
    |> List.length

// More explicit version
let countPointMutations3 (strand1: string) (strand2: string) : int =
    let rec countMutations acc i =
        if i >= strand1.Length then acc
        else
            let mut = if strand1.[i] <> strand2.[i] then 1 else 0
            countMutations (acc + mut) (i + 1)
    countMutations 0 0
```

## Example Usage

```fsharp
// Example from Rosalind
let dna1 = "GAGCCTACTAACGGGAT"
let dna2 = "CATCGGTATCGAACCGG"

let result = countPointMutations dna1 dna2
printfn "Hamming distance: %d" result  // Output: 7

// Test with other examples
let test1 = countPointMutations "ABC" "ABC"  // Should be 0
let test2 = countPointMutations "ABC" "DEF"  // Should be 3
let test3 = countPointMutations "AGT" "AGA"  // Should be 1
```

## Explanation

The solution works by:

1. **Using `Seq.zip`**: Pairs up corresponding characters from both DNA strands
2. **Filtering differences**: Keeps only pairs where the nucleotides are different (`<>`)
3. **Counting**: Returns the length of the filtered sequence

**Time Complexity**: O(n) where n is the length of the DNA strands
**Space Complexity**: O(n) for creating the zipped sequence

## Input/Output Format

**Input**: Two DNA strings of equal length, each on a separate line
**Output**: Single integer representing the Hamming distance

## Sample Input
```
GAGCCTACTAACGGGAT
CATCGGTATCGAACCGG
```

## Sample Output
```
7
```

This solution is concise, functional, and leverages F#'s powerful sequence operations to elegantly solve the point mutation counting problem.

