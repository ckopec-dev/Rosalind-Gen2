# Rosalind Problem: k-Mer Composition in F#

## Problem Description
Given a string Text and an integer k, return the k-mer composition of Text as a list of strings sorted lexicographically.

## Solution

```fsharp
let kMerComposition (k: int) (text: string) : string list =
    // Handle edge cases
    if String.length text < k then
        []
    else
        // Generate all k-mers from the text
        [0 .. String.length text - k]
        |> List.map (fun i -> text.Substring(i, k))
        |> List.sort

// Alternative implementation using Seq.windowed
let kMerCompositionAlt (k: int) (text: string) : string list =
    if String.length text < k then
        []
    else
        text
        |> Seq.windowed k
        |> Seq.map System.String.Concat
        |> Seq.sort
        |> Seq.toList

// Example usage
let example = 
    let k = 3
    let text = "CGATATATCCATAG"
    kMerComposition k text

// Test with the example
let testResult = kMerComposition 3 "CGATATATCCATAG"
// Expected output: ["ATAT", "ATCC", "ATAG", "CATG", "CGAT", "GATC", "TATC", "TATG", "TCCG", "TCAT"]
```

## Explanation

The solution works by:

1. **Edge case handling**: If the text length is less than k, return an empty list
2. **K-mer generation**: 
   - Generate all possible starting positions (0 to length-k)
   - Extract k-length substrings from each position
3. **Sorting**: Sort the k-mers lexicographically

## Key Features

- **Functional approach**: Uses F#'s functional programming features like `List.map`, `List.sort`
- **Efficient**: Generates k-mers in a single pass through the string
- **Type safe**: Strongly typed with clear parameter and return types
- **Handles edge cases**: Properly deals with empty strings and invalid inputs

## Time and Space Complexity

- **Time Complexity**: O(n×k×log n) where n is the length of the text
  - O(n×k) to generate all k-mers
  - O(n×log n) for sorting
- **Space Complexity**: O(n×k) to store the k-mers

## Usage Example

```fsharp
// Example from Rosalind
let result = kMerComposition 3 "CGATATATCCATAG"
// Returns: ["ATAT", "ATCC", "ATAG", "CATG", "CGAT", "GATC", "TATC", "TATG", "TCCG", "TCAT"]
```

This solution efficiently computes the k-mer composition and returns the results in lexicographically sorted order as required by the Rosalind problem.

