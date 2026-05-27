# Rosalind Problem: Construct the Burrows-Wheeler Transform of a String

## Problem Description
The Burrows-Wheeler Transform (BWT) is a reversible string transformation used in data compression and bioinformatics. Given a string, the BWT is constructed by:
1. Creating all cyclic rotations of the string
2. Sorting the rotations lexicographically
3. Taking the last column of the sorted rotations

## Solution in Go

```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

// burrowsWheelerTransform computes the Burrows-Wheeler Transform of a string
func burrowsWheelerTransform(s string) string {
    // Add sentinel character (typically $) to mark end of string
    s = s + "$"
    
    // Create all cyclic rotations
    rotations := make([]string, len(s))
    for i := 0; i < len(s); i++ {
        rotations[i] = s[i:] + s[:i]
    }
    
    // Sort rotations lexicographically
    sort.Strings(rotations)
    
    // Take the last character of each rotation
    bwt := make([]byte, len(s))
    for i, rotation := range rotations {
        bwt[i] = rotation[len(rotation)-1]
    }
    
    return string(bwt)
}

// Alternative implementation using strings.Builder for better performance
func burrowsWheelerTransformOptimized(s string) string {
    s = s + "$"
    n := len(s)
    
    // Create all rotations
    rotations := make([]string, n)
    for i := 0; i < n; i++ {
        rotations[i] = s[i:] + s[:i]
    }
    
    // Sort rotations
    sort.Strings(rotations)
    
    // Build BWT
    var result strings.Builder
    result.Grow(n)
    for _, rotation := range rotations {
        result.WriteByte(rotation[n-1])
    }
    
    return result.String()
}

func main() {
    // Example from Rosalind problem
    input := "GCGTGCCTGGTCA"
    result := burrowsWheelerTransform(input)
    
    fmt.Printf("Input string: %s\n", input)
    fmt.Printf("BWT: %s\n", result)
    
    // Test with simple example
    simple := "ABCD"
    simpleResult := burrowsWheelerTransform(simple)
    fmt.Printf("\nSimple example:\n")
    fmt.Printf("Input: %s\n", simple)
    fmt.Printf("BWT: %s\n", simpleResult)
    
    // Verify with expected output
    // For "GCGTGCCTGGTCA", the BWT should be "ACTGGCTGCCGTT"
    fmt.Printf("\nVerification:\n")
    fmt.Printf("Expected: ACTGGCTGCCGTT\n")
    fmt.Printf("Actual:   %s\n", result)
}
```

## Step-by-step explanation

1. **Add sentinel character**: Append `$` to the input string to mark the end
2. **Generate rotations**: Create all possible cyclic rotations of the string
3. **Sort rotations**: Sort all rotations in lexicographical order
4. **Extract last column**: Take the last character from each sorted rotation

## Example walkthrough

For input string "GCGTGCCTGGTCA":
1. Add sentinel: "GCGTGCCTGGTCA$"
2. Generate rotations:
   - GCGTGCCTGGTCA$
   - CGTGCCTGGTCA$G
   - GTGCCTGGTCA$GC
   - TGCCTGGTCA$GCG
   - GCCTGGTCA$GCGT
   - CCTGGTCA$GCGTC
   - CTGGTCA$GCGTCC
   - TGGTCA$GCGTCCG
   - GGTCA$GCGTCCGT
   - GTCA$GCGTCCGTG
   - TCA$GCGTCCGTGC
   - CA$GCGTCCGTGCC
   - A$GCGTCCGTGCCG
   - $GCGTCCGTGCCGA
3. Sort rotations lexicographically
4. Extract last column to get BWT

## Time and Space Complexity

- **Time Complexity**: O(n² log n) where n is the length of the string
  - O(n²) for generating all rotations
  - O(n log n) for sorting
- **Space Complexity**: O(n²) for storing all rotations

## Usage

```bash
go run burrows_wheeler.go
```

The program will output the Burrows-Wheeler Transform of the given string, which can be used for bioinformatics applications like sequence alignment and compression.

