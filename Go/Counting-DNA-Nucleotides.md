# Rosalind Problem: Counting DNA Nucleotides

## Problem Statement
Given: A DNA string s of length at most 1000 nt.
Return: Four integers (separated by spaces) counting the respective number of times that the symbols 'A', 'C', 'G', and 'T' occur in s.

## Solution in Go

```go
package main

import (
    "fmt"
    "io/ioutil"
    "strings"
)

func countDNA nucleotides(dna string) (int, int, int, int) {
    var countA, countC, countG, countT int
    
    for _, nucleotide := range dna {
        switch nucleotide {
        case 'A':
            countA++
        case 'C':
            countC++
        case 'G':
            countG++
        case 'T':
            countT++
        }
    }
    
    return countA, countC, countG, countT
}

func main() {
    // Read input from file (assuming input.txt contains the DNA string)
    data, err := ioutil.ReadFile("input.txt")
    if err != nil {
        fmt.Println("Error reading file:", err)
        return
    }
    
    dna := strings.TrimSpace(string(data))
    
    // Count nucleotides
    countA, countC, countG, countT := countDNA nucleotides(dna)
    
    // Print result
    fmt.Printf("%d %d %d %d\n", countA, countC, countG, countT)
}
```

## Alternative Solution (More Efficient for Large Inputs)

```go
package main

import (
    "fmt"
    "io/ioutil"
    "strings"
)

func countDNA nucleotides(dna string) (int, int, int, int) {
    // Pre-allocate counters for efficiency
    counters := make([]int, 256) // ASCII table size
    
    for _, nucleotide := range dna {
        counters[nucleotide]++
    }
    
    return counters['A'], counters['C'], counters['G'], counters['T']
}

func main() {
    // Read input from file
    data, err := ioutil.ReadFile("input.txt")
    if err != nil {
        fmt.Println("Error reading file:", err)
        return
    }
    
    dna := strings.TrimSpace(string(data))
    
    // Count nucleotides
    countA, countC, countG, countT := countDNA nucleotides(dna)
    
    // Print result
    fmt.Printf("%d %d %d %d\n", countA, countC, countG, countT)
}
```

## Example Usage

**Input file (input.txt):**
```
AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
```

**Output:**
```
20 12 17 21
```

## Explanation

1. **Function `countDNA nucleotides`**: Takes a DNA string and counts occurrences of each nucleotide (A, C, G, T)
2. **Switch statement**: Iterates through each character in the DNA string and increments the appropriate counter
3. **Return values**: Returns four integers representing counts of A, C, G, and T respectively
4. **File handling**: Reads DNA string from input file and processes it
5. **Output formatting**: Prints counts separated by spaces as required

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the DNA string
- **Space Complexity**: O(1) - only using a constant amount of extra space for counters

The solution handles the constraints of the problem (DNA string length ≤ 1000 nt) efficiently and correctly.

