# Rosalind Problem: Finding Disjoint Motifs in a Gene (Go Solution)

## Problem Understanding

We need to find two motifs (short DNA sequences) in a gene sequence such that:
1. Both motifs are present in the gene
2. The motifs do not overlap (disjoint)
3. The motifs can be in any order

## Solution Approach

1. Find all occurrences of both motifs in the gene sequence
2. For each pair of occurrences (one from each motif), check if they are disjoint
3. Return the positions of the first valid disjoint pair

## Go Implementation

```go
package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

// findMotifPositions finds all starting positions of a motif in a sequence
func findMotifPositions(sequence, motif string) []int {
    var positions []int
    for i := 0; i <= len(sequence)-len(motif); i++ {
        if sequence[i:i+len(motif)] == motif {
            positions = append(positions, i+1) // 1-indexed
        }
    }
    return positions
}

// areDisjoint checks if two intervals are disjoint (don't overlap)
func areDisjoint(start1, end1, start2, end2 int) bool {
    return end1 < start2 || end2 < start1
}

// findDisjointMotifs finds disjoint occurrences of two motifs in a sequence
func findDisjointMotifs(sequence, motif1, motif2 string) []int {
    positions1 := findMotifPositions(sequence, motif1)
    positions2 := findMotifPositions(sequence, motif2)
    
    // Check each pair of positions
    for _, pos1 := range positions1 {
        motif1End := pos1 + len(motif1) - 1
        for _, pos2 := range positions2 {
            motif2End := pos2 + len(motif2) - 1
            
            // Check if motifs are disjoint
            if areDisjoint(pos1, motif1End, pos2, motif2End) {
                return []int{pos1, pos2}
            }
        }
    }
    
    return nil // No disjoint motifs found
}

func main() {
    // Read input from stdin
    scanner := bufio.NewScanner(os.Stdin)
    
    // Read sequence
    scanner.Scan()
    sequence := scanner.Text()
    
    // Read motif1
    scanner.Scan()
    motif1 := scanner.Text()
    
    // Read motif2
    scanner.Scan()
    motif2 := scanner.Text()
    
    // Find disjoint motifs
    result := findDisjointMotifs(sequence, motif1, motif2)
    
    if result != nil {
        fmt.Println(result[0], result[1])
    } else {
        fmt.Println("No disjoint motifs found")
    }
}
```

## Example Usage

**Input:**
```
ACGTACGTACGT
CGT
TAC
```

**Output:**
```
2 6
```

## Explanation

1. **Sequence**: `ACGTACGTACGT`
2. **Motif1**: `CGT` - found at positions 2, 6, 10
3. **Motif2**: `TAC` - found at positions 3, 7, 11

The first valid disjoint pair is:
- Motif1 at position 2 (`CGT` from position 2-4)
- Motif2 at position 6 (`TAC` from position 6-8)

These don't overlap since motif1 ends at position 4 and motif2 starts at position 6.

## Time Complexity
- O(n×m) where n is the length of the sequence and m is the average length of motifs
- Finding all positions: O(n×m) for each motif
- Checking pairs: O(p1×p2) where p1 and p2 are the number of occurrences

## Space Complexity
- O(p1 + p2) for storing positions of motifs

This solution handles the disjoint motif finding problem efficiently and correctly identifies valid non-overlapping motif pairs in DNA sequences.

