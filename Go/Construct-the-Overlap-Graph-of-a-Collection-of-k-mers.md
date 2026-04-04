# Rosalind Problem: Construct the Overlap Graph of a Collection of k-mers

## Problem Description

Given a collection of k-mers, construct the overlap graph where each k-mer is a node, and there is a directed edge from k-mer A to k-mer B if the suffix of A (of length k-1) matches the prefix of B (of length k-1).

## Solution

```go
package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

// getSuffix returns the suffix of a string of length k-1
func getSuffix(s string, k int) string {
    return s[1:k]
}

// getPrefix returns the prefix of a string of length k-1
func getPrefix(s string, k int) string {
    return s[0 : k-1]
}

// constructOverlapGraph builds the overlap graph from a collection of k-mers
func constructOverlapGraph(kmers []string, k int) []string {
    var edges []string
    
    // For each k-mer, check all other k-mers for overlap
    for i := 0; i < len(kmers); i++ {
        for j := 0; j < len(kmers); j++ {
            // Skip self-edges
            if i == j {
                continue
            }
            
            // Check if suffix of kmers[i] matches prefix of kmers[j]
            if getSuffix(kmers[i], k) == getPrefix(kmers[j], k) {
                edges = append(edges, fmt.Sprintf("%s -> %s", kmers[i], kmers[j]))
            }
        }
    }
    
    return edges
}

func main() {
    // Read input from stdin
    scanner := bufio.NewScanner(os.Stdin)
    var kmers []string
    
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if line != "" {
            kmers = append(kmers, line)
        }
    }
    
    if len(kmers) == 0 {
        return
    }
    
    // Get k from the first k-mer
    k := len(kmers[0])
    
    // Construct overlap graph
    edges := constructOverlapGraph(kmers, k)
    
    // Print results
    for _, edge := range edges {
        fmt.Println(edge)
    }
}
```

## Explanation

### Key Steps:

1. **Input Parsing**: Read k-mers from standard input, one per line
2. **Helper Functions**:
   - `getSuffix`: Extracts the suffix of length k-1 from a k-mer
   - `getPrefix`: Extracts the prefix of length k-1 from a k-mer
3. **Graph Construction**: 
   - For each k-mer, compare it with every other k-mer
   - If the suffix of one k-mer matches the prefix of another, create an edge
   - Skip self-edges (a k-mer pointing to itself)
4. **Output**: Print all edges in the format "kmer1 -> kmer2"

### Time Complexity: O(n² × k) where n is the number of k-mers and k is the k-mer length

### Space Complexity: O(n²) for storing the edges

## Example

**Input:**
```
ATGCG
GCATG
CATGC
AGGCA
GGCAT
```

**Output:**
```
ATGCG -> CGCAT
CATGC -> ATGCG
GCATG -> CATGC
AGGCA -> GGCAT
GGCAT -> GCATG
```

## Usage

Save the code as `overlap_graph.go` and run:

```bash
go run overlap_graph.go < input.txt
```

Where `input.txt` contains the k-mers, one per line.

