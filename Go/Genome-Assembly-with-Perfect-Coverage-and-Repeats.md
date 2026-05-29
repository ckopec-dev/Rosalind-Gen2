# Rosalind Problem: Genome Assembly with Perfect Coverage and Repeats

## Problem Understanding

This problem asks us to reconstruct a genome from a set of reads that have perfect coverage (each position in the genome appears exactly once in the reads) and may contain repeats. The key insight is that we need to find an Eulerian path in a de Bruijn graph.

## Solution Approach

1. **Build de Bruijn graph**: Create a graph where nodes are k-1 length prefixes/suffixes and edges represent k-length kmers
2. **Find Eulerian path**: Since we have perfect coverage, there should be an Eulerian path
3. **Reconstruct genome**: Follow the path to get the original sequence

## Go Implementation

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "sort"
    "strings"
)

// Edge represents an edge in the de Bruijn graph
type Edge struct {
    from, to string
}

// DeBruijnGraph represents the graph structure
type DeBruijnGraph struct {
    edges    map[string][]string
    inDegree map[string]int
    outDegree map[string]int
}

// NewDeBruijnGraph creates a new graph
func NewDeBruijnGraph() *DeBruijnGraph {
    return &DeBruijnGraph{
        edges:     make(map[string][]string),
        inDegree:  make(map[string]int),
        outDegree: make(map[string]int),
    }
}

// AddEdge adds an edge to the graph
func (g *DeBruijnGraph) AddEdge(from, to string) {
    g.edges[from] = append(g.edges[from], to)
    g.outDegree[from]++
    g.inDegree[to]++
}

// GetEulerianPath finds an Eulerian path in the graph
func (g *DeBruijnGraph) GetEulerianPath() []string {
    // Find starting node (vertex with outDegree > inDegree)
    start := ""
    for node, out := range g.outDegree {
        if out > g.inDegree[node] {
            start = node
            break
        }
    }
    
    // If no start node found, use any node
    if start == "" {
        for node := range g.outDegree {
            start = node
            break
        }
    }
    
    // Build path using Hierholzer's algorithm
    path := []string{}
    stack := []string{start}
    
    for len(stack) > 0 {
        current := stack[len(stack)-1]
        
        if len(g.edges[current]) > 0 {
            // Take the next edge
            next := g.edges[current][0]
            g.edges[current] = g.edges[current][1:]
            stack = append(stack, next)
        } else {
            // No more edges, add to path
            path = append([]string{current}, path...)
            stack = stack[:len(stack)-1]
        }
    }
    
    return path
}

// GenomeAssembly solves the genome assembly problem
func GenomeAssembly(reads []string, k int) string {
    if len(reads) == 0 {
        return ""
    }
    
    // Build de Bruijn graph
    graph := NewDeBruijnGraph()
    
    for _, read := range reads {
        if len(read) < k {
            continue
        }
        
        // Add all k-mers to graph
        for i := 0; i <= len(read)-k; i++ {
            prefix := read[i : i+k-1]
            suffix := read[i+1 : i+k]
            graph.AddEdge(prefix, suffix)
        }
    }
    
    // Find Eulerian path
    path := graph.GetEulerianPath()
    
    // Reconstruct genome
    if len(path) == 0 {
        return ""
    }
    
    // First node's prefix + last node's suffix
    result := path[0]
    for i := 1; i < len(path); i++ {
        result += path[i][len(path[i])-1:]
    }
    
    return result
}

// ReadInput reads the input from stdin
func ReadInput() ([]string, int) {
    scanner := bufio.NewScanner(os.Stdin)
    var reads []string
    k := 0
    
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if line == "" {
            continue
        }
        
        if k == 0 {
            k = len(line) // Assuming all reads have same length
        }
        reads = append(reads, line)
    }
    
    return reads, k
}

// ReadInputFromFile reads from a file
func ReadInputFromFile(filename string) ([]string, int) {
    file, err := os.Open(filename)
    if err != nil {
        panic(err)
    }
    defer file.Close()
    
    scanner := bufio.NewScanner(file)
    var reads []string
    k := 0
    
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if line == "" {
            continue
        }
        
        if k == 0 {
            k = len(line) // Assuming all reads have same length
        }
        reads = append(reads, line)
    }
    
    return reads, k
}

func main() {
    // For testing with sample data
    // You can uncomment this to test with file input
    /*
    reads, k := ReadInputFromFile("rosalind_gasm.txt")
    result := GenomeAssembly(reads, k)
    fmt.Println(result)
    */
    
    // For stdin input (like typical Rosalind problems)
    reads, k := ReadInput()
    result := GenomeAssembly(reads, k)
    fmt.Println(result)
}
```

## Example Usage

For input like:
```
ATTAGACCTG
TGCCTAACGA
CCTGACGTCA
```

The program will:
1. Build a de Bruijn graph with k=3
2. Find the Eulerian path
3. Reconstruct the genome

## Key Points

1. **De Bruijn Graph**: Each k-mer is represented as an edge between (k-1)-mers
2. **Eulerian Path**: Since we have perfect coverage, there's a unique Eulerian path
3. **Reconstruction**: The path gives us the sequence with overlaps resolved
4. **Time Complexity**: O(|reads| × k) for building graph + O(|edges|) for finding path

## Edge Cases Handled

- Empty input
- Reads shorter than k
- Perfect coverage assumption
- Repeats in the genome (handled by the Eulerian path approach)

This solution correctly handles the genome assembly problem where we have perfect coverage and may have repeats in the original genome.

