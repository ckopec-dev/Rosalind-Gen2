# Rosalind Problem: Reconstruct a String from its Paired Composition

## Problem Understanding

Given a paired composition of a string (k-mers with their corresponding (k-1)-mers), we need to reconstruct the original string. This is a variation of the Eulerian path problem in de Bruijn graphs.

## Solution Approach

1. Build a de Bruijn graph from the paired k-mers
2. Find an Eulerian path in the graph
3. Reconstruct the original string from the path

## Go Implementation

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
)

// Pair represents a paired k-mer
type Pair struct {
    prefix string
    suffix string
}

// Graph represents the de Bruijn graph
type Graph struct {
    adjacency map[string][]string
    inDegree  map[string]int
    outDegree map[string]int
}

// NewGraph creates a new empty graph
func NewGraph() *Graph {
    return &Graph{
        adjacency: make(map[string][]string),
        inDegree:  make(map[string]int),
        outDegree: make(map[string]int),
    }
}

// AddEdge adds an edge to the graph
func (g *Graph) AddEdge(from, to string) {
    g.adjacency[from] = append(g.adjacency[from], to)
    g.outDegree[from]++
    g.inDegree[to]++
}

// FindEulerianPath finds an Eulerian path in the graph
func (g *Graph) FindEulerianPath() []string {
    // Find starting node (node with out-degree - in-degree = 1)
    start := ""
    for node, out := range g.outDegree {
        if out-g.inDegree[node] == 1 {
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
    
    // Hierholzer's algorithm
    path := []string{}
    stack := []string{start}
    
    for len(stack) > 0 {
        current := stack[len(stack)-1]
        if len(g.adjacency[current]) > 0 {
            next := g.adjacency[current][0]
            g.adjacency[current] = g.adjacency[current][1:]
            stack = append(stack, next)
        } else {
            path = append([]string{current}, path...)
            stack = stack[:len(stack)-1]
        }
    }
    
    return path
}

// ReconstructString reconstructs the original string from paired composition
func ReconstructString(pairs []Pair, k int) string {
    // Build de Bruijn graph
    graph := NewGraph()
    
    // Create nodes and edges from paired k-mers
    for _, pair := range pairs {
        // Create prefix and suffix nodes
        prefixNode := pair.prefix[:k-1]
        suffixNode := pair.suffix[:k-1]
        
        // Add edge from prefix node to suffix node
        graph.AddEdge(prefixNode, suffixNode)
    }
    
    // Find Eulerian path
    path := graph.FindEulerianPath()
    
    // Reconstruct string
    if len(path) == 0 {
        return ""
    }
    
    // First node contributes its full prefix
    result := path[0]
    
    // For subsequent nodes, we need to determine the last character
    // of each node to reconstruct the full string
    for i := 1; i < len(path); i++ {
        result += string(path[i][k-2]) // Last character of node
    }
    
    return result
}

// ParseInput parses the input format
func ParseInput(input string) ([]Pair, int) {
    lines := strings.Split(strings.TrimSpace(input), "\n")
    
    // First line contains k and d
    firstLine := strings.Split(lines[0], " ")
    k, _ := strconv.Atoi(firstLine[0])
    d, _ := strconv.Atoi(firstLine[1])
    
    var pairs []Pair
    for i := 1; i < len(lines); i++ {
        line := strings.TrimSpace(lines[i])
        if line == "" {
            continue
        }
        
        // Split the paired k-mer
        parts := strings.Split(line, "|")
        if len(parts) == 2 {
            prefix := strings.TrimSpace(parts[0])
            suffix := strings.TrimSpace(parts[1])
            pairs = append(pairs, Pair{prefix: prefix, suffix: suffix})
        }
    }
    
    return pairs, k
}

// ReconstructFromPairedComposition reconstructs string from paired composition
func ReconstructFromPairedComposition(input string) string {
    pairs, k := ParseInput(input)
    
    // Build graph from paired k-mers
    graph := NewGraph()
    
    for _, pair := range pairs {
        // Create prefix and suffix nodes for k-1 length
        prefixNode := pair.prefix[:k-1]
        suffixNode := pair.suffix[:k-1]
        
        graph.AddEdge(prefixNode, suffixNode)
    }
    
    // Find Eulerian path
    path := graph.FindEulerianPath()
    
    if len(path) == 0 {
        return ""
    }
    
    // Reconstruct the string
    // First node contributes its full prefix
    result := path[0]
    
    // For each subsequent node, add the last character of the node
    for i := 1; i < len(path); i++ {
        result += string(path[i][k-2])
    }
    
    return result
}

func main() {
    // Example input
    input := `4 2
AAC|GTT
ACG|TTC
CCG|TTG
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA
TGC|GAC
GAC|ACT
ACT|CTT
TTG|TTC
TCG|TCT
CGT|TGT
GTT|TGA
TGA|GAA