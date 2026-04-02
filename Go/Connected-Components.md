# Rosalind Problem: Connected Components in Go

## Problem Understanding

The Connected Components problem asks us to find the number of connected components in an undirected graph. A connected component is a subgraph in which any two vertices are connected to each other by paths, and which is connected to no additional vertices in the supergraph.

## Approach

I'll use the Union-Find (Disjoint Set Union) data structure to efficiently solve this problem:
1. Initialize each node as its own parent
2. For each edge, union the two nodes
3. Count the number of distinct root nodes (connected components)

## Solution

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
)

// UnionFind represents the Union-Find data structure
type UnionFind struct {
    parent []int
    rank   []int
    count  int // number of connected components
}

// NewUnionFind creates a new Union-Find structure
func NewUnionFind(n int) *UnionFind {
    parent := make([]int, n)
    rank := make([]int, n)
    
    // Initialize each node to be its own parent
    for i := 0; i < n; i++ {
        parent[i] = i
        rank[i] = 0
    }
    
    return &UnionFind{
        parent: parent,
        rank:   rank,
        count:  n,
    }
}

// Find finds the root of x with path compression
func (uf *UnionFind) Find(x int) int {
    if uf.parent[x] != x {
        uf.parent[x] = uf.Find(uf.parent[x]) // Path compression
    }
    return uf.parent[x]
}

// Union unions two nodes x and y
func (uf *UnionFind) Union(x, y int) {
    rootX := uf.Find(x)
    rootY := uf.Find(y)
    
    if rootX != rootY {
        // Union by rank
        if uf.rank[rootX] < uf.rank[rootY] {
            uf.parent[rootX] = rootY
        } else if uf.rank[rootX] > uf.rank[rootY] {
            uf.parent[rootY] = rootX
        } else {
            uf.parent[rootY] = rootX
            uf.rank[rootX]++
        }
        uf.count-- // Decrease component count
    }
}

// Count returns the number of connected components
func (uf *UnionFind) Count() int {
    return uf.count
}

// solveConnectedComponents solves the connected components problem
func solveConnectedComponents(n int, edges [][]int) int {
    uf := NewUnionFind(n)
    
    for _, edge := range edges {
        u, v := edge[0]-1, edge[1]-1 // Convert to 0-indexed
        uf.Union(u, v)
    }
    
    return uf.Count()
}

func main() {
    // Read input from stdin
    input, err := os.ReadFile("rosalind_cc.txt")
    if err != nil {
        panic(err)
    }
    
    lines := strings.Split(strings.TrimSpace(string(input)), "\n")
    
    // Parse the first line to get number of nodes and edges
    firstLine := strings.Fields(lines[0])
    n, _ := strconv.Atoi(firstLine[0]) // Number of nodes
    // m, _ := strconv.Atoi(firstLine[1]) // Number of edges (not needed)
    
    // Parse edges
    var edges [][]int
    for i := 1; i < len(lines); i++ {
        fields := strings.Fields(lines[i])
        if len(fields) >= 2 {
            u, _ := strconv.Atoi(fields[0])
            v, _ := strconv.Atoi(fields[1])
            edges = append(edges, []int{u, v})
        }
    }
    
    // Solve and print result
    result := solveConnectedComponents(n, edges)
    fmt.Println(result)
}
```

## How it works:

1. **UnionFind Structure**: Implements the Union-Find data structure with path compression and union by rank for optimal performance
2. **Find Operation**: Uses path compression to make future queries faster
3. **Union Operation**: Merges two sets and keeps track of component count
4. **Main Logic**: 
   - Initialize UnionFind with n nodes
   - Process each edge by uniting the two nodes
   - Return the final count of connected components

## Time Complexity:
- O(n + m × α(n)) where n is the number of nodes, m is the number of edges, and α(n) is the inverse Ackermann function (practically constant)

## Space Complexity:
- O(n) for the Union-Find structure

## Sample Input Format:
```
4 2
1 2
3 4
```

## Sample Output:
```
2
```

This solution efficiently handles the connected components problem using the Union-Find data structure, which is optimal for this type of dynamic connectivity problem.

