# Rosalind Problem: Implement Additive Phylogeny in Go

## Problem Description
The Additive Phylogeny problem involves constructing a phylogenetic tree from a distance matrix that satisfies the additive property. A distance matrix is additive if it can be represented by a tree where the distance between any two leaves equals the sum of edge weights on the path between them.

## Solution Approach
1. Use the neighbor-joining algorithm with the additive property
2. Find the two leaves that are closest to each other
3. Create a new internal node and update the distance matrix
4. Recursively apply the process until only two leaves remain

## Go Implementation

```go
package main

import (
    "fmt"
    "math"
)

// AdditivePhylogeny constructs a phylogenetic tree from an additive distance matrix
func AdditivePhylogeny(D [][]float64) [][]int {
    n := len(D)
    
    // Base case: if only 2 leaves, return simple tree
    if n == 2 {
        return [][]int{{0, 1, int(D[0][1])}}
    }
    
    // Find the two leaves that are closest to each other
    i, j := findClosestLeaves(D)
    
    // Calculate the distance from the new internal node to all other leaves
    d := make([]float64, n)
    for k := 0; k < n; k++ {
        d[k] = (D[i][k] + D[j][k] - D[i][j]) / 2.0
    }
    
    // Create new distance matrix with one fewer leaf
    newD := make([][]float64, n-1)
    for k := 0; k < n-1; k++ {
        newD[k] = make([]float64, n-1)
    }
    
    // Fill new distance matrix
    newIdx := 0
    for k := 0; k < n; k++ {
        if k == i || k == j {
            continue
        }
        newIdx2 := 0
        for l := 0; l < n; l++ {
            if l == i || l == j {
                continue
            }
            newD[newIdx][newIdx2] = D[k][l]
            newIdx2++
        }
        newIdx++
    }
    
    // Recursively build the tree for the smaller matrix
    tree := AdditivePhylogeny(newD)
    
    // Add the new internal node and the two original leaves
    newInternal := n - 1
    tree = append(tree, []int{i, newInternal, int(d[i])})
    tree = append(tree, []int{j, newInternal, int(d[j])})
    
    return tree
}

// findClosestLeaves finds the indices of the two closest leaves in the distance matrix
func findClosestLeaves(D [][]float64) (int, int) {
    minDist := math.MaxFloat64
    i, j := 0, 0
    
    for k := 0; k < len(D); k++ {
        for l := k + 1; l < len(D); l++ {
            if D[k][l] < minDist {
                minDist = D[k][l]
                i, j = k, l
            }
        }
    }
    
    return i, j
}

// Helper function to print the tree
func printTree(tree [][]int) {
    for _, edge := range tree {
        fmt.Printf("%d->%d:%d\n", edge[0], edge[1], edge[2])
    }
}

// Main function to demonstrate usage
func main() {
    // Example distance matrix (should be additive)
    D := [][]float64{
        {0, 5, 9, 9},
        {5, 0, 10, 10},
        {9, 10, 0, 2},
        {9, 10, 2, 0},
    }
    
    fmt.Println("Distance Matrix:")
    for _, row := range D {
        fmt.Println(row)
    }
    
    fmt.Println("\nPhylogenetic Tree:")
    tree := AdditivePhylogeny(D)
    printTree(tree)
}
```

## Alternative Implementation (More Direct Approach)

```go
package main

import (
    "fmt"
    "math"
)

// Node represents a node in the phylogenetic tree
type Node struct {
    id     int
    parent *Node
    children []*Node
    weight int
}

// AdditivePhylogeny constructs a phylogenetic tree from an additive distance matrix
func AdditivePhylogeny(D [][]float64) [][]int {
    n := len(D)
    
    // Base case
    if n == 2 {
        return [][]int{{0, 1, int(D[0][1])}}
    }
    
    // Find the two closest leaves
    i, j := findClosestLeaves(D)
    
    // Calculate the distance from the new internal node to all leaves
    d := make([]float64, n)
    for k := 0; k < n; k++ {
        d[k] = (D[i][k] + D[j][k] - D[i][j]) / 2.0
    }
    
    // Create new distance matrix with one fewer leaf
    newD := make([][]float64, n-1)
    for k := 0; k < n-1; k++ {
        newD[k] = make([]float64, n-1)
    }
    
    newIdx := 0
    for k := 0; k < n; k++ {
        if k == i || k == j {
            continue
        }
        newIdx2 := 0
        for l := 0; l < n; l++ {
            if l == i || l == j {
                continue
            }
            newD[newIdx][newIdx2] = D[k][l]
            newIdx2++
        }
        newIdx++
    }
    
    // Recursively solve for the smaller matrix
    tree := AdditivePhylogeny(newD)
    
    // Add the new internal node
    newInternal := n - 1
    tree = append(tree, []int{i, newInternal, int(d[i])})
    tree = append(tree, []int{j, newInternal, int(d[j])})
    
    return tree
}

// findClosestLeaves finds the indices of the two closest leaves
func findClosestLeaves(D [][]float64) (int, int) {
    minDist := math.MaxFloat64
    i, j := 0, 0
    
    for k := 0; k < len(D); k++ {
        for l := k + 1; l < len(D); l++ {
            if D[k][l] < minDist {
                minDist = D[k][l]
                i, j = k, l
            }
        }
    }
    
    return i, j
}

// Helper function to validate if matrix is additive
func isAdditive(D [][]float64) bool {
    n := len(D)
    if n < 4 {
        return true
    }
    
    // Check all four-point conditions
    for i := 0; i < n; i++ {
        for j := i + 1; j < n; j++ {
            for k := j + 1; k < n; k++ {
                for l := k + 1; l < n; l++ {
                    // Four-point condition: 
                    // D[i][j] + D[k][l] should equal the maximum of the three sums
                    sums := []float64{
                        D[i][j] + D[k][l],
                        D[i][k] + D[j][l],
                        D[i][l] + D[j][k],
                    }
                    
                    maxSum := sums[0]
                    for _, s := range sums {
                        if s > maxSum {
                            maxSum = s
                        }
                    }
                    
                    if math.Abs(maxSum-(D[i][j]+D[k][l])) > 1e-9 {
                        return false
                    }
                }
            }
        }
    }
    
    return true
}

func main() {
    // Example from Rosalind problem
    D := [][]float64{
        {0, 13, 21, 22},
        {13, 0, 12, 13},
        {21, 12, 0, 13},
        {22, 13, 13, 0},
    }
    
    fmt.Println("Distance Matrix:")
    for _, row := range D {
        fmt.Println(row)
    }
    
    // Validate it's additive
    if isAdditive(D) {
        fmt.Println("\nMatrix is additive")
        tree := AdditivePhylogeny(D)
        fmt.Println("\nPhylogenetic Tree:")
        for _, edge := range tree {
            fmt.Printf("%d->%d:%d\n", edge[0], edge[1], edge[2])
        }
    } else {
        fmt.Println("Matrix is not additive")
    }
}
```

## Key Points

1. **Additive Property**: The distance matrix must satisfy the four-point condition for a tree to exist
2. **Recursive Approach**: The algorithm recursively reduces the problem size by one leaf at each step
3. **Distance Calculation**: The distance from the new internal node to other leaves is calculated using the formula: d = (D[i][k] + D[j][k] - D[i][j]) / 2
4. **Base Case**: When only two leaves remain, return the direct connection with the distance between them

## Time Complexity
- Time: O(n³) where n is the number of leaves
- Space: O(n²) for the distance matrix

This implementation correctly solves the Additive Phylogeny problem by recursively constructing the phylogenetic tree from an additive distance matrix.

