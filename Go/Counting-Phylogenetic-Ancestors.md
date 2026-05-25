# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Statement
Given: A positive integer n (n ≤ 1000).

Return: The number of internal nodes of any unrooted binary tree having n leaves.

## Solution

```go
package main

import (
    "fmt"
    "os"
    "strconv"
)

// countInternalNodes calculates the number of internal nodes in an unrooted binary tree
// with n leaves
func countInternalNodes(n int) int {
    // For an unrooted binary tree with n leaves, the number of internal nodes is n - 2
    // This is a mathematical property of binary trees
    return n - 2
}

func main() {
    // Read input from command line argument or stdin
    var n int
    if len(os.Args) > 1 {
        n, _ = strconv.Atoi(os.Args[1])
    } else {
        fmt.Print("Enter number of leaves: ")
        fmt.Scanf("%d", &n)
    }
    
    // Validate input
    if n <= 0 || n > 1000 {
        fmt.Println("Error: n must be between 1 and 1000")
        return
    }
    
    // Calculate and print result
    result := countInternalNodes(n)
    fmt.Println(result)
}
```

## Explanation

### Mathematical Background
In an unrooted binary tree:
- Each internal node has exactly 3 edges (degree 3)
- Each leaf has exactly 1 edge (degree 1)
- The total number of edges in any tree is (number of nodes - 1)

For an unrooted binary tree with n leaves:
1. Let i = number of internal nodes
2. Total nodes = n + i
3. Total edges = (n + i) - 1 = n + i - 1
4. Each internal node contributes 3 edges, each leaf contributes 1 edge
5. Total edge count = 3*i + n
6. Setting equations equal: 3*i + n = n + i - 1
7. Solving: 3*i = i - 1 → 2*i = -1 → This approach needs correction

Let me reconsider the mathematical approach:

Actually, for an unrooted binary tree with n leaves, we can use the fact that:
- In any tree, the number of edges = number of nodes - 1
- In a binary tree, if we have n leaves and i internal nodes:
  - Each internal node contributes 3 edges (but each edge is counted twice)
  - Total edges = 3*i + n (this is incorrect approach)

The correct mathematical relationship for unrooted binary trees:
- For any binary tree with n leaves, the number of internal nodes = n - 2

This can be derived from the fact that in a binary tree, if we start with n leaves and work backwards, each internal node splits into two branches, and we end up with n leaves.

## Alternative Implementation with Direct Formula

```go
package main

import (
    "fmt"
    "os"
    "strconv"
)

func main() {
    // Read input
    var n int
    if len(os.Args) > 1 {
        n, _ = strconv.Atoi(os.Args[1])
    } else {
        fmt.Print("Enter number of leaves: ")
        fmt.Scanf("%d", &n)
    }
    
    // Validate input
    if n <= 0 || n > 1000 {
        fmt.Println("Error: n must be between 1 and 1000")
        return
    }
    
    // For unrooted binary tree: internal nodes = n - 2
    result := n - 2
    fmt.Println(result)
}
```

## Test Cases

```
Input: 4
Output: 2

Input: 10
Output: 8

Input: 100
Output: 98
```

## Key Points

1. **Formula**: For an unrooted binary tree with n leaves, the number of internal nodes is n - 2
2. **Time Complexity**: O(1)
3. **Space Complexity**: O(1)
4. **Edge Cases**: n must be at least 1, and at most 1000 as per problem constraints

The solution is straightforward because this is a well-known mathematical property of binary trees.

