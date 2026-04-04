# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Description
Given: A positive integer n (n≤1000).

Return: The number of internal nodes of any unrooted binary tree having n leaves.

## Solution Approach
In an unrooted binary tree:
- Each internal node has exactly 3 edges
- Each leaf has exactly 1 edge
- The total number of edges in a tree with n leaves is 2n - 2
- Each internal node contributes 3 edges, and each leaf contributes 1 edge
- If we let i be the number of internal nodes, then: 3i + n = 2(2n - 2)
- Solving for i: 3i + n = 4n - 4
- Therefore: 3i = 3n - 4
- So: i = (3n - 4) / 3

Wait, let me reconsider this more carefully.

In an unrooted binary tree with n leaves:
- Each internal node has degree 3
- Each leaf has degree 1  
- Total degree = 2 × (number of edges)
- Number of edges = n - 1 (for any tree with n vertices)
- But in a binary tree, we need to be more precise about the structure

Actually, let's think about it this way:
- In any tree, number of edges = number of vertices - 1
- For an unrooted binary tree with n leaves, if we count internal nodes as vertices, we have (n + i) vertices where i is internal nodes
- Number of edges = (n + i) - 1 = n + i - 1
- But in a binary tree, we can also think of it as: n leaves + i internal nodes = n + i vertices
- The sum of all degrees = 2 × number of edges = 2(n + i - 1)
- Leaves have degree 1, internal nodes have degree 3
- So: n × 1 + i × 3 = 2(n + i - 1)
- n + 3i = 2n + 2i - 2
- 3i - 2i = 2n - n - 2
- i = n - 2

Let me verify this with small examples:
- n = 3: 3 - 2 = 1 internal node ✓
- n = 4: 4 - 2 = 2 internal nodes ✓

## C Implementation

```c
#include <stdio.h>

int main() {
    int n;
    
    // Read input
    scanf("%d", &n);
    
    // Calculate number of internal nodes
    // For an unrooted binary tree with n leaves, number of internal nodes = n - 2
    int internal_nodes = n - 2;
    
    // Output result
    printf("%d\n", internal_nodes);
    
    return 0;
}
```

## Explanation

The key insight is that for any unrooted binary tree:
- The number of internal nodes = number of leaves - 2
- This formula comes from the relationship between vertices, edges, and degrees in tree structures
- For a binary tree where each internal node has degree 3 and each leaf has degree 1, this relationship holds

## Sample Input/Output
```
Input: 4
Output: 2

Input: 10
Output: 8
```

