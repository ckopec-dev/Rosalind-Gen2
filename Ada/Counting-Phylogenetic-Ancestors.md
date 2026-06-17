# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Statement
Given: A positive integer n (n≤1000).

Return: The number of internal nodes of any unrooted binary tree having n leaves.

## Solution Approach

This is a classic problem in phylogenetics. For an unrooted binary tree with n leaves, we need to determine the number of internal nodes.

Let's think about this step by step:
- In any tree, if we have n leaves and k internal nodes, then the total number of nodes is n + k
- In a binary tree, each internal node has exactly 2 children (except for the root in rooted trees)
- For an unrooted binary tree with n leaves, there are exactly n - 2 internal nodes

## Mathematical Analysis

For any unrooted binary tree:
- Each internal node contributes exactly 1 edge to connect to its children
- The total number of edges in a tree with m nodes is m - 1
- With n leaves and k internal nodes: n + k = total nodes
- Total edges = (n + k) - 1 = n + k - 1
- But we also know that each internal node contributes 2 edges, and leaves contribute 0 edges, so:
  - Total edges = 2k (since internal nodes have 2 children each)
  - Therefore: n + k - 1 = 2k
  - Solving: n - 1 = k

Wait, that's not right. Let me reconsider the structure more carefully.

In an unrooted binary tree:
- Each internal node has degree 3 (connected to 3 other nodes)
- Each leaf has degree 1  
- The total number of edges is (n + k - 1) where n + k = total nodes
- Sum of degrees = 2 × edges = 2(n + k - 1)
- But sum of degrees = n×1 + k×3 = n + 3k
- So: n + 3k = 2(n + k - 1) = 2n + 2k - 2
- Simplifying: 3k - 2k = 2n - n - 2 = n - 2
- Therefore: k = n - 2

## Implementation in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Counting_Phylogenetic_Ancestors is
   n : Integer;
begin
   -- Read input
   Get(n);
   
   -- For an unrooted binary tree with n leaves, 
   -- the number of internal nodes is n - 2
   Put(n - 2);
   New_Line;
end Counting_Phylogenetic_Ancestors;
```

## Explanation

The key insight for this problem is understanding the relationship between leaves and internal nodes in an unrooted binary tree:

1. **Base case**: A tree with 2 leaves has 0 internal nodes (just a single edge)
2. **Inductive step**: Each time we add a leaf by splitting an internal node, we increase the number of leaves by 1 and the number of internal nodes by 1
3. **General formula**: For n leaves, there are exactly n - 2 internal nodes

This can be proven using the fact that in any tree:
- Number of edges = Number of nodes - 1  
- In an unrooted binary tree with n leaves and k internal nodes:
  - Total nodes = n + k
  - Total edges = (n + k) - 1
  - Also, sum of degrees = 2 × edges = 2(n + k - 1)
  - Leaves contribute degree 1 each, internal nodes contribute degree 3 each
  - So: n + 3k = 2(n + k - 1)
  - Solving: n + 3k = 2n + 2k - 2 → k = n - 2

## Example
- Input: 4
- Output: 2 (since 4 - 2 = 2 internal nodes)

The solution is simply `n - 2` for any valid input n.