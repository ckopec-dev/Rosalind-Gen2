# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Statement
Given: A positive integer n (n≤1000).

Return: The number of internal nodes of any unrooted binary tree having n leaves.

## Solution Approach

In an unrooted binary tree:
- Each internal node has exactly 3 edges
- Each leaf has exactly 1 edge
- The total number of edges in any tree is (number of nodes - 1)

For an unrooted binary tree with n leaves:
- Let k be the number of internal nodes
- Total nodes = n + k
- Total edges = (n + k - 1)
- Each internal node contributes 3 edges, each leaf contributes 1 edge
- Total edges = 3k + n

Setting up the equation:
3k + n = 2(n + k - 1)
3k + n = 2n + 2k - 2
3k - k = 2n - n - 2
2k = n - 2
k = (n - 2) / 2

Wait, let me reconsider this more carefully.

Actually, for an unrooted binary tree:
- Each internal node connects to 3 other nodes
- Each leaf connects to 1 other node
- Total degree = 3k + n
- But each edge contributes 2 to the total degree
- So: 2 × (n + k - 1) = 3k + n
- 2n + 2k - 2 = 3k + n
- n - 2 = k

So the number of internal nodes = n - 2

## Haskell Solution

```haskell
-- Counting Phylogenetic Ancestors
-- Problem: Given n leaves, find number of internal nodes in unrooted binary tree

countPhylogeneticAncestors :: Int -> Int
countPhylogeneticAncestors n = n - 2

-- Main function to read input and output result
main :: IO ()
main = do
    input <- getLine
    let n = read input :: Int
    let result = countPhylogeneticAncestors n
    print result
```

## Explanation

The key insight is that for any unrooted binary tree:
1. Every internal node has degree 3
2. Every leaf has degree 1
3. The total number of edges in a tree with N nodes is N-1
4. The sum of all degrees equals 2 × (number of edges)

If we have n leaves and k internal nodes:
- Total nodes = n + k
- Total edges = n + k - 1
- Sum of degrees = 3k + n
- Therefore: 2(n + k - 1) = 3k + n
- Solving: 2n + 2k - 2 = 3k + n
- Simplifying: n - 2 = k

So the number of internal nodes is always n - 2.

## Example
- Input: 4
- Output: 2
- An unrooted binary tree with 4 leaves has 2 internal nodes

## Test Cases
- n = 1 → 1 - 2 = -1 (not valid - minimum is 3 leaves for a binary tree)
- n = 3 → 3 - 2 = 1
- n = 4 → 4 - 2 = 2
- n = 1000 → 1000 - 2 = 998

