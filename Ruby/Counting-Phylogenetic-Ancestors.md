# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Statement
Given: A positive integer n (n ≤ 1000).

Return: The number of internal nodes of any unrooted binary tree having n leaves.

## Solution

```ruby
def counting_phylogenetic_ancestors(n)
  # For an unrooted binary tree with n leaves, the number of internal nodes
  # is always n - 2
  return n - 2
end

# Read input from stdin
n = gets.chomp.to_i

# Solve and output result
puts counting_phylogenetic_ancestors(n)
```

## Explanation

This problem is based on a fundamental property of unrooted binary trees:

1. In any unrooted binary tree, each internal node has exactly 3 edges connected to it
2. The total number of edges in a tree with n leaves is 2n - 2
3. Since each internal node contributes 3 edges and each leaf contributes 1 edge, if we let i be the number of internal nodes:
   - Total edges = 3i + n (since internal nodes have 3 edges and leaves have 1)
   - But each edge is counted twice (once from each end), so: 2(3i + n) = 2n - 2
   - Simplifying: 6i + 2n = 2n - 2
   - Therefore: 6i = -2, which is incorrect approach

Let me reconsider this more carefully:

For an unrooted binary tree:
- Each internal node has degree 3
- Each leaf has degree 1  
- Total degree = 2 × (number of edges)
- Number of edges = n - 1 (for a tree with n leaves)
- Total degree = 2(n - 1) = 2n - 2
- Let i = number of internal nodes
- Total degree = 3i + n (3 edges per internal node + 1 edge per leaf)
- So: 3i + n = 2n - 2
- Therefore: 3i = n - 2
- So: i = (n - 2)/3

Wait, let me think again about unrooted binary trees specifically:

Actually, for an unrooted binary tree with n leaves, the number of internal nodes is simply n - 2.

This is because:
- Start with n leaves
- Each internal node adds exactly one more node to the tree
- The relationship is: number of internal nodes = n - 2

## Test Case
Input: 4
Output: 2

For a tree with 4 leaves, there are 2 internal nodes.

## Alternative Implementation

```ruby
# More explicit approach
def counting_phylogenetic_ancestors(n)
  # For unrooted binary trees: number of internal nodes = n - 2
  if n < 2
    return 0
  else
    return n - 2
  end
end

# Read input and solve
n = gets.chomp.to_i
puts counting_phylogenetic_ancestors(n)
```

