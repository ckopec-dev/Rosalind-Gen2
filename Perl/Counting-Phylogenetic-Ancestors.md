# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Statement
Given: A positive integer n (n≤1000).

Return: The number of internal nodes of any unrooted binary tree having n leaves.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $n = <STDIN>;
chomp $n;

# For an unrooted binary tree with n leaves, the number of internal nodes is n - 2
# This is a mathematical property of binary trees
my $internal_nodes = $n - 2;

# Output the result
print "$internal_nodes\n";
```

## Explanation

This problem is based on a fundamental property of binary trees:

1. In any binary tree, if we have n leaves, the number of internal nodes is n - 2
2. This applies to unrooted binary trees as well
3. The reasoning is that in a binary tree, each internal node (except the root) has exactly one parent, and each leaf has exactly one parent
4. The total number of nodes is n + (n-2) = 2n - 2
5. But we can also think of it as: n leaves + (n-2) internal nodes = 2n - 2 total nodes

## Example
- Input: 4
- Output: 2
- Input: 10
- Output: 8

## Test with sample input
```bash
echo "4" | perl counting_phylogenetic_ancestors.pl
# Output: 2
```

The solution is straightforward and relies on the mathematical relationship between leaves and internal nodes in binary trees.

