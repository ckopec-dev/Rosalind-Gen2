# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Statement
Given: A positive integer n (n ≤ 1000).

Return: The number of internal nodes of any unrooted binary tree having n leaves.

## Solution

```python
def counting_phylogenetic_ancestors(n):
    """
    Calculate the number of internal nodes in an unrooted binary tree with n leaves.
    
    For an unrooted binary tree with n leaves, the number of internal nodes is n - 2.
    
    Args:
        n (int): Number of leaves
        
    Returns:
        int: Number of internal nodes
    """
    return n - 2

# Read input
n = int(input().strip())

# Calculate and print result
result = counting_phylogenetic_ancestors(n)
print(result)
```

## Explanation

This problem is based on the mathematical properties of unrooted binary trees:

1. **Key Insight**: In any unrooted binary tree, if there are n leaves, then there are exactly n - 2 internal nodes.

2. **Mathematical Reasoning**:
   - An unrooted binary tree with n leaves has 2n - 2 total vertices (including leaves)
   - Since n of these are leaves, the remaining vertices are internal nodes
   - Therefore: Internal nodes = (2n - 2) - n = n - 2

3. **Example**:
   - If n = 4, then internal nodes = 4 - 2 = 2
   - If n = 10, then internal nodes = 10 - 2 = 8

## Test Case
```
Input: 4
Output: 2
```

## Time and Space Complexity
- **Time Complexity**: O(1) - constant time operation
- **Space Complexity**: O(1) - constant space usage

This solution directly applies the mathematical formula for unrooted binary trees and works for all valid inputs within the given constraints (n ≤ 1000).

