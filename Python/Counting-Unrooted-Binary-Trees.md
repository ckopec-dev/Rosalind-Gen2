# Rosalind Problem: Counting Unrooted Binary Trees

## Problem Statement
Given a collection of n taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

For n taxa, the number of distinct unrooted binary trees is given by the formula: (2n-5)!! (double factorial)

## Solution

```python
def count_unrooted_binary_trees(n):
    """
    Count the number of distinct unrooted binary trees with n leaves.
    
    For n taxa, the number of distinct unrooted binary trees is (2n-5)!!
    
    Args:
        n (int): Number of taxa/leaves
        
    Returns:
        int: Number of distinct unrooted binary trees
    """
    if n < 3:
        return 0
    
    # Calculate (2n-5)!! = (2n-5) * (2n-7) * (2n-9) * ... * 3 * 1
    result = 1
    for i in range(2 * n - 5, 0, -2):
        result *= i
    
    return result

def double_factorial(n):
    """
    Calculate the double factorial of n.
    
    Args:
        n (int): Number to calculate double factorial for
        
    Returns:
        int: n!! (double factorial)
    """
    if n <= 0:
        return 1
    result = 1
    while n > 0:
        result *= n
        n -= 2
    return result

# Read input and solve
def solve():
    # Read the number of taxa from input
    n = int(input().strip())
    
    # Calculate and print the result
    result = count_unrooted_binary_trees(n)
    print(result)

# Alternative implementation using math library
import math

def count_unrooted_binary_trees_v2(n):
    """
    Alternative implementation using mathematical approach.
    For n >= 3: (2n-5)!! = (2n-5)! / (2^(n-2) * (n-2)!)
    """
    if n < 3:
        return 0
    
    # Using the double factorial formula
    return double_factorial(2 * n - 5)

# Example usage:
if __name__ == "__main__":
    # Test cases
    test_cases = [3, 4, 5, 6, 7]
    
    print("Testing different values of n:")
    for n in test_cases:
        result = count_unrooted_binary_trees(n)
        print(f"n = {n}: {result}")
    
    # For actual Rosalind problem:
    # Uncomment the following lines to read from stdin
    # solve()
```

## Explanation

The problem asks us to count the number of distinct unrooted binary trees that can be formed with n taxa (leaves).

### Mathematical Background:
- For n taxa, the number of distinct unrooted binary trees is given by the double factorial (2n-5)!!
- This formula comes from the theory of phylogenetic trees
- The double factorial n!! is defined as n × (n-2) × (n-4) × ... × 1 (for odd n)

### Example:
- For n = 3: (2×3-5)!! = 1!! = 1
- For n = 4: (2×4-5)!! = 3!! = 3 × 1 = 3  
- For n = 5: (2×5-5)!! = 5!! = 5 × 3 × 1 = 15

### Time Complexity:
- O(n) where n is the number of taxa
- We iterate through approximately n steps to calculate the double factorial

### Space Complexity:
- O(1) - only using constant extra space

## Sample Input/Output

**Input:**
```
4
```

**Output:**
```
3
```

**Input:**
```
5
```

**Output:**
```
15
```

The solution handles the mathematical formula directly and efficiently computes the result for any valid input value of n.

