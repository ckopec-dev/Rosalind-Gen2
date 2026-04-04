# Rosalind Problem: Fibonacci Numbers

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of Fn (the nth Fibonacci number).

## Solution

```python
def fibonacci(n):
    """
    Calculate the nth Fibonacci number
    
    Args:
        n (int): Position in Fibonacci sequence (1-indexed)
    
    Returns:
        int: The nth Fibonacci number
    """
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    else:
        # Use iterative approach for efficiency
        a, b = 0, 1
        for _ in range(2, n + 1):
            a, b = b, a + b
        return b

# Read input
n = int(input().strip())

# Calculate and print result
result = fibonacci(n)
print(result)
```

## Alternative Solution (Recursive with Memoization)

```python
def fibonacci_memo(n, memo={}):
    """
    Calculate the nth Fibonacci number using memoization
    
    Args:
        n (int): Position in Fibonacci sequence
        memo (dict): Memoization dictionary
    
    Returns:
        int: The nth Fibonacci number
    """
    if n in memo:
        return memo[n]
    
    if n <= 0:
        return 0
    elif n == 1:
        return 1
    else:
        memo[n] = fibonacci_memo(n-1, memo) + fibonacci_memo(n-2, memo)
        return memo[n]

# Read input
n = int(input().strip())

# Calculate and print result
result = fibonacci_memo(n)
print(result)
```

## Example

**Input:**
```
6
```

**Output:**
```
8
```

**Explanation:**
The Fibonacci sequence starts as: 1, 1, 2, 3, 5, 8, 13, ...
So F₆ = 8

## Key Points

1. **Fibonacci Sequence**: Each number is the sum of the two preceding numbers
2. **Base Cases**: F₁ = 1, F₂ = 1
3. **Efficiency**: The iterative solution is more efficient than recursive approach
4. **Input Range**: n is guaranteed to be ≤ 25, so no overflow concerns

The iterative solution has O(n) time complexity and O(1) space complexity, making it optimal for this problem.

