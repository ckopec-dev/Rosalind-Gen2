# Rosalind Problem: Partial Permutations

## Problem Statement
In combinatorics, a partial permutation (or k-permutation of n) is an ordering of k objects selected from a set of n objects. The number of such partial permutations is denoted by P(n,k) = n!/(n-k)!.

Given: Positive integers n and k such that 1000 ≥ n > 0 and 100 ≥ k > 0.

Return: The total number of partial permutations P(n,k), modulo 1,000,000.

## Solution in Lua

```lua
-- Function to calculate factorial
function factorial(n)
    local result = 1
    for i = 1, n do
        result = result * i
    end
    return result
end

-- Function to calculate partial permutation P(n,k)
function partial_permutation(n, k)
    local MOD = 1000000
    
    -- P(n,k) = n! / (n-k)!
    -- We can compute this as n * (n-1) * (n-2) * ... * (n-k+1)
    local result = 1
    for i = n - k + 1, n do
        result = (result * i) % MOD
    end
    
    return result
end

-- Read input from stdin
local line = io.read()
local n, k = string.match(line, "(%d+)%s+(%d+)")

n = tonumber(n)
k = tonumber(k)

-- Calculate and print result
local result = partial_permutation(n, k)
print(result)
```

## Explanation

The solution works by directly computing the partial permutation formula P(n,k) = n!/(n-k)! without calculating the full factorials, which would be computationally expensive and potentially cause overflow issues.

Key insights:
1. **Direct computation**: Instead of computing n! and (n-k)! separately, we compute the product n × (n-1) × ... × (n-k+1)
2. **Modular arithmetic**: We apply the modulo operation at each step to prevent integer overflow
3. **Efficiency**: This approach runs in O(k) time complexity instead of O(n)

## Example
For n = 21 and k = 7:
- P(21,7) = 21 × 20 × 19 × 18 × 17 × 16 × 15
- The result should be computed modulo 1,000,000

## Alternative Implementation (More Explicit)

```lua
function partial_permutation_explicit(n, k)
    local MOD = 1000000
    local result = 1
    
    -- Multiply n * (n-1) * ... * (n-k+1)
    for i = 1, k do
        result = (result * (n - i + 1)) % MOD
    end
    
    return result
end
```

This approach directly implements the mathematical definition of partial permutations and handles the modulo requirement efficiently.

