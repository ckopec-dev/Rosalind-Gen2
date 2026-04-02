# Rosalind Problem: Introduction to Alternative Splicing

## Problem Statement
In this problem, we need to calculate the number of ways to choose k items from n items, where k can be any value from 0 to n. This is essentially calculating the sum of all binomial coefficients C(n,k) for k = 0 to n, which equals 2^n.

## Solution Approach
The mathematical insight is that:
$$\sum_{k=0}^{n} \binom{n}{k} = 2^n$$

This is because each element can either be included or not included in a subset, giving us 2 choices for each of the n elements.

## Lua Implementation

```lua
function alternative_splicing(n)
    -- Calculate 2^n using bit shifting for efficiency
    -- Since 2^n = 1 << n in binary representation
    return 1 << n
end

-- Read input from stdin
local line = io.read()
local n = tonumber(line)

-- Calculate and print result
local result = alternative_splicing(n)
print(result % 1000000)  -- Return result modulo 1000000 as required
```

## Explanation

1. **Mathematical Foundation**: The problem asks for the sum of all possible ways to choose subsets from n elements. This is equivalent to calculating the sum of all binomial coefficients C(n,k) where k ranges from 0 to n.

2. **Key Insight**: Using the binomial theorem, we know that:
   $$(1 + 1)^n = \sum_{k=0}^{n} \binom{n}{k} \cdot 1^{n-k} \cdot 1^k = \sum_{k=0}^{n} \binom{n}{k} = 2^n$$

3. **Implementation**: 
   - We read the input value n
   - Calculate 2^n using bit shifting (1 << n) for efficiency
   - Return the result modulo 1000000 as required by the problem

## Example
If n = 3:
- C(3,0) + C(3,1) + C(3,2) + C(3,3) = 1 + 3 + 3 + 1 = 8
- 2^3 = 8 ✓

The solution has O(1) time complexity and O(1) space complexity.

