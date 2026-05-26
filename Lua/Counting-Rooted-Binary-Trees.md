# Rosalind Problem: Counting Rooted Binary Trees

## Problem Statement
Given a positive integer n (n ≤ 1000), return the number of distinct rooted binary trees on n labeled taxa.

## Solution Approach
This is a classic combinatorics problem related to Catalan numbers. For n labeled taxa, the number of distinct rooted binary trees is given by the formula:

**Number of rooted binary trees = (2n-3)!!**

Where (2n-3)!! represents the double factorial of (2n-3).

## Implementation

```lua
function double_factorial(n)
    -- Calculate double factorial n!!
    if n <= 0 then
        return 1
    end
    
    local result = 1
    for i = n, 1, -2 do
        result = result * i
    end
    
    return result
end

function counting_rooted_binary_trees(n)
    -- For n labeled taxa, number of rooted binary trees = (2n-3)!!
    if n <= 0 then
        return 0
    end
    
    local result = double_factorial(2 * n - 3)
    
    -- Return result modulo 1000000 as per typical Rosalind requirements
    return result % 1000000
end

-- Read input
local n = tonumber(io.read())

-- Solve and print result
print(counting_rooted_binary_trees(n))
```

## Explanation

1. **Double Factorial Function**: The `double_factorial` function calculates n!! which is the product of all integers from n down to 1 that have the same parity as n.

2. **Formula Application**: For rooted binary trees with n labeled taxa, the formula is (2n-3)!!, which represents the number of ways to arrange the internal nodes.

3. **Modular Arithmetic**: Since the numbers can get very large, the result is taken modulo 1000000 as is typical in Rosalind problems.

## Example
For n = 3:
- We want (2×3-3)!! = 3!! = 3×1 = 3
- For n = 4:
- We want (2×4-3)!! = 5!! = 5×3×1 = 15

## Time Complexity
- Time: O(n) where n is the input value
- Space: O(1)

This solution efficiently computes the number of distinct rooted binary trees for the given number of labeled taxa using the mathematical relationship to double factorials.

