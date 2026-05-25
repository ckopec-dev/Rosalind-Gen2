# Rosalind Problem: Implement_ChromosomeToCycle in Lua

## Problem Description
The chromosome to cycle problem involves converting a chromosome (a sequence of signed integers) into a cycle representation. In this representation, each signed integer is converted to two consecutive nodes in a cycle.

## Solution

```lua
function chromosome_to_cycle(chromosome)
    local cycle = {}
    local n = #chromosome
    
    -- Process each element in the chromosome
    for i = 1, n do
        local element = chromosome[i]
        
        -- If element is positive, add 2*i-1 and 2*i to cycle
        if element > 0 then
            cycle[2*i-1] = 2*element - 1
            cycle[2*i] = 2*element
        else
            -- If element is negative, add 2*|element| and 2*|element|-1 to cycle
            cycle[2*i-1] = -2*element
            cycle[2*i] = -2*element - 1
        end
    end
    
    return cycle
end

-- Helper function to print cycle in the required format
function print_cycle(cycle)
    local result = {}
    for i = 1, #cycle do
        table.insert(result, tostring(cycle[i]))
    end
    return "(" .. table.concat(result, " ") .. ")"
end

-- Example usage:
-- Input: [1, -2, 3, -4]
-- Expected output: (1 2 4 3 6 5 8 7)
local example_chromosome = {1, -2, 3, -4}
local result = chromosome_to_cycle(example_chromosome)
print(print_cycle(result))
```

## Explanation

The algorithm works as follows:

1. **Input**: A chromosome represented as a sequence of signed integers
2. **Process**: For each element in the chromosome:
   - If the element is positive: 
     - Place `2*element-1` at position `2*i-1`
     - Place `2*element` at position `2*i`
   - If the element is negative:
     - Place `-2*element` at position `2*i-1`
     - Place `-2*element-1` at position `2*i`
3. **Output**: The resulting cycle as a sequence of integers

## Key Points

- Each signed integer is converted to two consecutive nodes in the cycle
- Positive integers are represented as `2k-1` and `2k` where `k` is the absolute value
- Negative integers are represented as `2k` and `2k-1` where `k` is the absolute value
- The cycle is 1-indexed as per standard Rosalind conventions

## Example Walkthrough

For chromosome `[1, -2, 3, -4]`:
- Element 1 (positive): Place 1 at position 1, 2 at position 2
- Element -2 (negative): Place 4 at position 3, 3 at position 4  
- Element 3 (positive): Place 5 at position 5, 6 at position 6
- Element -4 (negative): Place 8 at position 7, 7 at position 8

Result: `(1 2 4 3 6 5 8 7)`

