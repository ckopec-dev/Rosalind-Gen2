# Rosalind Problem: Implement GreedySorting to Sort a Permutation by Reversals

## Problem Description
Given a permutation π, we want to sort it into the identity permutation (1, 2, 3, ..., n) using reversals. The greedy sorting algorithm works by repeatedly finding the smallest element that is not in its correct position and reversing the segment that contains it.

## Solution in Lua

```lua
function greedy_sorting(permutation)
    local results = {}
    local n = #permutation
    
    -- Create a copy to work with (we'll modify this)
    local p = {}
    for i = 1, n do
        p[i] = permutation[i]
    end
    
    for k = 1, n do
        -- Find where k is located
        local position = 0
        for i = k, n do
            if p[i] == k or p[i] == -k then
                position = i
                break
            end
        end
        
        -- If k is not in position k
        if position ~= k then
            -- If k is negative, we need to reverse it to positive
            if p[k] == -k then
                -- This case handles when we have -k in position k
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
                -- Reverse the segment from k to position
                local temp = {}
                for i = k, position do
                    temp[i] = p[i]
                end
                for i = k, position do
                    p[i] = -temp[position - i + k]
                end
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
            else
                -- Reverse the segment from k to position
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
                local temp = {}
                for i = k, position do
                    temp[i] = p[i]
                end
                for i = k, position do
                    p[i] = -temp[position - i + k]
                end
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
            end
        end
        
        -- If the element at position k is negative, we need to flip it
        if p[k] == -k then
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
            p[k] = k
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
        end
    end
    
    return results
end

-- More accurate implementation for signed permutations
function greedy_sorting_signed(permutation)
    local results = {}
    local n = #permutation
    
    -- Create a copy to work with
    local p = {}
    for i = 1, n do
        p[i] = permutation[i]
    end
    
    for k = 1, n do
        -- Find where k is located (either k or -k)
        local position = 0
        for i = k, n do
            if math.abs(p[i]) == k then
                position = i
                break
            end
        end
        
        -- If k is not in position k
        if position ~= k then
            -- If k is in position position but with negative sign
            if p[position] == -k then
                -- Need to reverse the segment from k to position
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
                -- Reverse the segment from k to position
                local temp = {}
                for i = k, position do
                    temp[i] = p[i]
                end
                for i = k, position do
                    p[i] = -temp[position - i + k]
                end
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
            else
                -- Need to reverse the segment from k to position
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
                -- Reverse the segment from k to position
                local temp = {}
                for i = k, position do
                    temp[i] = p[i]
                end
                for i = k, position do
                    p[i] = -temp[position - i + k]
                end
                table.insert(results, "(" .. table.concat(p, " ") .. ")")
            end
        end
        
        -- If the element at position k is negative, we need to flip it
        if p[k] == -k then
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
            p[k] = k
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
        end
    end
    
    return results
end

-- Correct implementation
function greedy_sorting_final(permutation)
    local results = {}
    local n = #permutation
    
    -- Create a copy to work with
    local p = {}
    for i = 1, n do
        p[i] = permutation[i]
    end
    
    for k = 1, n do
        -- Find where k is located
        local position = 0
        for i = k, n do
            if math.abs(p[i]) == k then
                position = i
                break
            end
        end
        
        -- If k is not in position k
        if position ~= k then
            -- If k is not in correct position, reverse the segment
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
            
            -- Reverse the segment from k to position
            local temp = {}
            for i = k, position do
                temp[i] = p[i]
            end
            for i = k, position do
                p[i] = -temp[position - i + k]
            end
            
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
        end
        
        -- If the element at position k is negative, we need to flip it
        if p[k] == -k then
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
            p[k] = k
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
        end
    end
    
    return results
end

-- Simple version that works with the standard problem format
function greedy_sorting_simple(permutation)
    local results = {}
    local n = #permutation
    
    -- Create a copy to work with
    local p = {}
    for i = 1, n do
        p[i] = permutation[i]
    end
    
    for k = 1, n do
        -- Find where k is located
        local position = 0
        for i = k, n do
            if math.abs(p[i]) == k then
                position = i
                break
            end
        end
        
        -- If k is not in position k
        if position ~= k then
            -- Reverse the segment from k to position
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
            
            -- Reverse segment from k to position
            local temp = {}
            for i = k, position do
                temp[i] = p[i]
            end
            for i = k, position do
                p[i] = -temp[position - i + k]
            end
            
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
        end
        
        -- If the element at position k is negative, flip it
        if p[k] == -k then
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
            p[k] = k
            table.insert(results, "(" .. table.concat(p, " ") .. ")")
        end
    end
    
    return results
end

-- Main function to solve the problem
function solve_greedy_sorting(input_string)
    -- Parse input string to create permutation array
    local permutation = {}
    for num in string.gmatch(input_string, "([^%s]+)") do
        table.insert(permutation, tonumber(num))
    end
    
    -- Apply greedy sorting
    local steps = greedy_sorting_simple(permutation)
    
    -- Print results
    for _, step in ipairs(steps) do
        print(step)
    end
    
    return steps
end

-- Example usage:
-- Input: "-3 +4 +1 +5 -2"
-- Expected output: steps showing the sorting process

-- Test with example
local example = "-3 +4 +1 +5 -2"
print("Input permutation: " .. example)
print("Sorting steps:")

-- Parse the example
local test_permutation = {}
for num in string.gmatch(example, "([^%s]+)") do
    table.insert(test_permutation, tonumber(num))
end

local steps = greedy_sorting_simple(test_permutation)
for _, step in ipairs(steps) do
    print(step)
end
```

## Explanation

The greedy sorting algorithm for sorting permutations by reversals works as follows:

1. **Iterate through positions**: Start from position 1 and work up to n
2. **Find target element**: For position k, find where the element k (or -k) is located
3. **Reverse if needed**: If k is not in position k, reverse the segment from k to the position of k
4. **Flip signs**: If the element at position k is -k, flip it to positive

## Key Points

- The algorithm works with signed permutations (positive and negative integers)
- Each reversal operation is recorded as a step
- The process continues until the permutation becomes the identity permutation (1, 2, 3, ..., n)
- The algorithm is greedy because it always makes the locally optimal choice at each step

## Time Complexity
O(n²) where n is the length of the permutation, since we may need to perform up to n reversals, and each reversal takes O(n) time.

## Space Complexity
O(n) for storing the permutation and intermediate results.

