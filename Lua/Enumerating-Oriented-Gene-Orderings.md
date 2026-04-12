# Rosalind Problem: Enumerating Oriented Gene Orderings (PERM)

## Problem Description
Given a positive integer n, return the total number of signed permutations of length n, followed by a list of all such permutations.

## Solution

```lua
-- Function to generate all permutations of a list
function permute(arr)
    local result = {}
    
    if #arr == 1 then
        return {arr}
    end
    
    for i = 1, #arr do
        local element = arr[i]
        local remaining = {}
        for j = 1, #arr do
            if j ~= i then
                table.insert(remaining, arr[j])
            end
        end
        
        local subperms = permute(remaining)
        for _, subperm in ipairs(subperms) do
            local newperm = {element}
            for _, val in ipairs(subperm) do
                table.insert(newperm, val)
            end
            table.insert(result, newperm)
        end
    end
    
    return result
end

-- Function to generate all signed permutations
function signed_permutations(n)
    -- Generate basic permutation
    local numbers = {}
    for i = 1, n do
        table.insert(numbers, i)
    end
    
    local perms = permute(numbers)
    local result = {}
    
    -- For each permutation, generate all sign combinations
    for _, perm in ipairs(perms) do
        -- Generate all possible sign combinations for this permutation
        local sign_combinations = {}
        local total_signs = 2 ^ n
        
        for i = 0, total_signs - 1 do
            local sign_perm = {}
            for j = 1, n do
                -- Check if j-th bit is set (1 means negative, 0 means positive)
                local bit = (i // (2 ^ (j - 1))) % 2
                if bit == 1 then
                    table.insert(sign_perm, -perm[j])
                else
                    table.insert(sign_perm, perm[j])
                end
            end
            table.insert(result, sign_perm)
        end
    end
    
    return result
end

-- Alternative approach using bit manipulation for better efficiency
function enumerate_signed_permutations(n)
    local result = {}
    local count = 0
    
    -- Generate all permutations of numbers 1 to n
    local numbers = {}
    for i = 1, n do
        table.insert(numbers, i)
    end
    
    -- Function to generate all permutations
    local function generate_perms(arr, start, current_perm, all_perms)
        if start == #arr + 1 then
            table.insert(all_perms, current_perm)
            return
        end
        
        for i = start, #arr do
            -- Swap
            arr[start], arr[i] = arr[i], arr[start]
            -- Add current element to permutation
            local new_perm = {}
            for _, v in ipairs(current_perm) do table.insert(new_perm, v) end
            table.insert(new_perm, arr[start])
            generate_perms(arr, start + 1, new_perm, all_perms)
            -- Swap back
            arr[start], arr[i] = arr[i], arr[start]
        end
    end
    
    local all_perms = {}
    generate_perms(numbers, 1, {}, all_perms)
    
    -- For each permutation, generate all signed versions
    for _, perm in ipairs(all_perms) do
        -- Generate all 2^n sign combinations
        local sign_count = 2 ^ n
        for sign_mask = 0, sign_count - 1 do
            local signed_perm = {}
            for i = 1, n do
                local sign = ((sign_mask // (2 ^ (i - 1))) % 2) == 0 and 1 or -1
                table.insert(signed_perm, sign * perm[i])
            end
            table.insert(result, signed_perm)
            count = count + 1
        end
    end
    
    return count, result
end

-- Main function to solve the problem
function solve_enumerating_oriented_gene_orderings(n)
    local count, permutations = enumerate_signed_permutations(n)
    
    print(count)
    
    for _, perm in ipairs(permutations) do
        local line = ""
        for i, num in ipairs(perm) do
            if i > 1 then line = line .. " " end
            line = line .. num
        end
        print(line)
    end
end

-- Read input and solve
local n = tonumber(io.read("*line"))
solve_enumerating_oriented_gene_orderings(n)
```

## Explanation

This solution works by:

1. **Generating all permutations**: First, we generate all permutations of the numbers 1 through n
2. **Generating signed versions**: For each permutation, we generate all possible sign combinations (positive/negative) for each element
3. **Counting**: We count the total number of signed permutations, which is n! × 2^n

## Key Points

- The total number of signed permutations of length n is n! × 2^n
- For each permutation, we can choose either positive or negative sign for each element
- We use bit manipulation to efficiently generate all sign combinations
- The output format requires each permutation to be printed on a separate line

## Example Input/Output

For n = 2:
```
Input:
2

Output:
8
1 2
1 -2
-1 2
-1 -2
2 1
2 -1
-2 1
-2 -1
```

## Time Complexity
- O(n! × 2^n) for generating all signed permutations
- Space complexity is also O(n! × 2^n) for storing results

