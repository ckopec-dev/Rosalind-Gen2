# Rosalind Problem: Enumerating Gene Orders (Permutations)

## Problem Description
Given a positive integer n ≤ 7, return the total number of permutations of length n, followed by a list of all such permutations (in any order).

## Solution in Lua

```lua
-- Function to generate all permutations of a given array
function permute(arr, start, result)
    if start == #arr then
        -- Add a copy of the current permutation to results
        local perm = {}
        for i = 1, #arr do
            table.insert(perm, arr[i])
        end
        table.insert(result, perm)
        return
    end
    
    for i = start, #arr do
        -- Swap elements at positions start and i
        arr[start], arr[i] = arr[i], arr[start]
        
        -- Recursively generate permutations for the rest
        permute(arr, start + 1, result)
        
        -- Backtrack: swap back
        arr[start], arr[i] = arr[i], arr[start]
    end
end

-- Function to print permutations in the required format
function print_permutations(n)
    -- Create array [1, 2, 3, ..., n]
    local arr = {}
    for i = 1, n do
        table.insert(arr, i)
    end
    
    -- Generate all permutations
    local permutations = {}
    permute(arr, 1, permutations)
    
    -- Print total number of permutations
    print(#permutations)
    
    -- Print each permutation
    for _, perm in ipairs(permutations) do
        local str = ""
        for i, num in ipairs(perm) do
            if i > 1 then str = str .. " " end
            str = str .. num
        end
        print(str)
    end
end

-- Main execution
local n = tonumber(io.read())
print_permutations(n)
```

## Example Input/Output

**Input:**
```
3
```

**Output:**
```
6
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1
```

## Explanation

1. **Problem Analysis**: We need to generate all possible permutations of numbers from 1 to n
2. **Algorithm**: 
   - Use backtracking to generate all permutations
   - For each position, try placing each remaining element
   - Recursively generate permutations for the remaining positions
   - Backtrack by restoring the original array state
3. **Time Complexity**: O(n! × n) where n! is the number of permutations and n is the time to copy each permutation
4. **Space Complexity**: O(n! × n) for storing all permutations

## Key Features

- Uses recursive backtracking to generate permutations
- Handles input/output in the required format
- Works for n ≤ 7 as specified in the problem
- Prints the total count followed by all permutations
- Each permutation is printed on a separate line with space-separated numbers

The solution efficiently generates all permutations using the standard backtracking algorithm and formats the output according to Rosalind's requirements.

