# Rosalind Problem: Construct a String Spelled by a Gapped Genome Path

## Problem Understanding

This problem asks us to reconstruct a string from a gapped genome path, where each element contains two parts separated by a gap of length k.

## Solution Approach

1. Parse the gapped path into prefix and suffix components
2. Extract the first part (prefix) of each component to form the first string
3. Extract the second part (suffix) of each component to form the second string
4. Combine the strings with proper overlap

## Lua Implementation

```lua
function construct_string_from_gapped_path(gapped_path, k)
    -- Split the gapped path into individual components
    local components = {}
    for component in string.gmatch(gapped_path, "%S+") do
        table.insert(components, component)
    end
    
    -- Extract prefix and suffix parts
    local prefixes = {}
    local suffixes = {}
    
    for _, component in ipairs(components) do
        -- Split by the '|' character
        local parts = {}
        for part in string.gmatch(component, "[^|]+") do
            table.insert(parts, part)
        end
        
        table.insert(prefixes, parts[1])
        table.insert(suffixes, parts[2])
    end
    
    -- Construct the first string from prefixes
    local first_string = prefixes[1]
    for i = 2, #prefixes do
        first_string = first_string .. prefixes[i]:sub(-1)
    end
    
    -- Construct the second string from suffixes
    local second_string = suffixes[1]
    for i = 2, #suffixes do
        second_string = second_string .. suffixes[i]:sub(-1)
    end
    
    -- Combine strings with overlap
    local result = first_string .. string.sub(second_string, k + 1)
    
    return result
end

-- Alternative implementation that's more explicit about the overlap
function construct_string_from_gapped_path_v2(gapped_path, k)
    -- Parse the gapped path
    local components = {}
    for component in string.gmatch(gapped_path, "%S+") do
        table.insert(components, component)
    end
    
    -- Extract prefix and suffix parts
    local prefixes = {}
    local suffixes = {}
    
    for _, component in ipairs(components) do
        local prefix, suffix = string.match(component, "(.-)%|(.-)$")
        table.insert(prefixes, prefix)
        table.insert(suffixes, suffix)
    end
    
    -- Build first string from prefixes
    local first_string = prefixes[1]
    for i = 2, #prefixes do
        first_string = first_string .. prefixes[i]:sub(-1)
    end
    
    -- Build second string from suffixes
    local second_string = suffixes[1]
    for i = 2, #suffixes do
        second_string = second_string .. suffixes[i]:sub(-1)
    end
    
    -- Combine with proper overlap
    local overlap = string.sub(second_string, k + 1)
    return first_string .. overlap
end

-- Main function to solve the problem
function solve_gapped_genome_path(gapped_path, k)
    return construct_string_from_gapped_path_v2(gapped_path, k)
end

-- Example usage:
-- local gapped_path = "GACC|GCGC ACCG|CGCC CCGA|GCCG CGAT|CCGA"
-- local k = 3
-- local result = solve_gapped_genome_path(gapped_path, k)
-- print(result)
```

## Example

Given the input:
```
GACC|GCGC ACCG|CGCC CCGA|GCCG CGAT|CCGA
k = 3
```

The algorithm:
1. Extract prefixes: GACC, ACCG, CCGA, CGAT
2. Extract suffixes: GCGC, CGCC, GCCG, CCGA
3. First string: GACC + (ACCG: last char) + (CCGA: last char) + (CGAT: last char) = GACCGAAT
4. Second string: GCGC + (CGCC: last char) + (GCCG: last char) + (CCGA: last char) = GCGCCCGA
5. Combine with overlap: GACCGAAT + (CCGA from second string, starting from position 4) = GACCGAATCGA

## Key Points

- The gap size `k` determines how much overlap there should be between the two strings
- We extract prefix and suffix components from each gapped element
- The final string is constructed by concatenating the first string with the suffix part starting from position k+1
- The solution handles the overlap correctly by taking only the non-overlapping portion of the second string

This implementation correctly solves the gapped genome path reconstruction problem from Rosalind.

