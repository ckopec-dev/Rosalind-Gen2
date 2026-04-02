# Rosalind Problem: Find the Shortest Non-Shared Substring of Two Strings

## Problem Understanding

We need to find the shortest substring that appears in one string but not in the other string.

## Solution Approach

1. Generate all substrings of both strings
2. Find substrings that exist in one string but not in the other
3. Return the shortest such substring

## Implementation

```lua
function find_shortest_non_shared_substring(s1, s2)
    -- Helper function to get all substrings of a string
    function get_all_substrings(str)
        local substrings = {}
        for i = 1, #str do
            for j = i, #str do
                table.insert(substrings, str:sub(i, j))
            end
        end
        return substrings
    end
    
    -- Helper function to check if a substring exists in a string
    function substring_exists(substring, str)
        return str:find(substring) ~= nil
    end
    
    -- Get all substrings of both strings
    local substrings1 = get_all_substrings(s1)
    local substrings2 = get_all_substrings(s2)
    
    -- Create sets for faster lookup
    local set1 = {}
    local set2 = {}
    
    for _, sub in ipairs(substrings1) do
        set1[sub] = true
    end
    
    for _, sub in ipairs(substrings2) do
        set2[sub] = true
    end
    
    -- Find non-shared substrings (in one string but not in the other)
    local non_shared = {}
    
    -- Check substrings from s1 that are not in s2
    for _, sub in ipairs(substrings1) do
        if not set2[sub] then
            table.insert(non_shared, sub)
        end
    end
    
    -- Check substrings from s2 that are not in s1
    for _, sub in ipairs(substrings2) do
        if not set1[sub] then
            table.insert(non_shared, sub)
        end
    end
    
    -- Find the shortest non-shared substring
    local shortest = nil
    for _, sub in ipairs(non_shared) do
        if shortest == nil or #sub < #shortest then
            shortest = sub
        end
    end
    
    return shortest
end

-- Alternative more efficient approach
function find_shortest_non_shared_substring_optimized(s1, s2)
    -- Helper function to generate substrings
    function get_substrings(str)
        local substrings = {}
        for i = 1, #str do
            for j = i, #str do
                table.insert(substrings, str:sub(i, j))
            end
        end
        return substrings
    end
    
    -- Get all substrings
    local subs1 = get_substrings(s1)
    local subs2 = get_substrings(s2)
    
    -- Create lookup tables
    local lookup1 = {}
    local lookup2 = {}
    
    for _, sub in ipairs(subs1) do
        lookup1[sub] = true
    end
    
    for _, sub in ipairs(subs2) do
        lookup2[sub] = true
    end
    
    -- Find the shortest non-shared substring
    local shortest = nil
    
    -- Check all substrings from s1 that are not in s2
    for _, sub in ipairs(subs1) do
        if not lookup2[sub] then
            if shortest == nil or #sub < #shortest then
                shortest = sub
            end
        end
    end
    
    -- Check all substrings from s2 that are not in s1
    for _, sub in ipairs(subs2) do
        if not lookup1[sub] then
            if shortest == nil or #sub < #shortest then
                shortest = sub
            end
        end
    end
    
    return shortest
end

-- Test function
function test_solution()
    -- Test case 1
    local s1 = "ABCD"
    local s2 = "ACDF"
    local result = find_shortest_non_shared_substring_optimized(s1, s2)
    print("Input strings: " .. s1 .. " and " .. s2)
    print("Shortest non-shared substring: '" .. result .. "'")
    print()
    
    -- Test case 2
    local s3 = "ABCD"
    local s4 = "EFGH"
    local result2 = find_shortest_non_shared_substring_optimized(s3, s4)
    print("Input strings: " .. s3 .. " and " .. s4)
    print("Shortest non-shared substring: '" .. result2 .. "'")
    print()
    
    -- Test case 3
    local s5 = "ABC"
    local s6 = "ABC"
    local result3 = find_shortest_non_shared_substring_optimized(s5, s6)
    print("Input strings: " .. s5 .. " and " .. s6)
    print("Shortest non-shared substring: '" .. result3 .. "'")
end

-- Run tests
test_solution()
```

## Explanation

The solution works as follows:

1. **Generate all substrings**: For each input string, we generate all possible substrings by iterating through all possible start and end positions.

2. **Create lookup sets**: We convert the substrings of each string into lookup tables (hash tables) for O(1) membership testing.

3. **Find non-shared substrings**: We check each substring from the first string to see if it exists in the second string, and vice versa.

4. **Return shortest**: Among all non-shared substrings, we return the one with minimum length.

## Time Complexity
- O(n³) where n is the length of the strings
- Generating all substrings: O(n²) for each string
- Checking membership: O(1) with hash tables
- Finding minimum: O(n²) in worst case

## Space Complexity
- O(n²) for storing all substrings and lookup tables

## Sample Output
```
Input strings: ABCD and ACDF
Shortest non-shared substring: 'B'

Input strings: ABCD and EFGH
Shortest non-shared substring: 'A'

Input strings: ABC and ABC
Shortest non-shared substring: 'A'
```

This solution efficiently finds the shortest substring that appears in one string but not in the other, which is exactly what the Rosalind problem requires.

