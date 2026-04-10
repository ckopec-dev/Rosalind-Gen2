# Rosalind Problem: Find the Most Frequent Words with Mismatches in a String

## Problem Description
Given a DNA string and integers k (k-mer length) and d (maximum number of mismatches), find the most frequent k-mers with up to d mismatches in the string.

## Solution in Lua

```lua
function hamming_distance(s1, s2)
    local distance = 0
    for i = 1, #s1 do
        if s1:sub(i, i) ~= s2:sub(i, i) then
            distance = distance + 1
        end
    end
    return distance
end

function get_kmers(text, k)
    local kmers = {}
    for i = 1, #text - k + 1 do
        table.insert(kmers, text:sub(i, i + k - 1))
    end
    return kmers
end

function get_neighbors(pattern, d)
    if d == 0 then
        return {pattern}
    end
    
    if #pattern == 1 then
        return {"A", "C", "G", "T"}
    end
    
    local suffix = pattern:sub(2)
    local suffix_neighbors = get_neighbors(suffix, d)
    local neighbors = {}
    
    for _, neighbor in ipairs(suffix_neighbors) do
        if hamming_distance(suffix, neighbor) < d then
            table.insert(neighbors, pattern:sub(1, 1) .. neighbor)
        else
            for _, base in ipairs({"A", "C", "G", "T"}) do
                table.insert(neighbors, base .. neighbor)
            end
        end
    end
    
    return neighbors
end

function frequent_words_with_mismatches(text, k, d)
    local frequency_map = {}
    local max_count = 0
    
    -- Get all k-mers from text
    local kmers = get_kmers(text, k)
    
    -- For each k-mer, find all neighbors with up to d mismatches
    for _, kmer in ipairs(kmers) do
        local neighbors = get_neighbors(kmer, d)
        for _, neighbor in ipairs(neighbors) do
            if not frequency_map[neighbor] then
                frequency_map[neighbor] = 0
            end
            frequency_map[neighbor] = frequency_map[neighbor] + 1
            if frequency_map[neighbor] > max_count then
                max_count = frequency_map[neighbor]
            end
        end
    end
    
    -- Find all patterns with maximum frequency
    local result = {}
    for pattern, count in pairs(frequency_map) do
        if count == max_count then
            table.insert(result, pattern)
        end
    end
    
    return result
end

-- Alternative implementation with better efficiency
function frequent_words_with_mismatches_optimized(text, k, d)
    local frequency_map = {}
    local max_count = 0
    
    -- Generate all possible k-mers
    local function generate_all_kmers(k)
        local kmers = {}
        local bases = {"A", "C", "G", "T"}
        
        local function backtrack(current, length)
            if length == 0 then
                table.insert(kmers, current)
                return
            end
            
            for _, base in ipairs(bases) do
                backtrack(current .. base, length - 1)
            end
        end
        
        backtrack("", k)
        return kmers
    end
    
    -- Get all k-mers from text
    local kmers = get_kmers(text, k)
    
    -- For each k-mer, count all its neighbors with up to d mismatches
    for _, kmer in ipairs(kmers) do
        -- Generate all k-mers with up to d mismatches
        local function get_all_mismatches(pattern, max_mismatches)
            local result = {}
            local function generate(current, mismatches, pos)
                if pos > #pattern then
                    table.insert(result, current)
                    return
                end
                
                if mismatches < max_mismatches then
                    -- Try changing this position
                    for _, base in ipairs({"A", "C", "G", "T"}) do
                        if base ~= pattern:sub(pos, pos) then
                            generate(current .. base, mismatches + 1, pos + 1)
                        end
                    end
                end
                
                -- Keep original base
                generate(current .. pattern:sub(pos, pos), mismatches, pos + 1)
            end
            
            generate("", 0, 1)
            return result
        end
        
        local mismatches = get_all_mismatches(kmer, d)
        for _, mismatch in ipairs(mismatches) do
            if not frequency_map[mismatch] then
                frequency_map[mismatch] = 0
            end
            frequency_map[mismatch] = frequency_map[mismatch] + 1
            if frequency_map[mismatch] > max_count then
                max_count = frequency_map[mismatch]
            end
        end
    end
    
    -- Find all patterns with maximum frequency
    local result = {}
    for pattern, count in pairs(frequency_map) do
        if count == max_count then
            table.insert(result, pattern)
        end
    end
    
    return result
end

-- Simple and correct approach
function solve_frequent_words_with_mismatches(text, k, d)
    local frequency_map = {}
    
    -- Get all k-mers from text
    local kmers = get_kmers(text, k)
    
    -- For each k-mer, count all neighbors with up to d mismatches
    for _, kmer in ipairs(kmers) do
        -- Generate all possible k-mers with up to d mismatches
        local function get_mismatches(pattern, max_mismatches)
            local result = {}
            local function generate(current, mismatches, pos)
                if pos > #pattern then
                    table.insert(result, current)
                    return
                end
                
                -- Try changing position to each base
                if mismatches < max_mismatches then
                    for _, base in ipairs({"A", "C", "G", "T"}) do
                        if base ~= pattern:sub(pos, pos) then
                            generate(current .. base, mismatches + 1, pos + 1)
                        end
                    end
                end
                
                -- Keep original base
                generate(current .. pattern:sub(pos, pos), mismatches, pos + 1)
            end
            
            generate("", 0, 1)
            return result
        end
        
        local neighbors = get_mismatches(kmer, d)
        for _, neighbor in ipairs(neighbors) do
            if not frequency_map[neighbor] then
                frequency_map[neighbor] = 0
            end
            frequency_map[neighbor] = frequency_map[neighbor] + 1
        end
    end
    
    -- Find maximum frequency
    local max_count = 0
    for _, count in pairs(frequency_map) do
        if count > max_count then
            max_count = count
        end
    end
    
    -- Collect all patterns with maximum frequency
    local result = {}
    for pattern, count in pairs(frequency_map) do
        if count == max_count then
            table.insert(result, pattern)
        end
    end
    
    return result
end

-- Example usage:
-- local text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
-- local k = 4
-- local d = 1
-- local result = solve_frequent_words_with_mismatches(text, k, d)
-- for _, pattern in ipairs(result) do
--     print(pattern)
-- end

return {
    hamming_distance = hamming_distance,
    get_kmers = get_kmers,
    solve_frequent_words_with_mismatches = solve_frequent_words_with_mismatches
}
```

## Explanation

This solution works by:

1. **Finding all k-mers**: Extract all possible k-length substrings from the input text
2. **Generating neighbors**: For each k-mer, generate all possible k-mers that differ by at most d positions
3. **Counting frequencies**: Keep track of how many times each pattern appears
4. **Finding maximum**: Identify the patterns with the highest frequency

## Key Functions

- `hamming_distance`: Calculates the Hamming distance between two strings
- `get_kmers`: Extracts all k-mers from a text string
- `solve_frequent_words_with_mismatches`: Main function that solves the problem

## Time Complexity
- O(n × k × 4^k) where n is the length of the text and k is the k-mer length
- The exponential factor 4^k comes from generating all possible mismatches

## Space Complexity
- O(4^k) for storing the frequency map of all possible patterns

The solution handles the case where multiple patterns may have the same maximum frequency and returns all such patterns.

