# Rosalind Problem: Find All Shared k-mers of a Pair of Strings (Lua Solution)

## Problem Understanding

Given two DNA strings, we need to find all k-mers that appear in both strings. A k-mer is a substring of length k.

## Solution

```lua
function find_all_shared_kmers(text1, text2, k)
    -- Function to get all k-mers from a string
    function get_kmers(text, k)
        local kmers = {}
        for i = 1, #text - k + 1 do
            table.insert(kmers, text:sub(i, i + k - 1))
        end
        return kmers
    end
    
    -- Get all k-mers from both strings
    local kmers1 = get_kmers(text1, k)
    local kmers2 = get_kmers(text2, k)
    
    -- Create a set of k-mers from first string
    local kmer_set = {}
    for _, kmer in ipairs(kmers1) do
        kmer_set[kmer] = true
    end
    
    -- Find shared k-mers
    local shared_kmers = {}
    for _, kmer in ipairs(kmers2) do
        if kmer_set[kmer] then
            table.insert(shared_kmers, kmer)
        end
    end
    
    -- Remove duplicates
    local unique_shared = {}
    local seen = {}
    for _, kmer in ipairs(shared_kmers) do
        if not seen[kmer] then
            seen[kmer] = true
            table.insert(unique_shared, kmer)
        end
    end
    
    return unique_shared
end

-- Function to print results in the required format
function print_shared_kmers(text1, text2, k)
    local shared = find_all_shared_kmers(text1, text2, k)
    
    -- Sort the results
    table.sort(shared)
    
    -- Print each k-mer
    for _, kmer in ipairs(shared) do
        print(kmer)
    end
end

-- Example usage:
-- local text1 = "AAACTCATC"
-- local text2 = "TTATDTCAC"
-- local k = 3
-- print_shared_kmers(text1, text2, k)

return {
    find_all_shared_kmers = find_all_shared_kmers,
    print_shared_kmers = print_shared_kmers
}
```

## Alternative Implementation with Reverse Complement

Since this is a Rosalind problem, we might also need to consider reverse complements:

```lua
function reverse_complement(dna)
    local complement = {
        ['A'] = 'T', ['T'] = 'A', ['G'] = 'C', ['C'] = 'G'
    }
    local result = ""
    for i = #dna, 1, -1 do
        result = result .. complement[dna:sub(i, i)]
    end
    return result
end

function find_all_shared_kmers_with_rc(text1, text2, k)
    -- Function to get all k-mers from a string
    function get_kmers(text, k)
        local kmers = {}
        for i = 1, #text - k + 1 do
            table.insert(kmers, text:sub(i, i + k - 1))
        end
        return kmers
    end
    
    -- Get all k-mers from both strings
    local kmers1 = get_kmers(text1, k)
    local kmers2 = get_kmers(text2, k)
    
    -- Create a set of k-mers from first string (including reverse complements)
    local kmer_set = {}
    for _, kmer in ipairs(kmers1) do
        kmer_set[kmer] = true
        kmer_set[reverse_complement(kmer)] = true
    end
    
    -- Find shared k-mers
    local shared_kmers = {}
    for _, kmer in ipairs(kmers2) do
        if kmer_set[kmer] then
            table.insert(shared_kmers, kmer)
        end
    end
    
    -- Remove duplicates and sort
    local unique_shared = {}
    local seen = {}
    for _, kmer in ipairs(shared_kmers) do
        if not seen[kmer] then
            seen[kmer] = true
            table.insert(unique_shared, kmer)
        end
    end
    
    table.sort(unique_shared)
    return unique_shared
end
```

## Complete Solution for Rosalind

```lua
function solve_rosalind_shared_kmers(text1, text2, k)
    -- Get all k-mers from both strings
    local function get_kmers(text, k)
        local kmers = {}
        for i = 1, #text - k + 1 do
            table.insert(kmers, text:sub(i, i + k - 1))
        end
        return kmers
    end
    
    -- Get k-mers from first string
    local kmers1 = get_kmers(text1, k)
    
    -- Create set from first string's k-mers
    local kmer_set = {}
    for _, kmer in ipairs(kmers1) do
        kmer_set[kmer] = true
    end
    
    -- Find shared k-mers from second string
    local shared = {}
    local kmers2 = get_kmers(text2, k)
    
    for _, kmer in ipairs(kmers2) do
        if kmer_set[kmer] then
            table.insert(shared, kmer)
        end
    end
    
    -- Remove duplicates and sort
    local result = {}
    local seen = {}
    for _, kmer in ipairs(shared) do
        if not seen[kmer] then
            seen[kmer] = true
            table.insert(result, kmer)
        end
    end
    
    table.sort(result)
    return result
end

-- Usage example:
-- local text1 = "AAACTCATC"
-- local text2 = "TTATCTCAC"
-- local k = 3
-- local solution = solve_rosalind_shared_kmers(text1, text2, k)
-- for _, kmer in ipairs(solution) do
--     print(kmer)
-- end
```

## Key Points

1. **Time Complexity**: O(n₁ × n₂ × k) where n₁ and n₂ are the lengths of the strings
2. **Space Complexity**: O(n₁ × k) for storing k-mers
3. **Edge Cases**: Handle strings shorter than k
4. **Output Format**: Results should be sorted alphabetically

This solution efficiently finds all shared k-mers between two DNA strings by using a hash set for fast lookup operations.

