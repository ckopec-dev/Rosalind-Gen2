# Rosalind Problem: Find_Frequent_Words_with_Mismatches_and_Reverse_Complements

## Problem Description
Find all k-mers that appear most frequently in a DNA string, allowing up to d mismatches and considering reverse complements.

## Solution in Lua

```lua
function reverse_complement(dna)
    local complement = {
        ['A'] = 'T', ['T'] = 'A',
        ['G'] = 'C', ['C'] = 'G'
    }
    local result = ""
    for i = #dna, 1, -1 do
        result = result .. complement[dna:sub(i, i)]
    end
    return result
end

function hamming_distance(s1, s2)
    local distance = 0
    for i = 1, #s1 do
        if s1:sub(i, i) ~= s2:sub(i, i) then
            distance = distance + 1
        end
    end
    return distance
end

function get_all_kmers(dna, k)
    local kmers = {}
    for i = 1, #dna - k + 1 do
        local kmer = dna:sub(i, i + k - 1)
        kmers[kmer] = (kmers[kmer] or 0) + 1
    end
    return kmers
end

function find_frequent_words_with_mismatches_and_reverse_complements(dna, k, d)
    local frequency = {}
    
    -- Get all k-mers and their reverse complements
    local all_kmers = {}
    
    -- Process original k-mers
    for i = 1, #dna - k + 1 do
        local kmer = dna:sub(i, i + k - 1)
        local rc = reverse_complement(kmer)
        
        -- Add both k-mer and its reverse complement
        if not all_kmers[kmer] then
            all_kmers[kmer] = true
        end
        if not all_kmers[rc] then
            all_kmers[rc] = true
        end
    end
    
    -- Count occurrences with mismatches
    for kmer in pairs(all_kmers) do
        frequency[kmer] = 0
        
        -- Count matches with original DNA string
        for i = 1, #dna - k + 1 do
            local current_kmer = dna:sub(i, i + k - 1)
            
            -- Check if current_kmer matches kmer with up to d mismatches
            if hamming_distance(kmer, current_kmer) <= d then
                frequency[kmer] = frequency[kmer] + 1
            end
            
            -- Check if current_kmer matches reverse complement of kmer with up to d mismatches
            local rc = reverse_complement(kmer)
            if hamming_distance(rc, current_kmer) <= d then
                frequency[kmer] = frequency[kmer] + 1
            end
        end
    end
    
    -- Find maximum frequency
    local max_freq = 0
    for _, freq in pairs(frequency) do
        if freq > max_freq then
            max_freq = freq
        end
    end
    
    -- Collect all words with maximum frequency
    local result = {}
    for word, freq in pairs(frequency) do
        if freq == max_freq then
            table.insert(result, word)
        end
    end
    
    return result
end

-- Alternative more efficient approach
function find_frequent_words_with_mismatches_and_reverse_complements_optimized(dna, k, d)
    local frequency = {}
    
    -- Generate all possible k-mers with mismatches
    local function get_mismatches(kmer, d)
        local mismatches = {}
        local function generate(current, pos)
            if pos > #kmer then
                table.insert(mismatches, current)
                return
            end
            
            local base = kmer:sub(pos, pos)
            generate(current .. base, pos + 1)  -- Keep original base
            
            -- Try all other bases
            if base ~= 'A' then generate(current .. 'A', pos + 1) end
            if base ~= 'T' then generate(current .. 'T', pos + 1) end
            if base ~= 'G' then generate(current .. 'G', pos + 1) end
            if base ~= 'C' then generate(current .. 'C', pos + 1) end
        end
        
        generate("", 1)
        return mismatches
    end
    
    -- Count all k-mers and their reverse complements with mismatches
    for i = 1, #dna - k + 1 do
        local kmer = dna:sub(i, i + k - 1)
        local rc = reverse_complement(kmer)
        
        -- Process original kmer and its mismatches
        local original_mismatches = get_mismatches(kmer, d)
        for _, mismatch in ipairs(original_mismatches) do
            frequency[mismatch] = (frequency[mismatch] or 0) + 1
        end
        
        -- Process reverse complement and its mismatches
        local rc_mismatches = get_mismatches(rc, d)
        for _, mismatch in ipairs(rc_mismatches) do
            frequency[mismatch] = (frequency[mismatch] or 0) + 1
        end
    end
    
    -- Find maximum frequency
    local max_freq = 0
    for _, freq in pairs(frequency) do
        if freq > max_freq then
            max_freq = freq
        end
    end
    
    -- Collect all words with maximum frequency
    local result = {}
    for word, freq in pairs(frequency) do
        if freq == max_freq then
            table.insert(result, word)
        end
    end
    
    return result
end

-- Example usage:
-- local dna = "ACGTTGCATGTCGCATGATGCATGAGAGG"
-- local k = 4
-- local d = 1
-- local result = find_frequent_words_with_mismatches_and_reverse_complements_optimized(dna, k, d)
-- print(table.concat(result, " "))
```

## Explanation

This solution addresses the Rosalind problem by:

1. **Reverse Complement Function**: Creates the reverse complement of a DNA string
2. **Hamming Distance**: Calculates the number of differences between two strings
3. **Main Algorithm**: 
   - For each k-mer in the DNA string, consider both the k-mer and its reverse complement
   - Count all occurrences allowing up to d mismatches
   - Find the maximum frequency
   - Return all k-mers with maximum frequency

The optimized version generates all possible k-mers with up to d mismatches for each k-mer and reverse complement, then counts their occurrences in the DNA string.

## Time Complexity
- O(n × k × 4^k) where n is the length of DNA string and k is the k-mer length
- Space complexity: O(4^k) for storing k-mers

## Example Input/Output
Input: DNA="ACGTTGCATGTCGCATGATGCATGAGAGG", k=4, d=1
Output: Words that appear most frequently with up to 1 mismatch and considering reverse complements

