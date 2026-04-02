# Rosalind Problem: Complementing a Strand of DNA (Lua Solution)

## Problem Statement
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'. The reverse complement of a DNA string is formed by first taking the reverse of the string, then taking the complement of each symbol.

Given: A DNA string s of length at most 1000 bp.

Return: The reverse complement s^c of s.

## Solution

```lua
function reverse_complement(dna_string)
    -- Create a mapping for DNA complement
    local complement_map = {
        ['A'] = 'T',
        ['T'] = 'A',
        ['C'] = 'G',
        ['G'] = 'C'
    }
    
    -- Reverse the string
    local reversed = string.reverse(dna_string)
    
    -- Complement each nucleotide
    local result = ""
    for i = 1, #reversed do
        local nucleotide = string.sub(reversed, i, i)
        result = result .. complement_map[nucleotide]
    end
    
    return result
end

-- Read input from stdin
local input = io.read()

-- Print the reverse complement
print(reverse_complement(input))
```

## Example

**Input:**
```
AAAACCCGGT
```

**Output:**
```
ACCGGGTTTT
```

## Explanation

1. **Original string:** `AAAACCCGGT`
2. **Reversed string:** `TGGCCCAAAA`
3. **Complemented string:** `ACCGGGTTTT`

The solution works by:
1. Creating a mapping table for DNA complements
2. Reversing the input string using `string.reverse()`
3. Iterating through each character of the reversed string
4. Looking up each nucleotide in the complement map
5. Building the final result string

## Alternative Compact Solution

```lua
function reverse_complement(dna_string)
    local map = {A='T', T='A', C='G', G='C'}
    local result = ""
    for i = #dna_string, 1, -1 do
        result = result .. map[string.sub(dna_string, i, i)]
    end
    return result
end

print(reverse_complement(io.read()))
```

This solution efficiently handles the DNA reverse complement problem with O(n) time complexity where n is the length of the DNA string.

