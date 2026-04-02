# Rosalind Problem: Inferring mRNA from Protein

## Problem Description
Given an amino acid string, determine the number of possible mRNA strings from which the protein could have been translated, modulo 1000000.

## Solution Approach
1. Create a codon table mapping amino acids to their possible codons
2. Count the number of codons for each amino acid
3. Multiply the counts for all amino acids
4. Apply modulo 1000000 to handle large numbers

## Lua Implementation

```lua
function inferring_mrna_from_protein(protein)
    -- Codon table: amino acid -> number of possible codons
    local codon_table = {
        ['A'] = 4,  -- GCU, GCC, GCA, GCG
        ['C'] = 2,  -- UGU, UGC
        ['D'] = 2,  -- GAU, GAC
        ['E'] = 2,  -- GAA, GAG
        ['F'] = 2,  -- UUU, UUC
        ['G'] = 4,  -- GGU, GGC, GGA, GGG
        ['H'] = 2,  -- CAU, CAC
        ['I'] = 3,  -- AUU, AUC, AUA
        ['K'] = 2,  -- AAA, AAG
        ['L'] = 6,  -- UUA, UUG, CUU, CUC, CUA, CUG
        ['M'] = 1,  -- AUG
        ['N'] = 2,  -- AAU, AAC
        ['P'] = 4,  -- CCU, CCC, CCA, CCG
        ['Q'] = 2,  -- CAA, CAG
        ['R'] = 6,  -- CGU, CGC, CGA, CGG, AGA, AGG
        ['S'] = 6,  -- UCU, UCC, UCA, UCG, AGU, AGC
        ['T'] = 4,  -- ACU, ACC, ACA, ACG
        ['V'] = 4,  -- GUU, GUC, GUA, GUG
        ['W'] = 1,  -- UGG
        ['Y'] = 2,  -- UAU, UAC
        ['*'] = 3   -- UAA, UAG, UGA (Stop codons)
    }
    
    local result = 1
    local modulo = 1000000
    
    -- Process each amino acid in the protein
    for i = 1, #protein do
        local amino_acid = string.sub(protein, i, i)
        result = (result * codon_table[amino_acid]) % modulo
    end
    
    -- Account for stop codon (multiply by 3 for final stop codon)
    result = (result * 3) % modulo
    
    return result
end

-- Example usage:
-- local protein = "MA"
-- print(inferring_mrna_from_protein(protein))
```

## Explanation

The solution works by:

1. **Codon Table**: Each amino acid can be encoded by multiple codons (3-letter RNA sequences). The table maps each amino acid to the number of possible codons that can encode it.

2. **Multiplication Principle**: For each amino acid in the protein sequence, we multiply the number of possible codons for that amino acid with our running total.

3. **Stop Codons**: The problem asks for the number of possible mRNA strings, which includes the stop codon. Since there are 3 possible stop codons (UAA, UAG, UGA), we multiply the final result by 3.

4. **Modulo Operation**: Since the result can be very large, we take modulo 1000000 at each step to prevent overflow and keep numbers manageable.

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the protein string
- **Space Complexity**: O(1) as we only use a fixed-size table

## Example
For protein "MA":
- M (methionine) → 1 codon → 1 way
- A (alanine) → 4 codons → 4 ways
- Stop codon → 3 ways
- Total: 1 × 4 × 3 = 12 possible mRNA strings

