# Rosalind Problem: Inferring mRNA from Protein

## Problem Description
Given an amino acid string, determine the number of possible mRNA strings from which it could have been translated, modulo 1000000.

## Solution

```ruby
def infer_mrna_from_protein(protein_string)
  # Codon table: number of codons for each amino acid
  codon_counts = {
    'A' => 4,  # GCA, GCC, GCG, GCU
    'C' => 2,  # UGC, UGU
    'D' => 2,  # GAU, GAC
    'E' => 2,  # GAA, GAG
    'F' => 2,  # UUC, UUU
    'G' => 4,  # GGA, GGC, GGG, GGU
    'H' => 2,  # CAC, CAU
    'I' => 3,  # AUU, AUC, AUA
    'K' => 2,  # AAA, AAG
    'L' => 6,  # UUA, UUG, CUU, CUC, CUA, CUG
    'M' => 1,  # AUG
    'N' => 2,  # AAU, AAC
    'P' => 4,  # CCA, CCC, CCG, CCU
    'Q' => 2,  # CAA, CAG
    'R' => 6,  # AGA, AGG, CGA, CGC, CGG, CGU
    'S' => 6,  # AGU, AGC, UCA, UCC, UCG, UCU
    'T' => 4,  # ACA, ACC, ACG, ACU
    'V' => 4,  # GUA, GUC, GUG, GUU
    'W' => 1,  # UGG
    'Y' => 2,  # UAC, UAU
    '*' => 3   # UAA, UAG, UGA (stop codons)
  }
  
  result = 1
  modulo = 1000000
  
  # For each amino acid in the protein string
  protein_string.each_char do |amino_acid|
    # Multiply by the number of possible codons for this amino acid
    result = (result * codon_counts[amino_acid]) % modulo
  end
  
  # Don't forget to account for the stop codon
  result = (result * 3) % modulo
  
  result
end

# Read input from file or stdin
input = gets.chomp
puts infer_mrna_from_protein(input)
```

## Explanation

1. **Codon Count Table**: Each amino acid can be encoded by multiple codons (3-letter RNA sequences). I've created a hash mapping each amino acid to the number of possible codons that encode it.

2. **Calculation Process**: 
   - Start with result = 1
   - For each amino acid in the protein string, multiply the result by the number of possible codons for that amino acid
   - Apply modulo 1000000 at each step to prevent integer overflow
   - Finally, multiply by 3 for the stop codons (there are 3 possible stop codons: UAA, UAG, UGA)

3. **Key Points**:
   - The problem asks for the number of possible mRNA strings, not the actual strings
   - Each amino acid contributes its codon count to the total possibilities
   - The final multiplication by 3 accounts for the stop codon

## Example Usage

If the input is "MA":
- M can be encoded by 1 codon (AUG)
- A can be encoded by 4 codons (GCA, GCC, GCG, GCU)
- Stop codon adds 3 possibilities
- Total = 1 × 4 × 3 = 12

The solution handles the modulo arithmetic to prevent integer overflow and returns the correct count of possible mRNA strings.

