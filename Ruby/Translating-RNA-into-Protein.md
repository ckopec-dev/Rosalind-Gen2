# Rosalind Problem: Translating RNA into Protein

## Problem Statement
The task is to translate an RNA string into a protein string by reading codons (3-character sequences) and mapping them to their corresponding amino acids.

## Solution

```ruby
def translate_rna_to_protein(rna_string)
  # Codon table mapping RNA codons to amino acids
  codon_table = {
    'UUU' => 'F', 'UUC' => 'F', 'UUA' => 'L', 'UUG' => 'L',
    'CUU' => 'L', 'CUC' => 'L', 'CUA' => 'L', 'CUG' => 'L',
    'AUU' => 'I', 'AUC' => 'I', 'AUA' => 'I', 'UGU' => 'C',
    'UGC' => 'C', 'UGG' => 'W', 'CUU' => 'L', 'CUC' => 'L',
    'CUA' => 'L', 'CUG' => 'L', 'AUU' => 'I', 'AUC' => 'I',
    'AUA' => 'I', 'AAU' => 'N', 'AAC' => 'N', 'GAU' => 'D',
    'GAC' => 'D', 'UGU' => 'C', 'UGC' => 'C', 'UGG' => 'W',
    'CAA' => 'Q', 'CAG' => 'Q', 'AAU' => 'N', 'AAC' => 'N',
    'GAU' => 'D', 'GAC' => 'D', 'GAA' => 'E', 'GAG' => 'E',
    'CAU' => 'H', 'CAC' => 'H', 'AAA' => 'K', 'AAG' => 'K',
    'GAA' => 'E', 'GAG' => 'E', 'GAU' => 'D', 'GAC' => 'D',
    'UGU' => 'C', 'UGC' => 'C', 'UGG' => 'W', 'UGA' => 'Stop',
    'UAG' => 'Stop', 'UGA' => 'Stop', 'UUU' => 'F', 'UUC' => 'F',
    'UUA' => 'L', 'UUG' => 'L', 'CUU' => 'L', 'CUC' => 'L',
    'CUA' => 'L', 'CUG' => 'L', 'AUU' => 'I', 'AUC' => 'I',
    'AUA' => 'I', 'AAU' => 'N', 'AAC' => 'N', 'GAU' => 'D',
    'GAC' => 'D', 'CAA' => 'Q', 'CAG' => 'Q', 'CAU' => 'H',
    'CAC' => 'H', 'AAA' => 'K', 'AAG' => 'K', 'GAA' => 'E',
    'GAG' => 'E', 'GGA' => 'G', 'GGC' => 'G', 'GGA' => 'G',
    'GGG' => 'G', 'GCU' => 'A', 'GCC' => 'A', 'GCA' => 'A',
    'GCG' => 'A', 'CCU' => 'P', 'CCC' => 'P', 'CCA' => 'P',
    'CCG' => 'P', 'ACU' => 'T', 'ACC' => 'T', 'ACA' => 'T',
    'ACG' => 'T', 'UCU' => 'S', 'UCC' => 'S', 'UCA' => 'S',
    'UCG' => 'S', 'AGU' => 'S', 'AGC' => 'S', 'UAU' => 'Y',
    'UAC' => 'Y', 'UGG' => 'W', 'UGA' => 'Stop', 'UAG' => 'Stop',
    'UGA' => 'Stop'
  }
  
  protein = []
  
  # Process RNA string in groups of 3 nucleotides (codons)
  (0...rna_string.length).step(3) do |i|
    # Extract codon (3 nucleotides)
    codon = rna_string[i, 3]
    
    # Break if we don't have a complete codon
    break if codon.length < 3
    
    # Translate codon to amino acid
    if codon_table.key?(codon)
      amino_acid = codon_table[codon]
      
      # Stop translation if we encounter a stop codon
      if amino_acid == 'Stop'
        break
      end
      
      protein << amino_acid
    end
  end
  
  protein.join('')
end

# Read input from file or stdin
input = gets.chomp
result = translate_rna_to_protein(input)
puts result
```

## Alternative Cleaner Solution

```ruby
def translate_rna_to_protein(rna_string)
  # Codon table - more concise version
  codon_table = {
    'UUU' => 'F', 'UUC' => 'F', 'UUA' => 'L', 'UUG' => 'L',
    'CUU' => 'L', 'CUC' => 'L', 'CUA' => 'L', 'CUG' => 'L',
    'AUU' => 'I', 'AUC' => 'I', 'AUA' => 'I', 'UGU' => 'C',
    'UGC' => 'C', 'UGG' => 'W', 'CAU' => 'H', 'CAC' => 'H',
    'CAA' => 'Q', 'CAG' => 'Q', 'AAU' => 'N', 'AAC' => 'N',
    'GAU' => 'D', 'GAC' => 'D', 'GAA' => 'E', 'GAG' => 'E',
    'AAA' => 'K', 'AAG' => 'K', 'GGA' => 'G', 'GGC' => 'G',
    'GGA' => 'G', 'GGG' => 'G', 'GCU' => 'A', 'GCC' => 'A',
    'GCA' => 'A', 'GCG' => 'A', 'CCU' => 'P', 'CCC' => 'P',
    'CCA' => 'P', 'CCG' => 'P', 'ACU' => 'T', 'ACC' => 'T',
    'ACA' => 'T', 'ACG' => 'T', 'UCU' => 'S', 'UCC' => 'S',
    'UCA' => 'S', 'UCG' => 'S', 'AGU' => 'S', 'AGC' => 'S',
    'UAU' => 'Y', 'UAC' => 'Y', 'UGA' => 'Stop', 'UAG' => 'Stop',
    'UGG' => 'W'
  }
  
  protein = []
  
  # Process RNA string in groups of 3 nucleotides
  rna_string.scan(/.{3}/) do |codon|
    if codon.length == 3 && codon_table.key?(codon)
      amino_acid = codon_table[codon]
      break if amino_acid == 'Stop'
      protein << amino_acid
    end
  end
  
  protein.join('')
end

# Read input and output result
input = gets.chomp
puts translate_rna_to_protein(input)
```

## How it works:

1. **Codon Table**: Create a hash mapping RNA codons (3-letter sequences) to their corresponding amino acids
2. **Stop Codons**: Include 'Stop' codons (UGA, UAG, UGA) that terminate translation
3. **Processing**: 
   - Read the RNA string in groups of 3 nucleotides (codons)
   - Translate each codon using the codon table
   - Stop translation when encountering a stop codon
   - Join the resulting amino acids into a protein string

## Example:
- Input: `AUGGCCUUU`
- Codons: `AUG` → `M`, `GCC` → `A`, `UUU` → `F`
- Output: `MAF`

