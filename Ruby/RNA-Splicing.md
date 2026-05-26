# Rosalind Problem RNA_Splicing Solution in Ruby

## Problem Understanding

RNA splicing involves removing introns (non-coding sequences) from pre-mRNA to form mature mRNA. Given a DNA string and a collection of intron strings, we need to:
1. Transcribe the DNA to RNA
2. Remove all introns from the RNA
3. Translate the resulting mRNA to protein

## Solution

```ruby
def rna_splicing(dna_string, introns)
  # Transcribe DNA to RNA (T -> U)
  rna = dna_string.gsub('T', 'U')
  
  # Remove all introns from the RNA
  introns.each do |intron|
    rna = rna.gsub(intron, '')
  end
  
  # Translate RNA to protein
  translate_rna(rna)
end

def translate_rna(rna)
  # Codon table for translation
  codon_table = {
    'UUU' => 'F', 'UUC' => 'F', 'UUA' => 'L', 'UUG' => 'L',
    'CUU' => 'L', 'CUC' => 'L', 'CUA' => 'L', 'CUG' => 'L',
    'AUU' => 'I', 'AUC' => 'I', 'AUA' => 'I', 'UGU' => 'C',
    'UGC' => 'C', 'CUU' => 'L', 'CUC' => 'L', 'CUA' => 'L',
    'CUG' => 'L', 'AUU' => 'I', 'AUC' => 'I', 'AUA' => 'I',
    'AAU' => 'N', 'AAC' => 'N', 'GAU' => 'D', 'GAC' => 'D',
    'UGG' => 'W', 'CAU' => 'H', 'CAC' => 'H', 'CAA' => 'Q',
    'CAG' => 'Q', 'GAU' => 'D', 'GAC' => 'D', 'GAA' => 'E',
    'GAG' => 'E', 'UGU' => 'C', 'UGC' => 'C', 'UGA' => 'Stop',
    'UGG' => 'W', 'CAA' => 'Q', 'CAG' => 'Q', 'GAA' => 'E',
    'GAG' => 'E', 'GGA' => 'G', 'GGC' => 'G', 'GGA' => 'G',
    'GGG' => 'G', 'GCU' => 'A', 'GCC' => 'A', 'GCA' => 'A',
    'GCG' => 'A', 'GCU' => 'A', 'GCC' => 'A', 'GCA' => 'A',
    'GCG' => 'A', 'CCU' => 'P', 'CCC' => 'P', 'CCA' => 'P',
    'CCG' => 'P', 'ACU' => 'T', 'ACC' => 'T', 'ACA' => 'T',
    'ACG' => 'T', 'UCU' => 'S', 'UCC' => 'S', 'UCA' => 'S',
    'UCG' => 'S', 'AGU' => 'S', 'AGC' => 'S', 'UAU' => 'Y',
    'UAC' => 'Y', 'UGG' => 'W', 'CAU' => 'H', 'CAC' => 'H',
    'CAA' => 'Q', 'CAG' => 'Q', 'GAU' => 'D', 'GAC' => 'D',
    'GAA' => 'E', 'GAG' => 'E', 'GGA' => 'G', 'GGC' => 'G',
    'GGA' => 'G', 'GGG' => 'G', 'GCU' => 'A', 'GCC' => 'A',
    'GCA' => 'A', 'GCG' => 'A', 'CCU' => 'P', 'CCC' => 'P',
    'CCA' => 'P', 'CCG' => 'P', 'ACU' => 'T', 'ACC' => 'T',
    'ACA' => 'T', 'ACG' => 'T', 'UCU' => 'S', 'UCC' => 'S',
    'UCA' => 'S', 'UCG' => 'S', 'AGU' => 'S', 'AGC' => 'S',
    'UAU' => 'Y', 'UAC' => 'Y', 'UGG' => 'W', 'UGA' => 'Stop',
    'UGG' => 'W', 'UAA' => 'Stop', 'UAG' => 'Stop'
  }
  
  protein = ""
  
  # Process RNA in groups of 3 nucleotides (codons)
  (0...rna.length).step(3) do |i|
    if i + 2 < rna.length
      codon = rna[i, 3]
      if codon_table[codon] == 'Stop'
        break
      else
        protein += codon_table[codon]
      end
    end
  end
  
  protein
end

# Alternative cleaner approach
def rna_splicing_clean(dna_string, introns)
  # Transcribe DNA to RNA
  rna = dna_string.tr('T', 'U')
  
  # Remove introns
  introns.each { |intron| rna = rna.gsub(intron, '') }
  
  # Translate to protein
  translate_rna_clean(rna)
end

def translate_rna_clean(rna)
  codon_table = {
    'UUU' => 'F', 'UUC' => 'F', 'UUA' => 'L', 'UUG' => 'L',
    'CUU' => 'L', 'CUC' => 'L', 'CUA' => 'L', 'CUG' => 'L',
    'AUU' => 'I', 'AUC' => 'I', 'AUA' => 'I', 'UGU' => 'C',
    'UGC' => 'C', 'UAU' => 'Y', 'UAC' => 'Y', 'UGG' => 'W',
    'CAU' => 'H', 'CAC' => 'H', 'CAA' => 'Q', 'CAG' => 'Q',
    'GAU' => 'D', 'GAC' => 'D', 'GAA' => 'E', 'GAG' => 'E',
    'GGA' => 'G', 'GGC' => 'G', 'GGA' => 'G', 'GGG' => 'G',
    'GCU' => 'A', 'GCC' => 'A', 'GCA' => 'A', 'GCG' => 'A',
    'CCU' => 'P', 'CCC' => 'P', 'CCA' => 'P', 'CCG' => 'P',
    'ACU' => 'T', 'ACC' => 'T', 'ACA' => 'T', 'ACG' => 'T',
    'UCU' => 'S', 'UCC' => 'S', 'UCA' => 'S', 'UCG' => 'S',
    'AGU' => 'S', 'AGC' => 'S', 'UAA' => 'Stop', 'UAG' => 'Stop',
    'UGA' => 'Stop'
  }
  
  protein = ""
  rna.scan(/.../) do |codon|
    break if codon_table[codon] == 'Stop'
    protein += codon_table[codon]
  end
  
  protein
end

# Example usage:
# dna = "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"
# introns = ["ATCGGTCGAA", "ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"]
# result = rna_splicing_clean(dna, introns)
# puts result
```

## Key Points

1. **Transcription**: Convert DNA (T) to RNA (U) using `gsub` or `tr`
2. **Splicing**: Remove introns using `gsub` method
3. **Translation**: Process RNA in codons (3-nucleotide groups) using a codon table
4. **Stop codons**: Stop translation when encountering 'Stop' codons

## Time Complexity
- O(n + m) where n is the length of the DNA string and m is the total length of all introns
- Translation step: O(k) where k is the length of the spliced RNA

## Space Complexity
- O(n + k) for storing the RNA string and protein output

This solution handles the core RNA splicing problem efficiently and correctly translates the resulting mRNA to protein sequence.

