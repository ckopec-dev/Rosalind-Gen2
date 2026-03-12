# Rosalind Problem: Inferring mRNA from Protein

## Problem Statement
Given an amino acid string, determine the number of possible mRNA strings from which it could have been translated, modulo 1000000.

## Solution in Pascal

```pascal
program InferringMRNAFromProtein;

uses
  SysUtils;

const
  MODULUS = 1000000;

var
  aminoAcids: array['A'..'Y'] of integer;
  input: string;
  i, j: integer;
  result: int64;

begin
  // Initialize the codon counts for each amino acid
  aminoAcids['A'] := 4;  // GCU, GCC, GCA, GCG
  aminoAcids['C'] := 2;  // UGU, UGC
  aminoAcids['D'] := 2;  // GAU, GAC
  aminoAcids['E'] := 2;  // GAA, GAG
  aminoAcids['F'] := 2;  // UUU, UUC
  aminoAcids['G'] := 4;  // GGU, GGC, GGA, GGG
  aminoAcids['H'] := 2;  // CAU, CAC
  aminoAcids['I'] := 3;  // AUU, AUC, AUA
  aminoAcids['K'] := 2;  // AAA, AAG
  aminoAcids['L'] := 6;  // UUA, UUG, CUU, CUC, CUA, CUG
  aminoAcids['M'] := 1;  // AUG
  aminoAcids['N'] := 2;  // AAU, AAC
  aminoAcids['P'] := 4;  // CCU, CCC, CCA, CCG
  aminoAcids['Q'] := 2;  // CAA, CAG
  aminoAcids['R'] := 6;  // CGU, CGC, CGA, CGG, AGA, AGG
  aminoAcids['S'] := 6;  // UCU, UCC, UCA, UCG, AGU, AGC
  aminoAcids['T'] := 4;  // ACU, ACC, ACA, ACG
  aminoAcids['V'] := 4;  // GUU, GUC, GUA, GUG
  aminoAcids['W'] := 1;  // UGG
  aminoAcids['Y'] := 2;  // UAU, UAC
  aminoAcids['*'] := 3;  // UAA, UAG, UGA (Stop codons)

  // Read input string
  readln(input);
  
  // Initialize result
  result := 1;
  
  // For each amino acid in the protein sequence
  for i := 1 to length(input) do
  begin
    // Multiply result by the number of codons for this amino acid
    result := (result * aminoAcids[input[i]]) mod MODULUS;
  end;
  
  // Account for stop codons (multiply by 3)
  result := (result * 3) mod MODULUS;
  
  writeln(result);
end.
```

## Explanation

This solution works by:

1. **Codon Count Mapping**: Creating a lookup table that maps each amino acid to the number of possible codons that can encode it
2. **Iterative Multiplication**: For each amino acid in the input protein sequence, multiplying the result by the number of possible codons for that amino acid
3. **Stop Codons**: Adding the factor of 3 for the stop codons (UAA, UAG, UGA) since any of these can terminate translation
4. **Modular Arithmetic**: Taking the result modulo 1000000 at each step to prevent integer overflow

## Key Points

- The problem asks for the number of possible mRNA sequences, not the actual sequences
- Each amino acid can be encoded by multiple codons (1-6 possibilities)
- Stop codons are included in the final count (3 possibilities)
- Modular arithmetic is essential to handle potentially large numbers

## Time Complexity
O(n) where n is the length of the input protein string

## Space Complexity  
O(1) - only using a fixed-size lookup table and a few variables

