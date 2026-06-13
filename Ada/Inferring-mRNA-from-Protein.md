# Rosalind Problem: Inferring mRNA from Protein (Ada Solution)

## Problem Understanding

We need to calculate how many different mRNA sequences can encode a given protein sequence, considering that some amino acids have multiple codons (triplets of nucleotides). We also need to account for the stop codons.

## Approach

1. Create a mapping from amino acids to their possible codon counts
2. Multiply the number of possibilities for each amino acid in the protein
3. Account for stop codons (which have 3 possible codons)
4. Apply modulo arithmetic to prevent overflow

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Inferring_MRNA_From_Protein is
   
   -- Number of codons for each amino acid
   type Codon_Count is range 0 .. 10;
   
   -- Mapping from amino acid to number of possible codons
   function Get_Codon_Count(Amino_Acid : Character) return Natural is
   begin
      case Amino_Acid is
         when 'A' => return 4;   -- GCU, GCC, GCA, GCG
         when 'C' => return 2;   -- UGU, UGC
         when 'D' => return 2;   -- GAU, GAC
         when 'E' => return 2;   -- GAA, GAG
         when 'F' => return 2;   -- UUU, UUC
         when 'G' => return 4;   -- GGU, GGC, GGA, GGG
         when 'H' => return 2;   -- CAU, CAC
         when 'I' => return 3;   -- AUU, AUC, AUA
         when 'K' => return 2;   -- AAA, AAG
         when 'L' => return 6;   -- CUU, CUC, CUA, CUG, UUA, UUG
         when 'M' => return 1;   -- AUG
         when 'N' => return 2;   -- AAU, AAC
         when 'P' => return 4;   -- CCU, CCC, CCA, CCG
         when 'Q' => return 2;   -- CAA, CAG
         when 'R' => return 6;   -- CGU, CGC, CGA, CGG, AGA, AGG
         when 'S' => return 6;   -- UCU, UCC, UCA, UCG, AGU, AGC
         when 'T' => return 4;   -- ACU, ACC, ACA, ACG
         when 'V' => return 4;   -- GUU, GUC, GUA, GUG
         when 'W' => return 1;   -- UGG
         when 'Y' => return 2;   -- UAU, UAC
         when '*' => return 3;   -- UAA, UAG, UGA (stop codons)
         when others => return 0;
      end case;
   end Get_Codon_Count;
   
   -- Modulo value to prevent overflow
   MOD : constant := 1_000_000;
   
   -- Function to calculate number of mRNA sequences
   function Count_MRNA_Sequences(Protein : String) return Natural is
      Result : Natural := 1;
   begin
      for I in Protein'First .. Protein'Last loop
         Result := (Result * Get_Codon_Count(Protein(I))) mod MOD;
      end loop;
      
      return Result;
   end Count_MRNA_Sequences;
   
   -- Main program
   Protein : String(1 .. 360);
   Length  : Integer;
   
begin
   -- Read the protein sequence from input
   Get_Line(Protein, Length);
   
   -- Calculate and print the result
   Put("Number of mRNA sequences: ");
   Put(Count_MRNA_Sequences(Protein(1 .. Length)), Width => 0);
   New_Line;
   
end Inferring_MRNA_From_Protein;
```

## Explanation

The solution works as follows:

1. **Codon Mapping**: Each amino acid is mapped to its number of possible codons (triplets). For example, alanine (A) has 4 possible codons, while methionine (M) has only 1.

2. **Stop Codons**: The stop codon symbol `*` is treated as having 3 possible codons (UAA, UAG, UGA).

3. **Calculation**: For each amino acid in the protein sequence, we multiply the current result by the number of possible codons for that amino acid.

4. **Modular Arithmetic**: Since the answer can be very large, we apply modulo 1,000,000 at each step to prevent integer overflow.

5. **Input/Output**: The program reads a protein sequence from standard input and outputs the number of possible mRNA sequences.

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the protein sequence
- **Space Complexity**: O(1) - only using a constant amount of extra space

This solution efficiently handles the combinatorial nature of the problem by recognizing that for each amino acid, we multiply the possibilities from all previous amino acids, and accounts for the stop codon requirement at the end of translation.