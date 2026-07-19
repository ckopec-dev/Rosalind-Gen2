# Rosalind Problem: Sequence a Peptide (Ada Solution)

## Problem Understanding

The task is to find all possible RNA sequences that could encode a given peptide sequence, considering the genetic code and degeneracy of codons.

## Solution Approach

I'll implement a solution that:
1. Maps amino acids to their possible codons
2. Generates all combinations of codons for the given peptide
3. Returns the count of valid RNA sequences

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Sequence_A_Peptide is
   
   -- Genetic code mapping amino acids to codons
   type Codon is array(1..3) of Character;
   type Codon_Vector is array(1..4) of Codon;
   
   -- Amino acid to codons mapping (simplified for this problem)
   function Get_Codons(Amino_Acid : Character) return Codon_Vector is
      Result : Codon_Vector;
      Count  : Integer := 0;
   begin
      case Amino_Acid is
         when 'A' => -- Alanine
            Result(1) := ('G', 'C', 'A');
            Result(2) := ('G', 'C', 'C');
            Result(3) := ('G', 'C', 'G');
            Result(4) := ('G', 'C', 'T');
            Count := 4;
         when 'C' => -- Cysteine
            Result(1) := ('T', 'G', 'M');
            Result(2) := ('T', 'G', 'T');
            Count := 2;
         when 'D' => -- Aspartic acid
            Result(1) := ('G', 'A', 'A');
            Result(2) := ('G', 'A', 'C');
            Count := 2;
         when 'E' => -- Glutamic acid
            Result(1) := ('G', 'A', 'A');
            Result(2) := ('G', 'A', 'C');
            Count := 2;
         when 'F' => -- Phenylalanine
            Result(1) := ('T', 'T', 'T');
            Result(2) := ('T', 'T', 'C');
            Count := 2;
         when 'G' => -- Glycine
            Result(1) := ('G', 'G', 'A');
            Result(2) := ('G', 'G', 'C');
            Result(3) := ('G', 'G', 'G');
            Result(4) := ('G', 'G', 'T');
            Count := 4;
         when 'H' => -- Histidine
            Result(1) := ('C', 'A', 'A');
            Result(2) := ('C', 'A', 'C');
            Count := 2;
         when 'I' => -- Isoleucine
            Result(1) := ('A', 'T', 'A');
            Result(2) := ('A', 'T', 'C');
            Result(3) := ('A', 'T', 'G');
            Count := 3;
         when 'K' => -- Lysine
            Result(1) := ('A', 'A', 'A');
            Result(2) := ('A', 'A', 'C');
            Count := 2;
         when 'L' => -- Leucine
            Result(1) := ('C', 'T', 'A');
            Result(2) := ('C', 'T', 'C');
            Result(3) := ('C', 'T', 'G');
            Result(4) := ('C', 'T', 'T');
            Count := 4;
         when 'M' => -- Methionine
            Result(1) := ('A', 'T', 'G');
            Count := 1;
         when 'N' => -- Asparagine
            Result(1) := ('A', 'A', 'A');
            Result(2) := ('A', 'A', 'C');
            Count := 2;
         when 'P' => -- Proline
            Result(1) := ('C', 'C', 'A');
            Result(2) := ('C', 'C', 'C');
            Result(3) := ('C', 'C', 'G');
            Result(4) := ('C', 'C', 'T');
            Count := 4;
         when 'Q' => -- Glutamine
            Result(1) := ('C', 'A', 'A');
            Result(2) := ('C', 'A', 'C');
            Count := 2;
         when 'R' => -- Arginine
            Result(1) := ('A', 'G', 'A');
            Result(2) := ('A', 'G', 'C');
            Result(3) := ('A', 'G', 'G');
            Result(4) := ('A', 'G', 'T');
            Count := 4;
         when 'S' => -- Serine
            Result(1) := ('T', 'C', 'A');
            Result(2) := ('T', 'C', 'C');
            Result(3) := ('T', 'C', 'G');
            Result(4) := ('T', 'C', 'T');
            Count := 4;
         when 'T' => -- Threonine
            Result(1) := ('A', 'C', 'A');
            Result(2) := ('A', 'C', 'C');
            Result(3) := ('A', 'C', 'G');
            Result(4) := ('A', 'C', 'T');
            Count := 4;
         when 'V' => -- Valine
            Result(1) := ('G', 'T', 'A');
            Result(2) := ('G', 'T', 'C');
            Result(3) := ('G', 'T', 'G');
            Result(4) := ('G', 'T', 'T');
            Count := 4;
         when 'W' => -- Tryptophan
            Result(1) := ('T', 'G', 'G');
            Count := 1;
         when 'Y' => -- Tyrosine
            Result(1) := ('T', 'A', 'C');
            Result(2) := ('T', 'A', 'T');
            Count := 2;
         when 'Stop' => -- Stop codons
            Result(1) := ('T', 'A', 'A');
            Result(2) := ('T', 'A', 'G');
            Result(3) := ('T', 'G', 'A');
            Count := 3;
         when others =>
            Count := 0;
      end case;
      
      -- Return only the valid codons (not the full array)
      return Result;
   end Get_Codons;

   -- Count total combinations
   function Count_Combinations(Peptide : String) return Integer is
      Total : Integer := 1;
      I     : Integer;
   begin
      for I in 1..Peptide'Length loop
         declare
            Amino_Acid : Character := Peptide(I);
            Codon_Count : Integer := 0;
         begin
            case Amino_Acid is
               when 'A' => Codon_Count := 4;
               when 'C' => Codon_Count := 2;
               when 'D' => Codon_Count := 2;
               when 'E' => Codon_Count := 2;
               when 'F' => Codon_Count := 2;
               when 'G' => Codon_Count := 4;
               when 'H' => Codon_Count := 2;
               when 'I' => Codon_Count := 3;
               when 'K' => Codon_Count := 2;
               when 'L' => Codon_Count := 6; -- Note: L has 6 codons in actual code
               when 'M' => Codon_Count := 1;
               when 'N' => Codon_Count := 2;
               when 'P' => Codon_Count := 4;
               when 'Q' => Codon_Count := 2;
               when 'R' => Codon_Count := 6; -- Note: R has 6 codons in actual code
               when 'S' => Codon_Count := 6; -- Note: S has 6 codons in actual code
               when 'T' => Codon_Count := 4;
               when 'V' => Codon_Count := 4;
               when 'W' => Codon_Count := 1;
               when 'Y' => Codon_Count := 2;
               when others => Codon_Count := 0;
            end case;
            
            Total := Total * Codon_Count;
         end;
      end loop;
      
      return Total;
   end Count_Combinations;

begin
   -- Example usage
   declare
      Peptide : constant String := "MA";
      Result  : Integer;
   begin
      Result := Count_Combinations(Peptide);
      Put("Number of possible RNA sequences for peptide ");
      Put(Peptide);
      Put(": ");
      Put(Result, Width => 0);
      New_Line;
   end;
   
end Sequence_A_Peptide;
```

## Key Points

1. **Genetic Code Implementation**: The solution maps each amino acid to its possible codons based on the standard genetic code.

2. **Combinatorial Approach**: For each amino acid in the peptide, we multiply by the number of possible codons that encode it.

3. **Degeneracy Handling**: Some amino acids have multiple codons (e.g., Leucine has 6 codons), which affects the total count.

4. **Edge Cases**: The solution handles various amino acids and their codon degeneracy.

## Example Output

For a peptide like "MA" (Methionine-Alanine):
- M (Met) = 1 codon
- A (Ala) = 4 codons  
- Total combinations = 1 × 4 = 4

This approach efficiently calculates the number of possible RNA sequences that could encode any given peptide sequence.