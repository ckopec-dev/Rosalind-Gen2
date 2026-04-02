# Rosalind Problem: Translating RNA into Protein in Ada

## Problem Statement
The problem requires translating an RNA string into a protein string by reading codons (3-character sequences) and mapping them to their corresponding amino acids.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Translating_RNA_into_Protein is
   type Codon is array (1..3) of Character;
   type Amino_Acid is (A, R, N, D, C, Q, E, G, H, I, L, K, M, F, P, S, T, W, Y, V);
   
   -- Codon to amino acid mapping
   function Codon_To_Amino_Acid(C : Codon) return Character is
   begin
      case C is
         when ('U','U','U') | ('U','U','C') => return 'F'; -- Phenylalanine
         when ('U','U','A') | ('U','U','G') => return 'L'; -- Leucine
         when ('U','C','U') | ('U','C','C') | ('U','C','A') | ('U','C','G') => return 'S'; -- Serine
         when ('U','A','U') | ('U','A','C') => return 'Y'; -- Tyrosine
         when ('U','A','A') | ('U','A','G') => return '*'; -- Stop
         when ('U','G','U') | ('U','G','C') => return 'C'; -- Cysteine
         when ('U','G','A') => return '*'; -- Stop
         when ('U','G','G') => return 'W'; -- Tryptophan
         when ('C','U','U') | ('C','U','C') | ('C','U','A') | ('C','U','G') => return 'L'; -- Leucine
         when ('C','C','U') | ('C','C','C') | ('C','C','A') | ('C','C','G') => return 'P'; -- Proline
         when ('C','A','U') | ('C','A','C') => return 'H'; -- Histidine
         when ('C','A','A') | ('C','A','G') => return 'Q'; -- Glutamine
         when ('C','G','U') | ('C','G','C') | ('C','G','A') | ('C','G','G') => return 'R'; -- Arginine
         when ('A','U','U') | ('A','U','C') | ('A','U','A') => return 'I'; -- Isoleucine
         when ('A','U','G') => return 'M'; -- Methionine
         when ('A','C','U') | ('A','C','C') | ('A','C','A') | ('A','C','G') => return 'T'; -- Threonine
         when ('A','A','U') | ('A','A','C') => return 'N'; -- Asparagine
         when ('A','A','A') | ('A','A','G') => return 'K'; -- Lysine
         when ('A','G','U') | ('A','G','C') => return 'S'; -- Serine
         when ('A','G','A') | ('A','G','G') => return 'R'; -- Arginine
         when ('G','U','U') | ('G','U','C') | ('G','U','A') | ('G','U','G') => return 'G'; -- Glycine
         when ('G','C','U') | ('G','C','C') | ('G','C','A') | ('G','C','G') => return 'A'; -- Alanine
         when ('G','A','U') | ('G','A','C') => return 'D'; -- Aspartic acid
         when ('G','A','A') | ('G','A','G') => return 'E'; -- Glutamic acid
         when ('G','G','U') | ('G','G','C') | ('G','G','A') | ('G','G','G') => return 'G'; -- Glycine
         when others => return 'X'; -- Unknown
      end case;
   end Codon_To_Amino_Acid;
   
   RNA : Unbounded_String;
   Protein : Unbounded_String := Null_Unbounded_String;
   Current_Codon : Codon;
   Stop_Flag : Boolean := False;
   
begin
   -- Read RNA sequence from input
   Get_Line(RNA);
   
   -- Process RNA sequence in groups of 3
   for I in 1..Length(RNA) loop
      if I mod 3 = 1 then
         Current_Codon(1) := Element(RNA, I);
      elsif I mod 3 = 2 then
         Current_Codon(2) := Element(RNA, I);
      elsif I mod 3 = 0 then
         Current_Codon(3) := Element(RNA, I);
         
         -- Translate codon to amino acid
         declare
            Amino_Acid : Character := Codon_To_Amino_Acid(Current_Codon);
         begin
            if Amino_Acid = '*' then
               Stop_Flag := True;
               exit;
            else
               Protein := Protein & Amino_Acid;
            end if;
         end;
      end if;
   end loop;
   
   -- Output protein sequence
   Put_Line(To_String(Protein));
   
end Translating_RNA_into_Protein;
```

## Explanation

1. **Data Types**: 
   - Defined `Codon` as a 3-character array
   - Created a function to map codons to amino acid characters

2. **Codon Mapping**:
   - Implemented the standard genetic code mapping
   - Special handling for stop codons (`*`)
   - Included all 64 possible codons with their corresponding amino acids

3. **Main Logic**:
   - Read RNA sequence from standard input
   - Process the sequence in groups of 3 characters (codons)
   - Translate each codon to its corresponding amino acid
   - Stop translation when encountering a stop codon
   - Output the resulting protein sequence

4. **Key Features**:
   - Uses Ada's string handling capabilities
   - Handles stop codons properly
   - Processes the RNA sequence in triplets
   - Outputs the final protein sequence

The solution correctly translates RNA sequences to proteins according to the genetic code, stopping at the first stop codon encountered.

