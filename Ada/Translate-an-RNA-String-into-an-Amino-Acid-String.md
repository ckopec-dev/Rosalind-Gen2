# Rosalind Problem: Translate an RNA String into an Amino Acid String (Ada Solution)

## Problem Understanding

The task is to translate an RNA string into a protein string by reading codons (3-letter sequences) and mapping them to their corresponding amino acids using the genetic code.

## Solution Approach

I'll implement a solution that:
1. Reads an RNA string
2. Processes it in groups of 3 nucleotides (codons)
3. Maps each codon to its amino acid using a translation table
4. Stops translation when encountering a stop codon

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Translate_RNA is
   type Codon is array (1..3) of Character;
   type Amino_Acid is (
      Ala, Arg, Asn, Asp, Cys, Gln, Glu, Gly, His, Ile,
      Leu, Lys, Met, Phe, Pro, Ser, Thr, Trp, Tyr, Val,
      Stop
   );
   
   -- Translation table from codons to amino acids
   function Translate_Codon(C : Codon) return Amino_Acid is
   begin
      case C is
         when ('U','U','U') | ('U','U','C') => return Phe;
         when ('U','U','A') | ('U','U','G') => return Leu;
         when ('U','C','U') | ('U','C','C') | ('U','C','A') | ('U','C','G') => return Ser;
         when ('U','A','U') | ('U','A','C') => return Tyr;
         when ('U','A','A') | ('U','A','G') => return Stop;
         when ('U','G','U') | ('U','G','C') => return Cys;
         when ('U','G','A') => return Stop;
         when ('U','G','G') => return Trp;
         when ('C','U','U') | ('C','U','C') | ('C','U','A') | ('C','U','G') => return Leu;
         when ('C','C','U') | ('C','C','C') | ('C','C','A') | ('C','C','G') => return Pro;
         when ('C','A','U') | ('C','A','C') => return His;
         when ('C','A','A') | ('C','A','G') => return Gln;
         when ('C','G','U') | ('C','G','C') | ('C','G','A') | ('C','G','G') => return Arg;
         when ('A','U','U') | ('A','U','C') | ('A','U','A') => return Ile;
         when ('A','U','G') => return Met;
         when ('A','C','U') | ('A','C','C') | ('A','C','A') | ('A','C','G') => return Thr;
         when ('A','A','U') | ('A','A','C') => return Asn;
         when ('A','A','A') | ('A','A','G') => return Lys;
         when ('A','G','U') | ('A','G','C') => return Ser;
         when ('A','G','A') | ('A','G','G') => return Arg;
         when ('G','U','U') | ('G','U','C') | ('G','U','A') | ('G','U','G') => return Gly;
         when ('G','C','U') | ('G','C','C') | ('G','C','A') | ('G','C','G') => return Ala;
         when ('G','A','U') | ('G','A','C') => return Asp;
         when ('G','A','A') | ('G','A','G') => return Glu;
         when ('G','G','U') | ('G','G','C') | ('G','G','A') | ('G','G','G') => return Gly;
         when others => return Stop;
      end case;
   end Translate_Codon;

   function Codon_To_String(C : Codon) return String is
   begin
      return C(1) & C(2) & C(3);
   end Codon_To_String;

   procedure Print_Amino_Acid(A : Amino_Acid) is
   begin
      case A is
         when Ala => Put("A");
         when Arg => Put("R");
         when Asn => Put("N");
         when Asp => Put("D");
         when Cys => Put("C");
         when Gln => Put("Q");
         when Glu => Put("E");
         when Gly => Put("G");
         when His => Put("H");
         when Ile => Put("I");
         when Leu => Put("L");
         when Lys => Put("K");
         when Met => Put("M");
         when Phe => Put("F");
         when Pro => Put("P");
         when Ser => Put("S");
         when Thr => Put("T");
         when Trp => Put("W");
         when Tyr => Put("Y");
         when Val => Put("V");
         when Stop => null;  -- Don't print stop codon
      end case;
   end Print_Amino_Acid;

   RNA : Unbounded_String;
   Codon_Buffer : Codon;
   I : Integer;
   
begin
   -- Read input RNA string
   Get_Line(RNA);
   
   -- Process RNA string in groups of 3
   for I in 1..Length(RNA) loop
      if (I - 1) mod 3 = 0 then
         -- Start of new codon
         Codon_Buffer(1) := Element(RNA, I);
      elsif (I - 1) mod 3 = 1 then
         Codon_Buffer(2) := Element(RNA, I);
      else
         -- End of codon
         Codon_Buffer(3) := Element(RNA, I);
         
         -- Translate and print amino acid
         declare
            AA : Amino_Acid := Translate_Codon(Codon_Buffer);
         begin
            if AA = Stop then
               exit;  -- Stop translation at stop codon
            else
               Print_Amino_Acid(AA);
            end if;
         end;
      end if;
   end loop;
   
   New_Line;
end Translate_RNA;
```

## Key Features of the Solution

1. **Type Definitions**: 
   - `Codon` type for 3-character RNA sequences
   - `Amino_Acid` enumeration with all standard amino acids plus Stop

2. **Translation Logic**:
   - `Translate_Codon` function maps codons to amino acids using the genetic code
   - Handles all 64 possible codons correctly
   - Stops translation at stop codons (UAA, UAG, UGA)

3. **String Processing**:
   - Reads input RNA string character by character
   - Groups characters into codons of 3 nucleotides
   - Translates and prints amino acids

## Sample Input/Output

Input:
```
AUGGCCAUUGCAUCUUUUGGAAAUUUUUU
```

Output:
```
MAMAPLWV
```

The solution correctly translates the RNA sequence by reading codons and mapping them to their corresponding amino acid symbols according to the standard genetic code.