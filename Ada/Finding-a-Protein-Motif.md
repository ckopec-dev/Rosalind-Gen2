# Rosalind Problem: Finding a Protein Motif (Ada Solution)

## Problem Description
Given a protein sequence, find all occurrences of the N-glycosylation motif (N{P}[ST]{P}) in the sequence.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;

procedure Finding_A_Protein_Motif is
   package U_String is new Ada.Strings.Unbounded;
   use U_String;
   
   type Position_Vector is array (Positive range <>) of Positive;
   package Position_Vectors is new Ada.Containers.Vectors (Positive, Positive);
   
   -- Function to check if a character is a valid amino acid
   function Is_Valid_AA(Char : Character) return Boolean is
   begin
      return Char in 'A' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'K' | 'L' | 'M' | 'N' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'V' | 'W' | 'Y';
   end Is_Valid_AA;
   
   -- Function to check if a character is S or T
   function Is_S_or_T(Char : Character) return Boolean is
   begin
      return Char = 'S' or Char = 'T';
   end Is_S_or_T;
   
   -- Function to check if a character is P
   function Is_P(Char : Character) return Boolean is
   begin
      return Char = 'P';
   end Is_P;
   
   -- Function to check if a character is N
   function Is_N(Char : Character) return Boolean is
   begin
      return Char = 'N';
   end Is_N;
   
   -- Function to check if the N-glycosylation motif is found at position
   function Is_Motif(Seq : Unbounded_String; Pos : Positive) return Boolean is
      Length : constant Positive := Length(Seq);
   begin
      -- Check if we have enough characters to form the motif
      if Pos + 3 > Length then
         return False;
      end if;
      
      -- N{P}[ST]{P} - N at position 0, P at position 1, S/T at position 2, P at position 3
      return Is_N(Element(Seq, Pos)) and
             Is_P(Element(Seq, Pos + 1)) and
             Is_S_or_T(Element(Seq, Pos + 2)) and
             Is_P(Element(Seq, Pos + 3));
   end Is_Motif;
   
   -- Function to find all motif positions in a sequence
   function Find_Motif_Positions(Seq : Unbounded_String) return Position_Vector is
      Length : constant Positive := Length(Seq);
      Positions : Position_Vectors.Vector;
      Pos : Positive := 1;
   begin
      while Pos <= Length - 3 loop
         if Is_Motif(Seq, Pos) then
            Positions.Append(Pos);
         end if;
         Pos := Pos + 1;
      end loop;
      
      -- Convert vector to array
      if Positions.Length = 0 then
         return Position_Vector'(1..0 => 0);
      else
         declare
            Result : Position_Vector(1..Positions.Length);
         begin
            for I in 1..Positions.Length loop
               Result(I) := Positions.Element(I);
            end loop;
            return Result;
         end;
      end if;
   end Find_Motif_Positions;
   
   -- Read FASTA format input
   function Read_Fasta return Unbounded_String is
      Line : Unbounded_String;
      Sequence : Unbounded_String := Null_Unbounded_String;
   begin
      while not End_Of_File loop
         Get_Line(Line);
         if Is_Empty(Line) then
            null;
         elsif Element(Line, 1) = '>' then
            -- Header line, skip
            null;
         else
            -- Sequence line
            Sequence := Sequence & Line;
         end if;
      end loop;
      
      return Sequence;
   end Read_Fasta;
   
   -- Main execution
   Sequence : Unbounded_String;
   Motif_Positions : Position_Vector;
   Length : Positive;
   
begin
   -- Read the protein sequence from standard input
   Sequence := Read_Fasta;
   
   -- Find all motif positions
   Motif_Positions := Find_Motif_Positions(Sequence);
   Length := Length(Sequence);
   
   -- Print results
   if Motif_Positions'Length = 0 then
      -- No motifs found
      null;
   else
      for I in Motif_Positions'First..Motif_Positions'Last loop
         Put_Line(Positive'Image(Motif_Positions(I)));
      end loop;
   end if;
   
exception
   when others =>
      Put_Line("Error processing sequence");
end Finding_A_Protein_Motif;
```

## Explanation

This Ada solution:

1. **Reads FASTA format input** from standard input, skipping header lines and collecting sequence data
2. **Implements motif detection** for N-glycosylation motif (N{P}[ST]{P})
3. **Checks each position** in the sequence to see if it matches the pattern:
   - N at position 0
   - P at position 1  
   - S or T at position 2
   - P at position 3
4. **Outputs all starting positions** (1-indexed) where the motif occurs

## Key Features

- Uses **unbounded strings** for flexible sequence handling
- Implements **proper error handling** for edge cases
- **Efficient searching** through the sequence
- **Proper FASTA parsing** that handles multi-line sequences
- **1-indexed output** as required by Rosalind problems

## Usage

Compile and run:
```bash
gnatmake finding_a_protein_motif.adb
./finding_a_protein_motif < input.txt
```

The program will read from standard input and output all motif positions to standard output.

