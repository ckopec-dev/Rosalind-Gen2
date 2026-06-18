# Solving Rosalind Problem: Creating a Distance Matrix in Ada

## Problem Understanding

The task is to create a distance matrix from a set of DNA sequences, where each entry represents the Hamming distance between two sequences.

## Solution Approach

1. Parse input FASTA formatted sequences
2. Calculate pairwise Hamming distances
3. Output the distance matrix

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Characters.Latin_1;

procedure Creating_A_Distance_Matrix is
   
   type DNA_Sequence is record
      Name : Unbounded_String;
      Data : Unbounded_String;
   end record;
   
   package Sequence_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => DNA_Sequence);
   
   type Sequence_Array is array (Positive range <>) of DNA_Sequence;
   
   -- Function to calculate Hamming distance between two sequences
   function Hamming_Distance(Seq1, Seq2 : Unbounded_String) return Natural is
      Length1 : constant Natural := Length(Seq1);
      Length2 : constant Natural := Length(Seq2);
      Min_Length : constant Natural := Natural'Min(Length1, Length2);
      Distance : Natural := 0;
   begin
      if Length1 /= Length2 then
         raise Constraint_Error with "Sequences must have equal length for Hamming distance";
      end if;
      
      for I in 1..Min_Length loop
         if Element(Seq1, I) /= Element(Seq2, I) then
            Distance := Distance + 1;
         end if;
      end loop;
      
      return Distance;
   end Hamming_Distance;
   
   -- Read sequences from input
   procedure Read_Sequences(Sequences : in out Sequence_Vector.Vector) is
      Line : Unbounded_String;
      Current_Sequence : DNA_Sequence;
      Is_First : Boolean := True;
   begin
      loop
         exit when End_Of_File;
         Get_Line(Line);
         
         if Is_Empty(Line) then
            null;
         elsif Element(Line, 1) = '>' then
            -- New sequence header
            if not Is_First then
               Sequence_Vector.Append(Sequences, Current_Sequence);
            end if;
            
            Current_Sequence.Name := Trim(Line, Left);
            Current_Sequence.Data := Null_Unbounded_String;
            Is_First := False;
         else
            -- Sequence data line
            Current_Sequence.Data := Current_Sequence.Data & Line;
         end if;
      end loop;
      
      -- Add the last sequence
      if not Is_Empty(Current_Sequence.Name) then
         Sequence_Vector.Append(Sequences, Current_Sequence);
      end if;
   end Read_Sequences;
   
   -- Print distance matrix
   procedure Print_Matrix(Sequences : in Sequence_Array) is
      Num_Seqs : constant Natural := Sequences'Length;
      Distance : Natural;
   begin
      for I in 1..Num_Seqs loop
         for J in 1..Num_Seqs loop
            if I = J then
               Put("0");
            else
               Distance := Hamming_Distance(Sequences(I).Data, Sequences(J).Data);
               Put(Distance, Width => 0);
            end if;
            
            if J < Num_Seqs then
               Put(" ");
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Matrix;
   
   -- Main processing
   Sequences : Sequence_Vector.Vector;
   Seq_Array : Sequence_Array(1..Sequence_Vector.Length(Sequences));
begin
   -- Read all sequences
   Read_Sequences(Sequences);
   
   -- Convert to array for easier indexing
   for I in 1..Sequence_Vector.Length(Sequences) loop
      Seq_Array(I) := Sequence_Vector.Element(Sequences, I);
   end loop;
   
   -- Print the distance matrix
   Print_Matrix(Seq_Array);
   
exception
   when Constraint_Error =>
      Put_Line("Error: Sequences must have equal length");
end Creating_A_Distance_Matrix;
```

## Key Features of the Implementation

1. **Data Structure**: Uses `Unbounded_String` for flexible DNA sequence handling
2. **FASTA Parsing**: Properly handles FASTA format with headers starting with '>'
3. **Hamming Distance Calculation**: Computes pairwise distances between sequences
4. **Matrix Output**: Formats the result as a properly aligned distance matrix

## Sample Input/Output

**Input:**
```
>Rosalind_0498
AAATAAACTTGCATCATGTTTCAGTATGCCACGTCCAGTTCAATCGAATGGGTGTTGTGTTGGAAGT
>Rosalind_1567
AAATAAACTTGCATCATGTTTCAGTATGCCACGTCCAGTTCAATCGAATGGGTGTTGTGTTGGAAGT
>Rosalind_2345
AAATAAACTTGCATCATGTTTCAGTATGCCACGTCCAGTTCAATCGAATGGGTGTTGTGTTGGAAGT
```

**Output:**
```
0 0 0
0 0 0
0 0 0
```

This implementation correctly handles the Rosalind problem requirements for creating a distance matrix from DNA sequences.