# Rosalind Problem: Alignment-Based Phylogeny - Ada Solution

## Problem Understanding

The alignment-based phylogeny problem involves constructing a phylogenetic tree from multiple DNA sequences using pairwise sequence alignment. The approach typically involves:
1. Computing pairwise distances between sequences using alignment scores
2. Using these distances to construct a phylogenetic tree (usually using neighbor-joining algorithm)

## Solution Implementation

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Alignment_Based_Phylogeny is
   
   -- Define sequence type
   type DNA_Sequence is array (Positive range <>) of Character;
   
   -- Vector for storing sequences
   package Sequence_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => DNA_Sequence);
   
   type Sequence_Array is array (Positive range <>) of DNA_Sequence;
   
   -- Matrix for storing pairwise distances
   type Distance_Matrix is array (Positive range <>) of 
     array (Positive range <>) of Float;
   
   -- Function to compute edit distance (Levenshtein distance)
   function Edit_Distance(Seq1, Seq2 : DNA_Sequence) return Integer is
      M : constant Positive := Seq1'Length;
      N : constant Positive := Seq2'Length;
      D : array (0 .. M, 0 .. N) of Integer;
   begin
      -- Initialize base cases
      for I in 0 .. M loop
         D(I, 0) := I;
      end loop;
      
      for J in 0 .. N loop
         D(0, J) := J;
      end loop;
      
      -- Fill the matrix
      for I in 1 .. M loop
         for J in 1 .. N loop
            if Seq1(I) = Seq2(J) then
               D(I, J) := D(I-1, J-1);
            else
               D(I, J) := Integer'Min(
                  Integer'Min(D(I-1, J), D(I, J-1)),
                  D(I-1, J-1)
               ) + 1;
            end if;
         end loop;
      end loop;
      
      return D(M, N);
   end Edit_Distance;
   
   -- Function to compute normalized distance
   function Normalized_Distance(Seq1, Seq2 : DNA_Sequence) return Float is
      Dist : constant Integer := Edit_Distance(Seq1, Seq2);
      Length : constant Positive := Integer'Max(Seq1'Length, Seq2'Length);
   begin
      if Length = 0 then
         return 0.0;
      else
         return Float(Dist) / Float(Length);
      end if;
   end Normalized_Distance;
   
   -- Function to compute distance matrix
   function Compute_Distance_Matrix(Seqs : Sequence_Array) return Distance_Matrix is
      N : constant Positive := Seqs'Length;
      Dist_Mat : Distance_Matrix(1 .. N, 1 .. N);
   begin
      for I in 1 .. N loop
         for J in 1 .. N loop
            if I = J then
               Dist_Mat(I, J) := 0.0;
            else
               Dist_Mat(I, J) := Normalized_Distance(Seqs(I), Seqs(J));
            end if;
         end loop;
      end loop;
      
      return Dist_Mat;
   end Compute_Distance_Matrix;
   
   -- Function to find minimum distance in matrix (excluding diagonal)
   function Find_Minimum_Distance(Dist_Mat : Distance_Matrix; N : Positive) 
     return (Positive, Positive) is
      Min_Dist : Float := Float'Last;
      Min_I, Min_J : Positive := 1;
   begin
      for I in 1 .. N loop
         for J in 1 .. N loop
            if I /= J and then Dist_Mat(I, J) < Min_Dist then
               Min_Dist := Dist_Mat(I, J);
               Min_I := I;
               Min_J := J;
            end if;
         end loop;
      end loop;
      
      return (Min_I, Min_J);
   end Find_Minimum_Distance;
   
   -- Function to compute neighbor joining matrix
   function Compute_Neighbor_Joining_Matrix(Dist_Mat : Distance_Matrix; N : Positive) 
     return Distance_Matrix is
      NJ_Matrix : Distance_Matrix(1 .. N, 1 .. N);
      Row_Sum : array (1 .. N) of Float := (others => 0.0);
      Col_Sum : array (1 .. N) of Float := (others => 0.0);
   begin
      -- Compute row sums and column sums
      for I in 1 .. N loop
         for J in 1 .. N loop
            Row_Sum(I) := Row_Sum(I) + Dist_Mat(I, J);
            Col_Sum(J) := Col_Sum(J) + Dist_Mat(I, J);
         end loop;
      end loop;
      
      -- Compute neighbor joining matrix
      for I in 1 .. N loop
         for J in 1 .. N loop
            if I /= J then
               NJ_Matrix(I, J) := (Dist_Mat(I, J) - Row_Sum(I) - Col_Sum(J)) / 
                 Float(N - 2);
            else
               NJ_Matrix(I, J) := 0.0;
            end if;
         end loop;
      end loop;
      
      return NJ_Matrix;
   end Compute_Neighbor_Joining_Matrix;
   
   -- Read sequences from input
   function Read_Sequences return Sequence_Array is
      Seqs : Sequence_Array(1 .. 4);  -- Assuming 4 sequences for example
      Line : Unbounded_String;
      I : Positive := 1;
   begin
      -- This would typically read from a file or stdin
      -- For demonstration, we'll hardcode some example sequences
      Seqs(1) := DNA_Sequence'(ASCII.LF, 'A', 'T', 'G', 'C');
      Seqs(2) := DNA_Sequence'(ASCII.LF, 'A', 'T', 'G', 'T');
      Seqs(3) := DNA_Sequence'(ASCII.LF, 'A', 'T', 'C', 'C');
      Seqs(4) := DNA_Sequence'(ASCII.LF, 'A', 'T', 'G', 'A');
      
      return Seqs;
   end Read_Sequences;
   
   -- Main procedure
begin
   Put_Line("Alignment-Based Phylogeny Solution");
   Put_Line("===============================");
   
   -- Read sequences
   declare
      Sequences : constant Sequence_Array := Read_Sequences;
      N : constant Positive := Sequences'Length;
      Distance_Matrix : Distance_Matrix;
   begin
      -- Compute distance matrix
      Distance_Matrix := Compute_Distance_Matrix(Sequences);
      
      Put_Line("Distance Matrix:");
      for I in 1 .. N loop
         for J in 1 .. N loop
            Put(Distance_Matrix(I, J), Fore => 5, Aft => 3, Exp => 0);
         end loop;
         New_Line;
      end loop;
      
      -- Find minimum distance pair (simplified approach)
      declare
         Min_I, Min_J : constant (Positive, Positive) := 
           Find_Minimum_Distance(Distance_Matrix, N);
      begin
         Put_Line("Minimum distance pair: " & 
                  Integer'Image(Min_I) & ", " & 
                  Integer'Image(Min_J));
      end;
      
   end;
   
end Alignment_Based_Phylogeny;
```

## Explanation

This Ada solution implements a basic alignment-based phylogeny approach with the following components:

1. **Edit Distance Calculation**: Implements the Levenshtein distance algorithm to compute pairwise sequence distances
2. **Distance Normalization**: Normalizes distances by sequence length for meaningful comparison
3. **Distance Matrix Computation**: Builds a matrix of all pairwise distances between sequences
4. **Neighbor-Joining Framework**: Includes functions for computing neighbor-joining matrices (though the full algorithm is simplified)

## Key Features

- **Type Safety**: Uses Ada's strong typing system with proper array declarations
- **Modular Design**: Separates concerns into different functions
- **Memory Management**: Uses arrays appropriately without requiring explicit memory management
- **Error Handling**: Includes basic error checking for edge cases
- **Clean Output**: Formats results clearly for interpretation

## Usage Notes

This implementation provides a foundation that can be extended to:
- Read sequences from FASTA format files
- Implement full neighbor-joining algorithm
- Add more sophisticated distance metrics
- Handle larger phylogenetic trees

The solution demonstrates core concepts of bioinformatics sequence alignment and phylogenetic tree construction in Ada's type-safe environment.