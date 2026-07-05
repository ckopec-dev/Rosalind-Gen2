# Rosalind Problem: Semiglobal Alignment in Ada

## Problem Understanding

Semiglobal alignment is a variant of sequence alignment where we allow gaps at the beginning and end of sequences, but not in the middle. This means we can have free gaps at the boundaries but must align the core parts of the sequences.

## Solution Approach

I'll implement a semiglobal alignment algorithm using dynamic programming with the following key differences from global alignment:
- No penalty for gaps at the beginning or end
- Only penalties for internal gaps (between aligned characters)
- The alignment score is calculated by filling a matrix where we can start and end gaps freely

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Semiglobal_Alignment is
   
   -- Constants for scoring
   Match_Score : constant := 1;
   Mismatch_Score : constant := -1;
   Gap_Penalty : constant := -1;
   
   -- String type definition
   type DNA_String is array (Positive range <>) of Character;
   
   -- Function to get character at position (handles bounds)
   function Get_Char(S : DNA_String; Pos : Natural) return Character is
   begin
      if Pos = 0 then
         return ' ';
      else
         return S(Pos);
      end if;
   end Get_Char;
   
   -- Main semiglobal alignment function
   procedure Semiglobal_Align(S1, S2 : DNA_String; Score : out Integer) is
      M : constant Natural := S1'Length;
      N : constant Natural := S2'Length;
      
      -- Matrix for dynamic programming (0-indexed)
      type Score_Matrix is array (0..M, 0..N) of Integer;
      Matrix : Score_Matrix;
      
      -- Initialize matrix
      procedure Initialize_Matrix is
      begin
         -- First row: all zeros (free gaps at start of second sequence)
         for J in 0..N loop
            Matrix(0, J) := 0;
         end loop;
         
         -- First column: accumulate gap penalties
         for I in 1..M loop
            Matrix(I, 0) := Matrix(I-1, 0) + Gap_Penalty;
         end loop;
      end Initialize_Matrix;
      
   begin
      Initialize_Matrix;
      
      -- Fill the matrix
      for I in 1..M loop
         for J in 1..N loop
            declare
               Match : Integer;
               Delete : Integer;
               Insert : Integer;
            begin
               -- Match/mismatch score
               if S1(I) = S2(J) then
                  Match := Matrix(I-1, J-1) + Match_Score;
               else
                  Match := Matrix(I-1, J-1) + Mismatch_Score;
               end if;
               
               -- Deletion (gap in first sequence)
               Delete := Matrix(I-1, J) + Gap_Penalty;
               
               -- Insertion (gap in second sequence)
               Insert := Matrix(I, J-1) + Gap_Penalty;
               
               -- Take maximum
               Matrix(I, J) := Integer'Max(Integer'Max(Match, Delete), Insert);
            end;
         end loop;
      end loop;
      
      -- The score is the maximum value in the last row (free gaps at end)
      Score := 0;
      for J in 0..N loop
         if Matrix(M, J) > Score then
            Score := Matrix(M, J);
         end if;
      end loop;
   end Semiglobal_Align;
   
   -- Test procedure
   procedure Test_Semiglobal is
      S1 : DNA_String(1..4) := "ACGT";
      S2 : DNA_String(1..3) := "ACG";
      Score : Integer;
   begin
      Semiglobal_Align(S1, S2, Score);
      Put_Line("Semiglobal alignment score: " & Integer'Image(Score));
   end Test_Semiglobal;
   
begin
   Test_Semiglobal;
end Semiglobal_Alignment;
```

## Key Features of This Implementation

1. **Matrix Initialization**: 
   - First row initialized to 0 (free gaps at start of second sequence)
   - First column filled with cumulative gap penalties (since we can't start with free gaps in first sequence)

2. **Dynamic Programming Transition**:
   - Match/Mismatch: `Matrix(i-1, j-1) + score`
   - Deletion: `Matrix(i-1, j) + gap_penalty` 
   - Insertion: `Matrix(i, j-1) + gap_penalty`

3. **Final Score Calculation**:
   - Since we allow free gaps at the end of the first sequence, we take the maximum value from the last row of the matrix

## Example Usage

For sequences "ACGT" and "ACG":
- The alignment can be: ACG- (with gap in second sequence) 
- Or: AC-G (with gap in first sequence)
- Score would be 2 for match + 1 for another match - 1 penalty = 2

This implementation provides the correct semiglobal alignment score that allows gaps at both ends while properly penalizing internal gaps.