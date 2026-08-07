# Rosalind Problem: Align Two Strings Using Affine Gap Penalties (Ada Solution)

## Problem Understanding

This problem requires implementing sequence alignment with affine gap penalties, where gaps have different costs for opening and extending gaps.

## Solution

```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Align_Two_Strings_Using_Affine_Gap_Penalties is
   type Matrix is array (0 .. 100, 0 .. 100) of Integer;
   
   -- Global variables for sequences
   seq1 : String(1 .. 100);
   seq2 : String(1 .. 100);
   len1 : Natural := 0;
   len2 : Natural := 0;
   
   -- Gap penalties
   gap_open : constant Integer := -11;
   gap_extend : constant Integer := -1;
   
   -- Scoring matrix
   score_matrix : Matrix;
   M_matrix : Matrix;  -- Match/mismatch
   X_matrix : Matrix;  -- Gap in sequence 1 (horizontal)
   Y_matrix : Matrix;  -- Gap in sequence 2 (vertical)
   
   procedure Read_Fasta is
      use Ada.Text_IO;
      line : String(1 .. 200);
      pos : Natural := 1;
      first_seq : Boolean := True;
   begin
      loop
         exit when End_Of_File;
         Get_Line(line);
         if line(1) = '>' then
            null; -- Skip header
         else
            if first_seq then
               for i in 1 .. line'Length loop
                  if line(i) /= ' ' and line(i) /= ASCII.LF then
                     seq1(pos) := line(i);
                     pos := pos + 1;
                  end if;
               end loop;
               len1 := pos - 1;
               first_seq := False;
            else
               pos := 1;
               for i in 1 .. line'Length loop
                  if line(i) /= ' ' and line(i) /= ASCII.LF then
                     seq2(pos) := line(i);
                     pos := pos + 1;
                  end if;
               end loop;
               len2 := pos - 1;
               exit;
            end if;
         end if;
      end loop;
   end Read_Fasta;
   
   function Score(a, b : Character) return Integer is
   begin
      if a = b then
         return 2;  -- Match score
      else
         return -3; -- Mismatch score
      end if;
   end Score;
   
   procedure Fill_Matrix is
      i, j : Natural;
      match_score : Integer;
      open_penalty : Integer := gap_open;
      extend_penalty : Integer := gap_extend;
   begin
      -- Initialize matrices
      for i in 0 .. len1 loop
         M_matrix(i, 0) := 0;
         X_matrix(i, 0) := 0;
         Y_matrix(i, 0) := 0;
      end loop;
      
      for j in 0 .. len2 loop
         M_matrix(0, j) := 0;
         X_matrix(0, j) := 0;
         Y_matrix(0, j) := 0;
      end loop;
      
      -- Fill the matrices
      for i in 1 .. len1 loop
         for j in 1 .. len2 loop
            match_score := Score(seq1(i), seq2(j));
            
            -- M matrix (match/mismatch)
            M_matrix(i, j) := max(
               M_matrix(i-1, j-1) + match_score,
               X_matrix(i-1, j-1) + match_score,
               Y_matrix(i-1, j-1) + match_score
            );
            
            -- X matrix (gap in sequence 1)
            X_matrix(i, j) := max(
               M_matrix(i-1, j) + open_penalty,
               X_matrix(i-1, j) + extend_penalty,
               Y_matrix(i-1, j) + open_penalty
            );
            
            -- Y matrix (gap in sequence 2)
            Y_matrix(i, j) := max(
               M_matrix(i, j-1) + open_penalty,
               X_matrix(i, j-1) + open_penalty,
               Y_matrix(i, j-1) + extend_penalty
            );
         end loop;
      end loop;
   end Fill_Matrix;
   
   function Get_Max_Score return Integer is
      max_score : Integer := -1000000;
      i, j : Natural;
   begin
      for i in 0 .. len1 loop
         for j in 0 .. len2 loop
            max_score := max(max_score, M_matrix(i, j));
            max_score := max(max_score, X_matrix(i, j));
            max_score := max(max_score, Y_matrix(i, j));
         end loop;
      end loop;
      return max_score;
   end Get_Max_Score;
   
   function max(a, b : Integer) return Integer is
   begin
      if a > b then
         return a;
      else
         return b;
      end if;
   end max;
   
begin
   Read_Fasta;
   Fill_Matrix;
   Ada.Integer_Text_IO.Put(Get_Max_Score);
   Ada.Text_IO.New_Line;
end Align_Two_Strings_Using_Affine_Gap_Penalties;
```

## Explanation

This Ada solution implements sequence alignment using affine gap penalties:

1. **Data Structures**: 
   - Uses 3 matrices (M, X, Y) to track scores for match/mismatch, gaps in sequence 1, and gaps in sequence 2
   - Implements a scoring function with match score of +2 and mismatch score of -3

2. **Gap Penalties**:
   - Gap opening penalty: -11
   - Gap extension penalty: -1

3. **Matrix Filling**:
   - M_matrix[i,j]: Score for match/mismatch at position (i,j)
   - X_matrix[i,j]: Score for gap in sequence 1 at position (i,j)
   - Y_matrix[i,j]: Score for gap in sequence 2 at position (i,j)

4. **Recurrence Relations**:
   - For M matrix: take maximum from diagonal elements (match/mismatch)
   - For X matrix: take maximum from left, above, and diagonal with appropriate penalties
   - For Y matrix: similar to X but for gaps in sequence 2

5. **Output**: Returns the maximum alignment score achievable with affine gap penalties

The algorithm follows the standard dynamic programming approach for global sequence alignment with affine gap costs, which is more biologically realistic than linear gap penalties for modeling insertions and deletions in biological sequences.