# Rosalind Problem: Global Alignment with Constant Gap Penalty

## Problem Description
Find a global alignment of two strings using a constant gap penalty.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Global_Alignment is
   type Matrix is array (0 .. 100, 0 .. 100) of Integer;
   
   -- Function to compute maximum of two integers
   function Max(a, b : Integer) return Integer is
   begin
      if a > b then
         return a;
      else
         return b;
      end if;
   end Max;
   
   -- Function to compute maximum of three integers
   function Max3(a, b, c : Integer) return Integer is
   begin
      return Max(Max(a, b), c);
   end Max3;
   
   -- Global alignment with constant gap penalty
   procedure Align(s1, s2 : String; gap_penalty : Integer; score : out Matrix) is
      m : constant Integer := s1'Length;
      n : constant Integer := s2'Length;
      match_score : Integer;
   begin
      -- Initialize the matrix
      for i in 0 .. m loop
         score(i, 0) := i * gap_penalty;
      end loop;
      
      for j in 0 .. n loop
         score(0, j) := j * gap_penalty;
      end loop;
      
      -- Fill the matrix
      for i in 1 .. m loop
         for j in 1 .. n loop
            if s1(i) = s2(j) then
               match_score := 1;  -- Match score (assuming 1 for match)
            else
               match_score := -1; -- Mismatch penalty (assuming -1)
            end if;
            
            score(i, j) := Max3(
               score(i-1, j) + gap_penalty,      -- Deletion
               score(i, j-1) + gap_penalty,      -- Insertion
               score(i-1, j-1) + match_score     -- Substitution
            );
         end loop;
      end loop;
   end Align;
   
   -- Function to get the optimal alignment
   function Get_Alignment(s1, s2 : String; gap_penalty : Integer) return String is
      m : constant Integer := s1'Length;
      n : constant Integer := s2'Length;
      score : Matrix;
      i, j : Integer;
      alignment : String(1 .. 2 * (m + n));
      pos : Integer := alignment'Last;
      match_score : Integer;
   begin
      -- Initialize matrix
      for i in 0 .. m loop
         score(i, 0) := i * gap_penalty;
      end loop;
      
      for j in 0 .. n loop
         score(0, j) := j * gap_penalty;
      end loop;
      
      -- Fill the matrix
      for i in 1 .. m loop
         for j in 1 .. n loop
            if s1(i) = s2(j) then
               match_score := 1;
            else
               match_score := -1;
            end if;
            
            score(i, j) := Max3(
               score(i-1, j) + gap_penalty,
               score(i, j-1) + gap_penalty,
               score(i-1, j-1) + match_score
            );
         end loop;
      end loop;
      
      -- Backtrack to find alignment
      i := m;
      j := n;
      
      while i > 0 or j > 0 loop
         if i > 0 and j > 0 then
            if s1(i) = s2(j) then
               match_score := 1;
            else
               match_score := -1;
            end if;
            
            if score(i, j) = score(i-1, j-1) + match_score then
               -- Match or mismatch
               alignment(pos) := s1(i);
               pos := pos - 1;
               alignment(pos) := s2(j);
               pos := pos - 1;
               i := i - 1;
               j := j - 1;
            elsif score(i, j) = score(i-1, j) + gap_penalty then
               -- Deletion
               alignment(pos) := '-';
               pos := pos - 1;
               alignment(pos) := s1(i);
               pos := pos - 1;
               i := i - 1;
            else
               -- Insertion
               alignment(pos) := s2(j);
               pos := pos - 1;
               alignment(pos) := '-';
               pos := pos - 1;
               j := j - 1;
            end if;
         elsif i > 0 then
            -- Deletion
            alignment(pos) := s1(i);
            pos := pos - 1;
            alignment(pos) := '-';
            pos := pos - 1;
            i := i - 1;
         else
            -- Insertion
            alignment(pos) := '-';
            pos := pos - 1;
            alignment(pos) := s2(j);
            pos := pos - 1;
            j := j - 1;
         end if;
      end loop;
      
      return alignment(pos+1 .. alignment'Last);
   end Get_Alignment;
   
   -- Main procedure
begin
   -- Example input (replace with actual problem input)
   declare
      seq1 : constant String := "PRTEINS";
      seq2 : constant String := "PRTWPSEIN";
      gap_pen : constant Integer := -2;
      score_matrix : Matrix;
      result : String(1 .. 200);
   begin
      -- Compute the alignment score matrix
      Align(seq1, seq2, gap_pen, score_matrix);
      
      -- Print the final score
      Put_Line("Alignment Score: " & Integer'Image(score_matrix(seq1'Length, seq2'Length)));
      
      -- Get and print the alignment
      result := Get_Alignment(seq1, seq2, gap_pen);
      Put_Line("Alignment:");
      Put_Line(result);
   end;
   
end Global_Alignment;
```

## Explanation

This Ada solution implements global sequence alignment with a constant gap penalty:

### Key Features:
1. **Matrix Initialization**: The scoring matrix is initialized with gap penalties along the borders
2. **Dynamic Programming**: Fills the matrix using the recurrence relation:
   - Match/Mismatch: `score[i-1][j-1] + match_score`
   - Deletion: `score[i-1][j] + gap_penalty` 
   - Insertion: `score[i][j-1] + gap_penalty`
3. **Backtracking**: Reconstructs the optimal alignment by tracing back through the matrix
4. **Gap Penalty**: Uses a constant penalty for gaps throughout the alignment

### Time and Space Complexity:
- **Time Complexity**: O(m×n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m×n) for the dynamic programming matrix

### Usage:
To use with Rosalind input, you would need to modify the main procedure to read from standard input or files, and adjust the gap penalty as specified in the problem.

The solution handles all three possible operations (match/mismatch, deletion, insertion) with appropriate scoring and backtracking to reconstruct the optimal alignment.