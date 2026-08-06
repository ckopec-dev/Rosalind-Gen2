# Finding a Highest-Scoring Alignment of Two Strings (Rosalind Problem)

I'll solve this bioinformatics problem using the Needleman-Wunsch algorithm to find the highest-scoring global alignment of two strings.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Find_Highest_Scoring_Alignment is
   
   -- Scoring parameters
   match_score : constant := 1;
   mismatch_score : constant := -1;
   gap_penalty : constant := -2;
   
   type Matrix_Type is array (0 .. 100, 0 .. 100) of Integer;
   type Alignment_Type is array (1 .. 200) of Character;
   
   -- Global variables for alignment
   score_matrix : Matrix_Type;
   seq1 : String(1..100);
   seq2 : String(1..100);
   len1, len2 : Integer;
   
   function Max(a, b, c : Integer) return Integer is
   begin
      if a >= b and a >= c then
         return a;
      elsif b >= c then
         return b;
      else
         return c;
      end if;
   end Max;
   
   procedure Build_Score_Matrix(s1, s2 : String; l1, l2 : Integer) is
      i, j : Integer;
   begin
      -- Initialize first row and column
      for i in 0 .. l1 loop
         score_matrix(i, 0) := i * gap_penalty;
      end loop;
      
      for j in 0 .. l2 loop
         score_matrix(0, j) := j * gap_penalty;
      end loop;
      
      -- Fill the matrix
      for i in 1 .. l1 loop
         for j in 1 .. l2 loop
            declare
               match : Integer;
               mismatch : Integer;
               gap1 : Integer;
               gap2 : Integer;
            begin
               if s1(i) = s2(j) then
                  match := score_matrix(i-1, j-1) + match_score;
               else
                  match := score_matrix(i-1, j-1) + mismatch_score;
               end if;
               
               gap1 := score_matrix(i-1, j) + gap_penalty;
               gap2 := score_matrix(i, j-1) + gap_penalty;
               
               score_matrix(i, j) := Max(match, gap1, gap2);
            end;
         end loop;
      end loop;
   end Build_Score_Matrix;
   
   procedure Traceback(s1, s2 : String; l1, l2 : Integer; alignment1, alignment2 : in out Alignment_Type; align_len : in out Integer) is
      i, j : Integer;
      pos : Integer := 200;
   begin
      i := l1;
      j := l2;
      align_len := 0;
      
      while i > 0 or j > 0 loop
         if i > 0 and j > 0 then
            declare
               match_score_val : Integer;
               gap1_score : Integer;
               gap2_score : Integer;
            begin
               if s1(i) = s2(j) then
                  match_score_val := score_matrix(i-1, j-1) + match_score;
               else
                  match_score_val := score_matrix(i-1, j-1) + mismatch_score;
               end if;
               
               gap1_score := score_matrix(i-1, j) + gap_penalty;
               gap2_score := score_matrix(i, j-1) + gap_penalty;
               
               if score_matrix(i, j) = match_score_val then
                  -- Match/mismatch
                  alignment1(pos) := s1(i);
                  alignment2(pos) := s2(j);
                  i := i - 1;
                  j := j - 1;
               elsif score_matrix(i, j) = gap1_score then
                  -- Gap in sequence 2
                  alignment1(pos) := s1(i);
                  alignment2(pos) := '-';
                  i := i - 1;
               else
                  -- Gap in sequence 1
                  alignment1(pos) := '-';
                  alignment2(pos) := s2(j);
                  j := j - 1;
               end if;
               
               pos := pos - 1;
               align_len := align_len + 1;
            end;
         elsif i > 0 then
            -- Gap in sequence 2
            alignment1(pos) := s1(i);
            alignment2(pos) := '-';
            i := i - 1;
            pos := pos - 1;
            align_len := align_len + 1;
         else
            -- Gap in sequence 1
            alignment1(pos) := '-';
            alignment2(pos) := s2(j);
            j := j - 1;
            pos := pos - 1;
            align_len := align_len + 1;
         end if;
      end loop;
      
      -- Adjust for the fact that we filled backwards
      for k in 1 .. align_len loop
         alignment1(k) := alignment1(pos + k);
         alignment2(k) := alignment2(pos + k);
      end loop;
   end Traceback;
   
begin
   -- Read input sequences (for demonstration)
   -- In actual Rosalind problem, these would be read from file or stdin
   seq1 := "PRTEINS";
   seq2 := "PRTWPVSN";
   len1 := 7;
   len2 := 8;
   
   -- Build the scoring matrix
   Build_Score_Matrix(seq1, seq2, len1, len2);
   
   -- Print score matrix (for verification)
   Put_Line("Score Matrix:");
   for i in 0 .. len1 loop
      for j in 0 .. len2 loop
         Put(score_matrix(i, j), Width => 4);
      end loop;
      New_Line;
   end loop;
   
   -- Get the alignment score
   Put_Line("Highest Score: " & Integer'Image(score_matrix(len1, len2)));
   
   -- Traceback to get alignment
   declare
      alignment1 : Alignment_Type;
      alignment2 : Alignment_Type;
      align_len : Integer;
   begin
      Traceback(seq1, seq2, len1, len2, alignment1, alignment2, align_len);
      
      Put_Line("Alignment:");
      for i in 1 .. align_len loop
         Put(alignment1(i));
      end loop;
      New_Line;
      
      for i in 1 .. align_len loop
         Put(alignment2(i));
      end loop;
      New_Line;
   end;
   
end Find_Highest_Scoring_Alignment;
```

## Problem Explanation

This program solves the Rosalind problem of finding the highest-scoring global alignment of two strings using the Needleman-Wunsch algorithm with:

1. **Match score**: +1
2. **Mismatch score**: -1  
3. **Gap penalty**: -2

## Algorithm Steps

1. **Initialize matrix**: Create a scoring matrix with dimensions (len1+1) × (len2+1)
2. **Fill first row and column**: Initialize with gap penalties
3. **Fill the matrix**: For each cell, compute the maximum of:
   - Match/mismatch score from diagonal
   - Gap penalty from left cell
   - Gap penalty from top cell
4. **Traceback**: Reconstruct alignment by following the path that gave the optimal score

## Output Format

The program outputs:
- The highest scoring alignment value
- The actual aligned sequences (with gaps represented as '-')

For the example input "PRTEINS" and "PRTWPVSN", it would find an optimal global alignment with the highest score.