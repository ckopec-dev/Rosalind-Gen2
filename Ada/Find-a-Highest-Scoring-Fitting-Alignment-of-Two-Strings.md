# Rosalind Problem: Find a Highest-Scoring Fitting Alignment of Two Strings (Ada Solution)

## Problem Understanding

The fitting alignment problem finds the highest-scoring alignment where the first string is a prefix of the second string. This is different from global alignment where both strings are fully aligned.

## Solution Approach

We'll use dynamic programming with a scoring matrix to find the optimal fitting alignment.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Find_Highest_Scoring_Fitting_Alignment is
   type Score_Type is range -1000..1000;
   type Matrix_Type is array (0..1000, 0..1000) of Score_Type;
   
   -- Scoring parameters
   MATCH_SCORE : constant Score_Type := 1;
   MISMATCH_SCORE : constant Score_Type := -1;
   GAP_SCORE : constant Score_Type := -1;
   
   -- Input strings
   S : Unbounded_String := To_Unbounded_String("GTAGGCTTAAGGTTA");
   T : Unbounded_String := To_Unbounded_String("TAGATA");
   
   -- Matrix for dynamic programming
   dp : Matrix_Type;
   
   -- Function to get character at position
   function Get_Char(Str : Unbounded_String; Pos : Natural) return Character is
   begin
      if Pos = 0 then
         return ' ';
      else
         return Element(Str, Pos);
      end if;
   end Get_Char;
   
   -- Function to compute maximum of three values
   function Max_Three(A, B, C : Score_Type) return Score_Type is
   begin
      if A >= B and A >= C then
         return A;
      elsif B >= C then
         return B;
      else
         return C;
      end if;
   end Max_Three;
   
   -- Function to compute maximum of two values
   function Max_Two(A, B : Score_Type) return Score_Type is
   begin
      if A >= B then
         return A;
      else
         return B;
      end if;
   end Max_Two;
   
   -- Function to get score for matching characters
   function Get_Score(Char1, Char2 : Character) return Score_Type is
   begin
      if Char1 = Char2 then
         return MATCH_SCORE;
      else
         return MISMATCH_SCORE;
      end if;
   end Get_Score;
   
   -- Main alignment function
   procedure Compute_Fitting_Alignment(S_Str, T_Str : Unbounded_String) is
      m : constant Natural := Length(S_Str);
      n : constant Natural := Length(T_Str);
      
      -- Initialize matrix
      i, j : Natural;
      max_score : Score_Type := -1000;
      max_pos : Natural := 0;
      
   begin
      -- Initialize first row
      for j in 0..n loop
         dp(0, j) := 0;
      end loop;
      
      -- Initialize first column
      for i in 0..m loop
         dp(i, 0) := 0;
      end loop;
      
      -- Fill the matrix
      for i in 1..m loop
         for j in 1..n loop
            declare
               match_score : Score_Type;
               score1, score2, score3 : Score_Type;
            begin
               -- Compute match/mismatch score
               match_score := Get_Score(Get_Char(S_Str, i), Get_Char(T_Str, j));
               
               -- Compute three possible scores
               score1 := dp(i-1, j) + GAP_SCORE;           -- Deletion
               score2 := dp(i, j-1) + GAP_SCORE;           -- Insertion
               score3 := dp(i-1, j-1) + match_score;       -- Match/mismatch
               
               -- Take maximum
               dp(i, j) := Max_Three(score1, score2, score3);
               
               -- Keep track of maximum score in last row
               if i = m and dp(i, j) > max_score then
                  max_score := dp(i, j);
                  max_pos := j;
               end if;
            end;
         end loop;
      end loop;
      
      -- Backtrack to find alignment
      Put_Line("Maximum score: " & max_score'Img);
      
      -- Print the alignment
      declare
         s_aligned : Unbounded_String := Null_Unbounded_String;
         t_aligned : Unbounded_String := Null_Unbounded_String;
         i, j : Natural := m;
         k : Natural := max_pos;
         current_score : Score_Type;
      begin
         -- Backtrack from (m, max_pos)
         while i > 0 and j > 0 loop
            current_score := dp(i, j);
            
            -- Check which direction we came from
            if i > 0 and j > 0 and 
               dp(i-1, j-1) + Get_Score(Get_Char(S_Str, i), Get_Char(T_Str, j)) = current_score then
               -- Match/mismatch
               s_aligned := Prepend(s_aligned, Get_Char(S_Str, i));
               t_aligned := Prepend(t_aligned, Get_Char(T_Str, j));
               i := i - 1;
               j := j - 1;
            elsif i > 0 and dp(i-1, j) + GAP_SCORE = current_score then
               -- Deletion in T
               s_aligned := Prepend(s_aligned, Get_Char(S_Str, i));
               t_aligned := Prepend(t_aligned, '-');
               i := i - 1;
            elsif j > 0 and dp(i, j-1) + GAP_SCORE = current_score then
               -- Insertion in T
               s_aligned := Prepend(s_aligned, '-');
               t_aligned := Prepend(t_aligned, Get_Char(T_Str, j));
               j := j - 1;
            else
               -- Should not happen
               exit;
            end if;
         end loop;
         
         -- Print alignment
         Put_Line("Alignment:");
         Put_Line(To_String(s_aligned));
         Put_Line(To_String(t_aligned));
      end;
   end Compute_Fitting_Alignment;
   
begin
   -- Read input strings
   Put_Line("Input strings:");
   Put_Line("S: " & To_String(S));
   Put_Line("T: " & To_String(T));
   Put_Line("");
   
   -- Compute fitting alignment
   Compute_Fitting_Alignment(S, T);
   
end Find_Highest_Scoring_Fitting_Alignment;
```

## Key Features of the Solution

1. **Dynamic Programming Matrix**: Uses a 2D matrix to store optimal scores for subproblems
2. **Fitting Alignment**: Ensures the first string is a prefix of the second string
3. **Backtracking**: Recovers the actual alignment from the computed scores
4. **Scoring System**: 
   - Match: +1
   - Mismatch: -1
   - Gap: -1

## How it Works

1. **Initialization**: Set up the scoring matrix with zeros
2. **Matrix Filling**: For each cell, compute the maximum score from:
   - Deletion (from above)
   - Insertion (from left)  
   - Match/mismatch (from diagonal)
3. **Optimization**: Track maximum score in the last row to ensure fitting alignment
4. **Backtracking**: Reconstruct the alignment by following the path of maximum scores

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the dynamic programming matrix

This solution correctly handles the fitting alignment problem by ensuring that the first string is aligned as a prefix of the second string, finding the highest-scoring such alignment.

