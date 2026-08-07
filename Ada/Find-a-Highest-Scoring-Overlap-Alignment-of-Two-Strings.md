# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings

## Problem Description
Given two strings, find the highest-scoring overlap alignment where one string can be aligned to the end of another with a penalty for gaps at the beginning.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Find_Highest_Scoring_Overlap_Alignment is
   
   -- Constants
   MATCH_SCORE : constant := 1;
   MISMATCH_SCORE : constant := -1;
   GAP_PENALTY : constant := -2;
   
   -- String type
   type String_Array is array (Positive range <>) of Character;
   
   -- Function to compute maximum of two integers
   function Max(A, B : Integer) return Integer is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;
   
   -- Function to compute maximum of three integers
   function Max3(A, B, C : Integer) return Integer is
   begin
      return Max(Max(A, B), C);
   end Max3;
   
   -- Function to get character score
   function Get_Score(C1, C2 : Character) return Integer is
   begin
      if C1 = C2 then
         return MATCH_SCORE;
      else
         return MISMATCH_SCORE;
      end if;
   end Get_Score;
   
   -- Main function to find overlap alignment
   procedure Overlap_Alignment(S1, S2 : String; 
                              Alignment1, Alignment2 : out String) is
      
      L1 : constant Positive := S1'Length;
      L2 : constant Positive := S2'Length;
      
      -- DP table for scoring
      type Score_Matrix is array (0 .. L1, 0 .. L2) of Integer;
      DP : Score_Matrix;
      
      -- Backtrack matrix to reconstruct alignment
      type Direction_Matrix is array (0 .. L1, 0 .. L2) of Character;
      Backtrack : Direction_Matrix;
      
      -- Initialize first row and column
      Max_Score : Integer := Integer'First;
      Max_Pos : Positive := 1;
      
   begin
      -- Initialize DP table with zeros
      for I in 0 .. L1 loop
         for J in 0 .. L2 loop
            DP(I, J) := 0;
         end loop;
      end loop;
      
      -- Fill the DP table
      for I in 1 .. L1 loop
         for J in 1 .. L2 loop
            declare
               Score1 : constant Integer := DP(I-1, J) + GAP_PENALTY;
               Score2 : constant Integer := DP(I, J-1) + GAP_PENALTY;
               Score3 : constant Integer := DP(I-1, J-1) + Get_Score(S1(I), S2(J));
            begin
               DP(I, J) := Max3(Score1, Score2, Score3);
               
               -- Record direction for backtracking
               if DP(I, J) = Score1 then
                  Backtrack(I, J) := 'D';  -- Down (gap in second string)
               elsif DP(I, J) = Score2 then
                  Backtrack(I, J) := 'R';  -- Right (gap in first string)
               else
                  Backtrack(I, J) := 'M';  -- Match/Mismatch
               end if;
            end;
         end loop;
      end loop;
      
      -- Find maximum score in last row (overlap alignment)
      for J in 1 .. L2 loop
         if DP(L1, J) > Max_Score then
            Max_Score := DP(L1, J);
            Max_Pos := J;
         end if;
      end loop;
      
      -- Reconstruct alignment from backtracking matrix
      declare
         I : Positive := L1;
         J : Positive := Max_Pos;
         Align1, Align2 : String(1 .. L1 + L2);
         Align_Index : Positive := Align1'Last;
      begin
         -- Backtrack from the maximum position in last row
         while I >= 1 and J >= 1 loop
            case Backtrack(I, J) is
               when 'M' =>
                  Align1(Align_Index) := S1(I);
                  Align2(Align_Index) := S2(J);
                  I := I - 1;
                  J := J - 1;
                  Align_Index := Align_Index - 1;
               when 'D' =>
                  Align1(Align_Index) := S1(I);
                  Align2(Align_Index) := '-';
                  I := I - 1;
                  Align_Index := Align_Index - 1;
               when 'R' =>
                  Align1(Align_Index) := '-';
                  Align2(Align_Index) := S2(J);
                  J := J - 1;
                  Align_Index := Align_Index - 1;
            end case;
         end loop;
         
         -- Handle remaining characters
         while I >= 1 loop
            Align1(Align_Index) := S1(I);
            Align2(Align_Index) := '-';
            I := I - 1;
            Align_Index := Align_Index - 1;
         end loop;
         
         while J >= 1 loop
            Align1(Align_Index) := '-';
            Align2(Align_Index) := S2(J);
            J := J - 1;
            Align_Index := Align_Index - 1;
         end loop;
         
         -- Adjust for the fact that we built strings backwards
         Alignment1 := Align1(Align_Index+1 .. Align1'Last);
         Alignment2 := Align2(Align_Index+1 .. Align2'Last);
      end;
      
   end Overlap_Alignment;
   
   -- Test with example
   S1 : constant String := "PAWHEAE";
   S2 : constant String := "HEAGAWGHEE";
   
   Result1, Result2 : String(1 .. 20);
   
begin
   Put_Line("Input strings:");
   Put_Line("S1: " & S1);
   Put_Line("S2: " & S2);
   Put_Line("");
   
   Overlap_Alignment(S1, S2, Result1, Result2);
   
   Put_Line("Highest-scoring overlap alignment:");
   Put_Line("Alignment 1: " & Result1);
   Put_Line("Alignment 2: " & Result2);
   
end Find_Highest_Scoring_Overlap_Alignment;
```

## Explanation

This Ada solution implements the overlap alignment algorithm using dynamic programming:

1. **Dynamic Programming Table**: Creates a 2D matrix `DP` where `DP[i][j]` represents the highest score for aligning the first `i` characters of string S1 with the first `j` characters of string S2.

2. **Scoring System**:
   - Match: +1
   - Mismatch: -1  
   - Gap penalty: -2

3. **Transition Rules**:
   - From top: `DP[i-1][j] + GAP_PENALTY`
   - From left: `DP[i][j-1] + GAP_PENALTY` 
   - From diagonal: `DP[i-1][j-1] + score(S1[i], S2[j])`

4. **Overlap Alignment**: The alignment is found by looking at the maximum value in the last row of the DP matrix, which represents alignments where the entire first string is aligned to some prefix of the second string.

5. **Backtracking**: Uses a backtracking matrix to reconstruct the actual alignment from the computed scores.

The algorithm correctly handles the overlap constraint where one sequence can be aligned to the end of another, making it suitable for bioinformatics applications like sequence assembly.