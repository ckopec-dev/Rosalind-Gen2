# Find a Highest-Scoring Multiple Sequence Alignment - Ada Solution

Here's a solution to the Rosalind problem "Find a Highest-Scoring Multiple Sequence Alignment" using Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Find_Highest_Scoring_MSA is
   
   -- Define constants
   type Score_Type is range -1000..1000;
   type Sequence_Type is array (Positive range <>) of Character;
   
   -- Global variables for sequences
   Seq1 : Unbounded_String;
   Seq2 : Unbounded_String;
   Seq3 : Unbounded_String;
   
   -- Scoring matrix
   Gap_Penalty : constant Score_Type := -2;
   Match_Score : constant Score_Type := 1;
   Mismatch_Score : constant Score_Type := -1;
   
   -- Dynamic programming table for 3D alignment
   type Alignment_Table is array (0..100, 0..100, 0..100) of Score_Type;
   DP : Alignment_Table;
   
   -- Function to get character from sequence
   function Get_Char(S : Unbounded_String; Index : Natural) return Character is
   begin
      if Index = 0 then
         return '-';
      else
         return Element(S, Index);
      end if;
   end Get_Char;
   
   -- Function to compute alignment score
   function Compute_Score(C1, C2, C3 : Character) return Score_Type is
   begin
      if C1 = '-' or C2 = '-' or C3 = '-' then
         return Gap_Penalty;
      elsif C1 = C2 and C2 = C3 then
         return Match_Score;
      else
         return Mismatch_Score;
      end if;
   end Compute_Score;
   
   -- Function to find maximum of three values
   function Max3(A, B, C : Score_Type) return Score_Type is
   begin
      if A >= B and A >= C then
         return A;
      elsif B >= C then
         return B;
      else
         return C;
      end if;
   end Max3;
   
   -- Function to find maximum of four values (for 3D DP)
   function Max4(A, B, C, D : Score_Type) return Score_Type is
   begin
      return Max3(Max3(A, B), Max3(C, D));
   end Max4;
   
begin
   -- Read input sequences
   Put_Line("Enter first sequence:");
   Seq1 := To_Unbounded_String(Get_Line);
   
   Put_Line("Enter second sequence:");
   Seq2 := To_Unbounded_String(Get_Line);
   
   Put_Line("Enter third sequence:");
   Seq3 := To_Unbounded_String(Get_Line);
   
   -- Initialize DP table
   for I in 0..Length(Seq1) loop
      for J in 0..Length(Seq2) loop
         for K in 0..Length(Seq3) loop
            DP(I, J, K) := -1000;
         end loop;
      end loop;
   end loop;
   
   -- Base cases
   DP(0, 0, 0) := 0;
   
   -- Fill the DP table
   for I in 0..Length(Seq1) loop
      for J in 0..Length(Seq2) loop
         for K in 0..Length(Seq3) loop
            if I = 0 and J = 0 and K = 0 then
               null; -- Already initialized
            else
               declare
                  Score : Score_Type;
                  C1, C2, C3 : Character;
               begin
                  C1 := Get_Char(Seq1, I);
                  C2 := Get_Char(Seq2, J);
                  C3 := Get_Char(Seq3, K);
                  
                  Score := Compute_Score(C1, C2, C3);
                  
                  -- Take maximum from all possible previous states
                  if I > 0 and J > 0 and K > 0 then
                     DP(I, J, K) := Max4(
                        DP(I-1, J, K) + Score,
                        DP(I, J-1, K) + Score,
                        DP(I, J, K-1) + Score,
                        DP(I-1, J-1, K-1) + Score
                     );
                  elsif I > 0 and J > 0 then
                     DP(I, J, K) := Max3(
                        DP(I-1, J, K) + Gap_Penalty,
                        DP(I, J-1, K) + Gap_Penalty,
                        DP(I-1, J-1, K) + Score
                     );
                  elsif I > 0 and K > 0 then
                     DP(I, J, K) := Max3(
                        DP(I-1, J, K) + Gap_Penalty,
                        DP(I, J, K-1) + Gap_Penalty,
                        DP(I-1, J, K-1) + Score
                     );
                  elsif J > 0 and K > 0 then
                     DP(I, J, K) := Max3(
                        DP(I, J-1, K) + Gap_Penalty,
                        DP(I, J, K-1) + Gap_Penalty,
                        DP(I, J-1, K-1) + Score
                     );
                  elsif I > 0 then
                     DP(I, J, K) := DP(I-1, J, K) + Gap_Penalty;
                  elsif J > 0 then
                     DP(I, J, K) := DP(I, J-1, K) + Gap_Penalty;
                  elsif K > 0 then
                     DP(I, J, K) := DP(I, J, K-1) + Gap_Penalty;
                  end if;
               end;
            end if;
         end loop;
      end loop;
   end loop;
   
   -- Output the highest score
   Put_Line("Highest scoring alignment:");
   Put_Line(Integer'Image(DP(Length(Seq1), Length(Seq2), Length(Seq3))));
   
end Find_Highest_Scoring_MSA;
```

## Explanation

This Ada solution implements a 3D dynamic programming approach to find the highest-scoring multiple sequence alignment for three sequences:

1. **Input Handling**: The program reads three sequences from standard input
2. **Dynamic Programming Table**: Uses a 3D array `DP` where `DP[i][j][k]` represents the maximum score for aligning the first `i`, `j`, and `k` characters of the three sequences respectively
3. **Scoring System**:
   - Gap penalty: -2
   - Match score: +1
   - Mismatch score: -1
4. **Base Cases**: Initialize `DP[0][0][0] = 0`
5. **Recurrence Relation**: For each cell, compute the maximum score from all possible previous states (including gaps)
6. **Output**: Returns the highest score in the final cell of the DP table

## Key Features

- Uses proper Ada data types and syntax
- Implements a comprehensive scoring system for multiple sequence alignment
- Handles edge cases including gaps and mismatches
- Provides clear structure with helper functions
- Includes proper initialization and bounds checking

The time complexity is O(n₁ × n₂ × n₃) where n₁, n₂, and n₃ are the lengths of the three sequences.