# Rosalind Problem: Maximizing the Gap Symbols of an Optimal Alignment (Ada Solution)

## Problem Understanding

This problem asks us to find the maximum number of gap symbols that can appear in any optimal alignment between two strings, where we're given a scoring system with:
- Match score: 2
- Mismatch score: -1  
- Gap penalty: -1

We need to compute the maximum number of gaps possible in any optimal alignment.

## Solution Approach

I'll use dynamic programming with a modified approach. The key insight is that while we want to maximize gaps, we also want to maximize the alignment score. I'll track both:
1. The maximum score achievable at each position
2. The maximum number of gaps that can occur in an optimal alignment ending at that position

## Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Maximizing_The_Gap_Symbols_Of_An_Optimal_Alignment is
   type Score_Type is range -1000..1000;
   
   -- Input strings
   S : Unbounded_String := To_Unbounded_String("ATCG");
   T : Unbounded_String := To_Unbounded_String("ACGT");
   
   -- Function to get character at position (1-indexed)
   function Get_Char(Str : Unbounded_String; Pos : Natural) return Character is
   begin
      if Pos = 0 then
         return ' ';
      else
         return Element(Str, Pos);
      end if;
   end Get_Char;
   
   -- Function to compute maximum gaps in optimal alignment
   function Max_Gaps(S : Unbounded_String; T : Unbounded_String) return Natural is
      N : constant Natural := Length(S);
      M : constant Natural := Length(T);
      
      -- DP table: (i,j) -> (score, max_gaps)
      type Cell_Type is record
         Score : Score_Type;
         Max_Gaps : Natural;
      end record;
      
      DP : array (0..N, 0..M) of Cell_Type := 
         (others => (Score => -1000, Max_Gaps => 0));
      
      -- Base cases
      -- Empty string aligned with prefix of T
      for i in 0..N loop
         DP(i, 0) := (Score => Score_Type(-i), Max_Gaps => i);
      end loop;
      
      -- Empty string aligned with prefix of S  
      for j in 0..M loop
         DP(0, j) := (Score => Score_Type(-j), Max_Gaps => j);
      end loop;
      
      -- Fill the DP table
      for i in 1..N loop
         for j in 1..M loop
            declare
               Match_Score : Score_Type;
               Gap_Score   : Score_Type;
               Current_Char_S : Character := Get_Char(S, i);
               Current_Char_T : Character := Get_Char(T, j);
            begin
               -- Calculate match/mismatch score
               if Current_Char_S = Current_Char_T then
                  Match_Score := 2;
               else
                  Match_Score := -1;
               end if;
               
               -- Calculate scores from three possible moves:
               -- 1. Diagonal (match/mismatch)
               declare
                  Diag_Score : Score_Type := DP(i-1, j-1).Score + Match_Score;
                  Diag_Gaps : Natural := DP(i-1, j-1).Max_Gaps;
               begin
                  if Current_Char_S = ' ' or Current_Char_T = ' ' then
                     Diag_Gaps := Diag_Gaps + 1;
                  end if;
                  
                  -- Update if this gives better score or same score with more gaps
                  if Diag_Score > DP(i, j).Score or 
                     (Diag_Score = DP(i, j).Score and Diag_Gaps > DP(i, j).Max_Gaps) then
                     DP(i, j) := (Score => Diag_Score, Max_Gaps => Diag_Gaps);
                  end if;
               end;
               
               -- 2. From top (gap in S)
               declare
                  Top_Score : Score_Type := DP(i-1, j).Score - 1;
                  Top_Gaps : Natural := DP(i-1, j).Max_Gaps + 1;
               begin
                  if Top_Score > DP(i, j).Score or 
                     (Top_Score = DP(i, j).Score and Top_Gaps > DP(i, j).Max_Gaps) then
                     DP(i, j) := (Score => Top_Score, Max_Gaps => Top_Gaps);
                  end if;
               end;
               
               -- 3. From left (gap in T)
               declare
                  Left_Score : Score_Type := DP(i, j-1).Score - 1;
                  Left_Gaps : Natural := DP(i, j-1).Max_Gaps + 1;
               begin
                  if Left_Score > DP(i, j).Score or 
                     (Left_Score = DP(i, j).Score and Left_Gaps > DP(i, j).Max_Gaps) then
                     DP(i, j) := (Score => Left_Score, Max_Gaps => Left_Gaps);
                  end if;
               end;
            end;
         end loop;
      end loop;
      
      return DP(N, M).Max_Gaps;
   end Max_Gaps;
   
begin
   -- Read input strings (example values)
   S := To_Unbounded_String("ATCG");
   T := To_Unbounded_String("ACGT");
   
   -- Calculate and output result
   Put_Line("Maximum gaps in optimal alignment: ");
   Put(Max_Gaps(S, T));
   New_Line;
   
   -- Test with another example
   S := To_Unbounded_String("AACCTTGG");
   T := To_Unbounded_String("ACGTACGT");
   
   Put_Line("Example 2:");
   Put(Max_Gaps(S, T));
   New_Line;
   
end Maximizing_The_Gap_Symbols_Of_An_Optimal_Alignment;
```

## Key Insights

1. **Modified DP Approach**: Instead of just tracking the maximum score, we track both score and maximum gaps.

2. **Gap Penalties**: Each gap (insertion or deletion) contributes -1 to the score and +1 to the gap count.

3. **Optimization Strategy**: When two paths have equal scores, we prefer the one with more gaps to maximize our final answer.

4. **Base Cases**: 
   - Empty string aligned with any prefix has gaps equal to the prefix length
   - Score is negative due to gap penalties

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the strings
- **Space Complexity**: O(m×n) for the DP table

This solution correctly handles the scoring system and maximizes the number of gap symbols while maintaining optimal alignment scores.