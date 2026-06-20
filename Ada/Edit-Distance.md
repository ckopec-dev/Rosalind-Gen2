# Rosalind Problem: Edit Distance (Edit_Distance) in Ada

## Problem Understanding

The edit distance (also known as Levenshtein distance) between two strings is the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

## Solution Approach

I'll use dynamic programming with a 2D table where `dp[i][j]` represents the edit distance between the first `i` characters of string1 and the first `j` characters of string2.

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Edit_Distance is
   type String_Access is access all String;
   
   function Edit_Dist(s1, s2 : String) return Integer is
      m : constant Integer := s1'Length;
      n : constant Integer := s2'Length;
      
      -- Create 2D array for dynamic programming
      type DP_Table is array (0 .. m, 0 .. n) of Integer;
      dp : DP_Table;
   begin
      -- Initialize base cases
      -- Empty string to s2[1..j] requires j insertions
      for j in 0 .. n loop
         dp(0, j) := j;
      end loop;
      
      -- s1[1..i] to empty string requires i deletions
      for i in 0 .. m loop
         dp(i, 0) := i;
      end loop;
      
      -- Fill the DP table
      for i in 1 .. m loop
         for j in 1 .. n loop
            if s1(i) = s2(j) then
               -- Characters match, no operation needed
               dp(i, j) := dp(i-1, j-1);
            else
               -- Take minimum of three operations:
               -- 1. Substitution: dp(i-1, j-1) + 1
               -- 2. Deletion: dp(i-1, j) + 1  
               -- 3. Insertion: dp(i, j-1) + 1
               dp(i, j) := Integer'Min(
                  Integer'Min(dp(i-1, j-1), dp(i-1, j)),
                  dp(i, j-1)
               ) + 1;
            end if;
         end loop;
      end loop;
      
      return dp(m, n);
   end Edit_Dist;
   
   -- Read input strings
   s1 : Unbounded_String := To_Unbounded_String(Get_Line);
   s2 : Unbounded_String := To_Unbounded_String(Get_Line);
   
begin
   Put_Line(Integer'Image(Edit_Dist(To_String(s1), To_String(s2))));
end Edit_Distance;
```

## Explanation of Key Components

1. **Dynamic Programming Table**: 
   - `dp[i][j]` stores the edit distance between `s1[1..i]` and `s2[1..j]`
   - Base cases: empty string to any prefix requires that many insertions/deletions

2. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j-1], dp[i-1][j], dp[i][j-1])`

3. **Operations**:
   - Substitution: `dp[i-1][j-1] + 1`
   - Deletion: `dp[i-1][j] + 1`  
   - Insertion: `dp[i][j-1] + 1`

## Example Usage

Input:
```
GAGA
GATA
```

Output:
```
1
```

The edit distance is 1 because we need one substitution (A→T) to change "GAGA" to "GATA".

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m × n) for the DP table

This solution efficiently computes the minimum edit distance using dynamic programming principles, which is the standard approach for this classic algorithmic problem.