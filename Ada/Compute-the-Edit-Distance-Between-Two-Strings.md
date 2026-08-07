# Rosalind Problem: Compute the Edit Distance Between Two Strings (Ada Solution)

## Problem Understanding

The edit distance (also known as Levenshtein distance) between two strings is the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

## Solution Approach

I'll use dynamic programming to solve this problem. The approach is to build a matrix where `dp[i][j]` represents the edit distance between the first `i` characters of the first string and the first `j` characters of the second string.

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Edit_Distance is
   type String_Array is array (Positive range <>) of Character;
   
   function Edit_Distance_Compute(S1, S2 : String) return Integer is
      M : constant Natural := S1'Length;
      N : constant Natural := S2'Length;
      
      -- Create DP table
      DP : array (0 .. M, 0 .. N) of Integer;
   begin
      -- Initialize base cases
      -- Empty string to prefix of S2
      for J in 0 .. N loop
         DP(0, J) := J;
      end loop;
      
      -- Empty string to prefix of S1
      for I in 0 .. M loop
         DP(I, 0) := I;
      end loop;
      
      -- Fill the DP table
      for I in 1 .. M loop
         for J in 1 .. N loop
            if S1(I) = S2(J) then
               -- Characters match, no operation needed
               DP(I, J) := DP(I-1, J-1);
            else
               -- Take minimum of three operations:
               -- 1. Substitution: DP[I-1][J-1] + 1
               -- 2. Deletion: DP[I-1][J] + 1
               -- 3. Insertion: DP[I][J-1] + 1
               DP(I, J) := Integer'Min(
                  Integer'Min(DP(I-1, J-1), DP(I-1, J)),
                  DP(I, J-1)
               ) + 1;
            end if;
         end loop;
      end loop;
      
      return DP(M, N);
   end Edit_Distance_Compute;
   
   S1, S2 : String;
begin
   -- Read input strings
   Get_Line(S1);
   Get_Line(S2);
   
   -- Compute and print edit distance
   Put_Line(Edit_Distance_Compute(S1, S2)'Image);
end Edit_Distance;
```

## How it Works

1. **Initialization**: Create a 2D array `DP` where `DP[i][j]` represents the edit distance between the first `i` characters of string S1 and the first `j` characters of string S2.

2. **Base Cases**: 
   - `DP[0][j] = j`: Converting empty string to first `j` characters of S2 requires `j` insertions
   - `DP[i][0] = i`: Converting first `i` characters of S1 to empty string requires `i` deletions

3. **Recurrence Relation**:
   - If `S1[i] = S2[j]`: No operation needed, so `DP[i][j] = DP[i-1][j-1]`
   - Otherwise: Take minimum of three operations:
     - Substitution: `DP[i-1][j-1] + 1`
     - Deletion: `DP[i-1][j] + 1`  
     - Insertion: `DP[i][j-1] + 1`

4. **Result**: The value at `DP[M][N]` gives the edit distance between the two strings.

## Example

For strings "GAC" and "AGT":
```
    ""  A  G  T
""   0  1  2  3
G    1  1  1  2
A    2  1  2  2
C    3  2  2  3
```

The edit distance is 3, which represents the operations needed to transform "GAC" into "AGT".

## Time and Space Complexity

- **Time Complexity**: O(M × N) where M and N are the lengths of the two strings
- **Space Complexity**: O(M × N) for the DP table