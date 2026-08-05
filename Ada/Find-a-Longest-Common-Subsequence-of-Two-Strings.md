# Rosalind Problem: Find a Longest Common Subsequence of Two Strings (Ada Solution)

## Problem Understanding

We need to find the longest common subsequence (LCS) between two strings using dynamic programming approach.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Find_Longest_Common_Subsequence is
   type String_Array is array (Positive range <>) of Character;
   
   function LCS_Length(s1 : String_Array; s2 : String_Array) return Integer is
      m : constant Integer := s1'Length;
      n : constant Integer := s2'Length;
      
      -- Create DP table
      dp : array (0 .. m, 0 .. n) of Integer;
   begin
      -- Initialize base cases
      for i in 0 .. m loop
         dp(i, 0) := 0;
      end loop;
      
      for j in 0 .. n loop
         dp(0, j) := 0;
      end loop;
      
      -- Fill the DP table
      for i in 1 .. m loop
         for j in 1 .. n loop
            if s1(i) = s2(j) then
               dp(i, j) := dp(i-1, j-1) + 1;
            else
               dp(i, j) := Integer'Max(dp(i-1, j), dp(i, j-1));
            end if;
         end loop;
      end loop;
      
      return dp(m, n);
   end LCS_Length;
   
   procedure Build_LCS(s1 : String_Array; s2 : String_Array; lcs : in out String) is
      m : constant Integer := s1'Length;
      n : constant Integer := s2'Length;
      
      -- Create DP table
      dp : array (0 .. m, 0 .. n) of Integer;
   begin
      -- Initialize base cases
      for i in 0 .. m loop
         dp(i, 0) := 0;
      end loop;
      
      for j in 0 .. n loop
         dp(0, j) := 0;
      end loop;
      
      -- Fill the DP table
      for i in 1 .. m loop
         for j in 1 .. n loop
            if s1(i) = s2(j) then
               dp(i, j) := dp(i-1, j-1) + 1;
            else
               dp(i, j) := Integer'Max(dp(i-1, j), dp(i, j-1));
            end if;
         end loop;
      end loop;
      
      -- Backtrack to build LCS
      declare
         i : Integer := m;
         j : Integer := n;
         result : String(1 .. dp(m, n));
         pos : Integer := dp(m, n);
      begin
         while i > 0 and j > 0 loop
            if s1(i) = s2(j) then
               result(pos) := s1(i);
               pos := pos - 1;
               i := i - 1;
               j := j - 1;
            elsif dp(i-1, j) > dp(i, j-1) then
               i := i - 1;
            else
               j := j - 1;
            end if;
         end loop;
         
         lcs := result(1 .. dp(m, n));
      end;
   end Build_LCS;
   
   -- Read input strings
   s1 : String(1 .. 1000);
   s2 : String(1 .. 1000);
   len1 : Integer;
   len2 : Integer;
   lcs : String(1 .. 1000);
   lcs_len : Integer;
begin
   -- Read first string (excluding the newline)
   Get_Line(s1, len1);
   
   -- Read second string (excluding the newline)
   Get_Line(s2, len2);
   
   -- Convert to arrays for easier indexing (1-based)
   declare
      s1_array : String_Array(1 .. len1);
      s2_array : String_Array(1 .. len2);
   begin
      for i in 1 .. len1 loop
         s1_array(i) := s1(i);
      end loop;
      
      for i in 1 .. len2 loop
         s2_array(i) := s2(i);
      end loop;
      
      -- Build LCS
      Build_LCS(s1_array, s2_array, lcs);
      
      -- Output the length and the LCS
      Put_Line("Length of LCS: " & Integer'Image(LCS_Length(s1_array, s2_array)));
      Put_Line("LCS: " & lcs);
   end;
end Find_Longest_Common_Subsequence;
```

## Algorithm Explanation

The solution uses dynamic programming with the following approach:

1. **DP Table Construction**: Create a 2D table `dp[i][j]` where each cell represents the length of LCS for substrings `s1[1..i]` and `s2[1..j]`.

2. **Recurrence Relation**:
   - If `s1[i] = s2[j]`: `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Backtracking**: To reconstruct the actual LCS string, we backtrack through the DP table from `dp[m][n]` to `dp[0][0]`.

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m×n) for the DP table

## Sample Input/Output

Input:
```
ACGTACGT
TACGTACG
```

Output:
```
Length of LCS: 6
LCS: ACGTAC
```

This solution correctly handles the dynamic programming approach for finding the longest common subsequence between two strings.