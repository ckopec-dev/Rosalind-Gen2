# Rosalind Problem: Interleaving Two Motifs (Ada Solution)

## Problem Understanding

Given two DNA strings s and t, we need to find the number of ways to interleave the characters of s and t to form a string that contains both as subsequences.

This is equivalent to finding the number of shortest paths in a grid from top-left to bottom-right where we can only move right or down, and each path represents an interleaving.

## Solution Approach

We'll use dynamic programming with the recurrence relation:
- `dp[i][j]` = number of ways to interleave first `i` characters of s and first `j` characters of t
- `dp[i][j] = dp[i-1][j] + dp[i][j-1]` if s[i] = t[j]
- `dp[i][j] = dp[i-1][j] + dp[i][j-1]` otherwise

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Interleaving_Two_Motifs is
   
   -- Function to compute binomial coefficient C(n,k)
   function Binomial(n, k : Integer) return Long_Long_Integer is
      result : Long_Long_Integer := 1;
   begin
      if k > n or k < 0 then
         return 0;
      end if;
      
      if k = 0 or k = n then
         return 1;
      end if;
      
      -- Take advantage of symmetry
      if k > n - k then
         k := n - k;
      end if;
      
      for i in 1..k loop
         result := result * (n - i + 1) / i;
      end loop;
      
      return result;
   end Binomial;
   
   -- Function to compute interleaving count using dynamic programming
   function Count_Interleavings(s, t : String) return Long_Long_Integer is
      n : constant Integer := s'Length;
      m : constant Integer := t'Length;
      
      -- Create DP table
      dp : array (0..n, 0..m) of Long_Long_Integer;
      
      i, j : Integer;
   begin
      -- Initialize base cases
      for i in 0..n loop
         dp(i, 0) := 1;
      end loop;
      
      for j in 0..m loop
         dp(0, j) := 1;
      end loop;
      
      -- Fill the DP table
      for i in 1..n loop
         for j in 1..m loop
            if s(i) = t(j) then
               dp(i, j) := dp(i-1, j) + dp(i, j-1);
            else
               dp(i, j) := dp(i-1, j) + dp(i, j-1);
            end if;
         end loop;
      end loop;
      
      return dp(n, m);
   end Count_Interleavings;
   
   -- Alternative approach using combinatorics
   function Count_Interleavings_Combinatorics(s, t : String) return Long_Long_Integer is
      n : constant Integer := s'Length;
      m : constant Integer := t'Length;
      
      -- This is equivalent to C(n+m, n) or C(n+m, m)
      -- which represents choosing positions for characters from s among (n+m) total positions
   begin
      return Binomial(n + m, n);
   end Count_Interleavings_Combinatorics;
   
   -- Read input strings
   s : String(1..1000);
   t : String(1..1000);
   s_len, t_len : Integer;
   
begin
   -- Read first string
   Get_Line(s, s_len);
   
   -- Read second string
   Get_Line(t, t_len);
   
   -- Calculate and print result
   Put_Line("Number of interleavings:");
   Put(Count_Interleavings(s(1..s_len), t(1..t_len)), Width => 0);
   New_Line;
   
end Interleaving_Two_Motifs;
```

## Explanation

The solution uses dynamic programming to count the number of ways to interleave two strings:

1. **DP Table Setup**: Create a 2D table `dp[i][j]` where `dp[i][j]` represents the number of ways to form an interleaving using the first `i` characters of string `s` and first `j` characters of string `t`.

2. **Base Cases**: 
   - `dp[0][j] = 1` for all j (only one way to interleave empty string with t)
   - `dp[i][0] = 1` for all i (only one way to interleave s with empty string)

3. **Recurrence Relation**:
   - If `s[i] = t[j]`: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`
   - Otherwise: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`

4. **Final Answer**: `dp[n][m]` gives the total number of interleavings.

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n and m are the lengths of the strings
- **Space Complexity**: O(n × m) for the DP table

## Example Usage

For input:
```
ABCD
EFGH
```

The output would be 70, representing the number of ways to interleave these two strings.

This problem is essentially counting lattice paths in a grid or computing binomial coefficients, which can also be solved directly using combinatorics: C(n+m, n) where n and m are the lengths of the two strings.