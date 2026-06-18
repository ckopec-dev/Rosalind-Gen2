# Rosalind Problem: Maximum Matchings and RNA Secondary Structures (Ada Solution)

## Problem Understanding

This problem asks us to find the maximum number of base pairings in an RNA sequence such that no base pairs cross each other (non-crossing matchings). This is equivalent to finding the maximum matching in a planar graph representation of the RNA structure.

## Approach

We'll use dynamic programming with the following recurrence relation:
- For a substring from index i to j, we want to find the maximum number of non-crossing base pairs
- If s[i] can pair with s[j], then we have one pairing plus optimal matchings for the remaining parts
- We consider all possible positions k where we could place a base pair at position i

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Maximum_Matchings_and_RNA_Secondary_Structures is
   type Sequence is array (Positive range <>) of Character;
   
   function Is_Pairing(a, b : Character) return Boolean is
   begin
      case a is
         when 'A' => return b = 'U';
         when 'U' => return b = 'A';
         when 'G' => return b = 'C';
         when 'C' => return b = 'G';
         when others => return False;
      end case;
   end Is_Pairing;
   
   function Max_Matchings(seq : Sequence) return Natural is
      n : constant Natural := seq'Length;
      -- dp[i][j] represents maximum matchings for substring from i to j
      type DP_Table is array (Positive range 1..n, Positive range 1..n) of Natural;
      dp : DP_Table := (others => (others => 0));
      
      function Get_Max(i, j : Natural) return Natural is
         result : Natural := 0;
         k : Natural;
      begin
         -- Base case: if length < 2, no matchings possible
         if j <= i then
            return 0;
         end if;
         
         -- If we can pair the first and last characters
         if Is_Pairing(seq(i), seq(j)) then
            -- Pair them and add optimal matchings for remaining substring
            result := 1 + Get_Max(i+1, j-1);
         end if;
         
         -- Try all possible split points
         for k in i..j-1 loop
            result := Natural'Max(result, Get_Max(i, k) + Get_Max(k+1, j));
         end loop;
         
         return result;
      end Get_Max;
   begin
      -- This approach is inefficient - let's use a better DP approach
      
      -- Correct approach: dp[i][j] = max matchings for substring from i to j
      -- We'll use the standard RNA secondary structure DP algorithm
      for length in 2..n loop
         for i in 1..n-length+1 loop
            declare
               j : constant Natural := i + length - 1;
            begin
               -- Initially, we can just take max of two parts
               dp(i, j) := dp(i, j-1);
               
               -- Try to match j with some k in [i..j-1]
               for k in i..j-1 loop
                  if Is_Pairing(seq(k), seq(j)) then
                     declare
                        temp : Natural;
                     begin
                        temp := (if k > 1 then dp(i, k-1) else 0) + 
                               (if j > k+1 then dp(k+1, j-1) else 0) + 1;
                        dp(i, j) := Natural'Max(dp(i, j), temp);
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end loop;
      
      return dp(1, n);
   end Max_Matchings;
   
   procedure Read_Sequence return Sequence is
      line : Unbounded_String;
      seq  : Sequence(1..200); -- Assuming maximum length of 200
      pos  : Positive := 1;
   begin
      Put_Line("Enter RNA sequence (max 200 characters): ");
      line := To_Unbounded_String(Get_Line);
      
      for i in 1..Length(line) loop
         seq(pos) := Element(line, i);
         pos := pos + 1;
      end loop;
      
      return seq(1..pos-1);
   end Read_Sequence;
   
   -- More efficient implementation using dynamic programming
   function Count_Max_Matchings(seq : Sequence) return Natural is
      n : constant Natural := seq'Length;
      dp : array (1..n, 1..n) of Natural := (others => (others => 0));
   begin
      for len in 2..n loop
         for i in 1..n-len+1 loop
            declare
               j : constant Natural := i + len - 1;
            begin
               -- Default case: don't pair the last character
               dp(i, j) := dp(i, j-1);
               
               -- Try all possible pairing positions for j
               for k in i..j-1 loop
                  if Is_Pairing(seq(k), seq(j)) then
                     declare
                        temp : Natural;
                     begin
                        temp := (if k > 1 then dp(i, k-1) else 0) +
                               (if j > k+1 then dp(k+1, j-1) else 0) + 1;
                        dp(i, j) := Natural'Max(dp(i, j), temp);
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end loop;
      
      return dp(1, n);
   end Count_Max_Matchings;
   
begin
   Put_Line("Maximum Matchings and RNA Secondary Structures");
   Put_Line("==============================================");
   
   -- Example from Rosalind problem
   declare
      test_seq : constant Sequence := 
         ('A', 'U', 'G', 'C', 'G', 'U', 'A', 'U', 'C', 'A');
   begin
      Put_Line("Example sequence: AUGC-GUAC");
      Put_Line("Maximum matchings: " & Natural'Image(Count_Max_Matchings(test_seq)));
   end;
   
   -- Interactive version
   Put_Line("Enter your own RNA sequence:");
   declare
      user_seq : Sequence := Read_Sequence;
   begin
      Put_Line("Your sequence: ");
      for i in user_seq'Range loop
         Put(user_seq(i));
      end loop;
      New_Line;
      
      Put_Line("Maximum matchings: " & Natural'Image(Count_Max_Matchings(user_seq)));
   end;
   
end Maximum_Matchings_and_RNA_Secondary_Structures;
```

## Explanation

This solution implements a dynamic programming approach to solve the maximum matching problem for RNA secondary structures:

1. **Base Cases**: For substrings of length < 2, we cannot form any matchings.

2. **Recurrence Relation**: 
   - `dp[i][j]` = maximum number of non-crossing base pairs in substring from i to j
   - For each possible pairing position k for the last character j:
     - If s[k] pairs with s[j], we get 1 + dp[i][k-1] + dp[k+1][j-1]
     - Take maximum over all valid k

3. **Time Complexity**: O(n³) where n is the length of the sequence
4. **Space Complexity**: O(n²) for the DP table

## Key Features

- Handles all valid RNA base pairings (A-U, U-A, G-C, C-G)
- Uses dynamic programming to avoid recomputation
- Implements proper boundary conditions for the DP algorithm
- Includes both example and interactive input modes

The solution correctly handles the constraint that base pairs cannot cross each other in the secondary structure.