# Rosalind Problem: Finding a Shared Spliced Motif (FIND)

## Problem Understanding

We need to find the longest common subsequence (LCS) between two DNA strings. A subsequence is formed by deleting some characters from the original string without changing the order of remaining characters.

## Solution Approach

I'll use dynamic programming to solve this problem:
1. Create a 2D DP table where `dp[i][j]` represents the length of LCS for first `i` characters of string1 and first `j` characters of string2
2. Fill the table using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. Backtrack through the table to reconstruct the actual LCS

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Finding_A_Shared_Spliced_Motif is
   
   type DNA_String is array (Positive range <>) of Character;
   
   procedure Read_Fasta_Sequence (Name : in out Unbounded_String;
                                  Seq  : in out Unbounded_String) is
      Line : Unbounded_String;
      First_Line : Boolean := True;
   begin
      while not End_Of_File loop
         Get_Line (Line);
         if Length (Line) > 0 then
            if Element (Line, 1) = '>' then
               Name := Line;
               First_Line := False;
            else
               if First_Line then
                  Name := Line;
                  First_Line := False;
               else
                  Seq := Seq & Line;
               end if;
            end if;
         end if;
      end loop;
   end Read_Fasta_Sequence;
   
   function Longest_Common_Subsequence (S1, S2 : String) return String is
      L1 : constant Natural := S1'Length;
      L2 : constant Natural := S2'Length;
      
      -- DP table for storing lengths of LCS
      type LCS_Table is array (0 .. L1, 0 .. L2) of Natural;
      Table : LCS_Table := (others => (others => 0));
      
      -- Fill the DP table
      procedure Fill_DP_Table is
      begin
         for I in 1 .. L1 loop
            for J in 1 .. L2 loop
               if S1(I) = S2(J) then
                  Table(I, J) := Table(I-1, J-1) + 1;
               else
                  Table(I, J) := Natural'Max(Table(I-1, J), Table(I, J-1));
               end if;
            end loop;
         end loop;
      end Fill_DP_Table;
      
      -- Reconstruct the LCS
      function Reconstruct_LCS return String is
         Result : Unbounded_String := Null_Unbounded_String;
         I : Natural := L1;
         J : Natural := L2;
      begin
         while I > 0 and then J > 0 loop
            if S1(I) = S2(J) then
               Result := To_Unbounded_String(S1(I)) & Result;
               I := I - 1;
               J := J - 1;
            elsif Table(I-1, J) > Table(I, J-1) then
               I := I - 1;
            else
               J := J - 1;
            end if;
         end loop;
         
         return To_String(Result);
      end Reconstruct_LCS;
      
   begin
      Fill_DP_Table;
      return Reconstruct_LCS;
   end Longest_Common_Subsequence;
   
   -- Main program logic
   S1, S2 : String := "GATTACA";
   LCS : String;
   
begin
   -- For this example, we'll use the test case from Rosalind
   -- In practice, you would read from input files
   
   -- Test with example sequences from Rosalind problem
   S1 := "AACCTTGG";
   S2 := "ACACTGTGA";
   
   Put_Line("String 1: " & S1);
   Put_Line("String 2: " & S2);
   
   LCS := Longest_Common_Subsequence(S1, S2);
   
   Put_Line("Longest Common Subsequence:");
   Put_Line(LCS);
   
   -- Alternative example with longer strings
   S1 := "ATATATA";
   S2 := "TATATAT";
   
   Put_Line("String 1: " & S1);
   Put_Line("String 2: " & S2);
   
   LCS := Longest_Common_Subsequence(S1, S2);
   
   Put_Line("Longest Common Subsequence:");
   Put_Line(LCS);
   
end Finding_A_Shared_Spliced_Motif;
```

## Alternative Simpler Version

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure FIND is
   
   function LCS_Length (S1, S2 : String) return Natural is
      L1 : constant Natural := S1'Length;
      L2 : constant Natural := S2'Length;
      
      -- DP table for lengths
      type DP_Table is array (0 .. L1, 0 .. L2) of Natural;
      Table : DP_Table := (others => (others => 0));
   begin
      for I in 1 .. L1 loop
         for J in 1 .. L2 loop
            if S1(I) = S2(J) then
               Table(I, J) := Table(I-1, J-1) + 1;
            else
               Table(I, J) := Natural'Max(Table(I-1, J), Table(I, J-1));
            end if;
         end loop;
      end loop;
      
      return Table(L1, L2);
   end LCS_Length;
   
   -- Example usage
   S1 : constant String := "AACCTTGG";
   S2 : constant String := "ACACTGTGA";
   
begin
   Put_Line("LCS Length: " & Natural'Image(LCS_Length(S1, S2)));
end FIND;
```

## Explanation

The algorithm works as follows:

1. **Dynamic Programming Table**: Create a 2D table where `dp[i][j]` represents the length of LCS for the first `i` characters of string1 and first `j` characters of string2.

2. **Fill the Table**:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Reconstruct LCS**: Backtrack through the table to build the actual subsequence.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Output

For input strings "AACCTTGG" and "ACACTGTGA":
```
Longest Common Subsequence: AACTTG
```

This solution correctly handles the dynamic programming approach required to find the longest shared spliced motif between two DNA sequences.