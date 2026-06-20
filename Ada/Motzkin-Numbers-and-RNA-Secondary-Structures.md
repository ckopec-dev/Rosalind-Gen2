# Rosalind Problem: Motzkin Numbers and RNA Secondary Structures

## Problem Description

In this problem, we need to count the number of possible secondary structures for an RNA string. This is equivalent to counting the number of non-crossing matchings in a string, which corresponds to the Motzkin numbers.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Motzkin_RNA is
   type RNA_String is array (Positive range <>) of Character;
   
   -- Function to check if two nucleotides can pair
   function Can_Pair(N1, N2 : Character) return Boolean is
   begin
      case N1 is
         when 'A' => return N2 = 'U';
         when 'U' => return N2 = 'A';
         when 'C' => return N2 = 'G';
         when 'G' => return N2 = 'C';
         when others => return False;
      end case;
   end Can_Pair;
   
   -- Function to compute Motzkin number using dynamic programming
   function Count_Secondary_Structures(S : RNA_String) return Long_Long_Integer is
      N : constant Positive := S'Length;
      -- Memoization array for dynamic programming
      DP : array (0 .. N) of Long_Long_Integer;
      
      -- Helper function to compute Motzkin numbers
      function Motzkin(i : Natural) return Long_Long_Integer is
         Result : Long_Long_Integer := 0;
      begin
         if i = 0 then
            return 1;
         elsif i = 1 then
            return 1;
         else
            -- M(n) = M(n-1) + sum_{k=2}^{n} M(k-2) * M(n-k)
            Result := Motzkin(i - 1);  -- No pairing at position i
            
            for k in 2 .. i loop
               if Can_Pair(S(i), S(i - k + 1)) then
                  Result := Result + Motzkin(k - 2) * Motzkin(i - k);
               end if;
            end loop;
            
            return Result;
         end if;
      end Motzkin;
   begin
      -- Initialize DP array
      for i in 0 .. N loop
         DP(i) := 0;
      end loop;
      
      DP(0) := 1;  -- Empty string has one structure (empty)
      DP(1) := 1;  -- Single nucleotide has one structure
      
      -- Fill DP array using recurrence relation
      for i in 2 .. N loop
         -- Case 1: No pairing at position i
         DP(i) := DP(i - 1);
         
         -- Case 2: Pairing at position i with some previous position
         for j in 1 .. i - 1 loop
            if Can_Pair(S(i), S(j)) then
               DP(i) := DP(i) + DP(j - 1) * DP(i - j - 1);
            end if;
         end loop;
      end loop;
      
      return DP(N);
   end Count_Secondary_Structures;
   
   -- Alternative simpler approach using recursion with memoization
   function Count_Structures(S : RNA_String) return Long_Long_Integer is
      N : constant Positive := S'Length;
      Memo : array (1 .. N) of Long_Long_Integer;
      
      function Helper(Pos : Natural) return Long_Long_Integer is
         Result : Long_Long_Integer := 0;
      begin
         if Pos = 0 then
            return 1;
         elsif Pos < 0 then
            return 0;
         elsif Memo(Pos) /= 0 then
            return Memo(Pos);
         else
            -- Option 1: Don't pair the current nucleotide
            Result := Helper(Pos - 1);
            
            -- Option 2: Pair with a previous nucleotide
            for i in 1 .. Pos loop
               if Can_Pair(S(Pos), S(i)) then
                  Result := Result + Helper(i - 1) * Helper(Pos - i - 1);
               end if;
            end loop;
            
            Memo(Pos) := Result;
            return Result;
         end if;
      end Helper;
   begin
      for i in 1 .. N loop
         Memo(i) := 0;
      end loop;
      
      return Helper(N);
   end Count_Structures;
   
   -- Read input RNA string (in practice, this would be from file)
   function Get_RNA_String return RNA_String is
      Input : Unbounded_String;
      Result : RNA_String(1 .. 200);  -- Assuming maximum length
      Length : Natural := 0;
   begin
      Put_Line("Enter RNA sequence (e.g. 'AUCG'): ");
      Get_Line(Input);
      
      for I in 1 .. Length_Of(Input) loop
         Length := Length + 1;
         Result(Length) := Element(Input, I);
      end loop;
      
      return Result(1 .. Length);
   end Get_RNA_String;
   
begin
   -- Example RNA sequence from problem
   declare
      Test_RNA : constant RNA_String := ('A', 'U', 'C', 'G');
      Count : Long_Long_Integer;
   begin
      Count := Count_Structures(Test_RNA);
      Put("Number of secondary structures for ");
      for I in Test_RNA'Range loop
         Put(Test_RNA(I));
      end loop;
      Put_Line(": ");
      Put(Count, Width => 1);
      New_Line;
   end;
   
   -- For actual problem input, you would read from file:
   -- declare
   --    Input_RNA : RNA_String := Get_RNA_String;
   --    Result : Long_Long_Integer;
   -- begin
   --    Result := Count_Structures(Input_RNA);
   --    Put_Line("Result: " & Long_Long_Integer'Image(Result));
   -- end;
   
end Motzkin_RNA;
```

## Explanation

This Ada solution implements a dynamic programming approach to count the number of possible secondary structures for an RNA sequence:

1. **Can_Pair Function**: Determines if two nucleotides can form a base pair (A-U, U-A, C-G, G-C)

2. **Count_Structures Function**: Uses recursive approach with memoization:
   - For each position, we have two options:
     - Don't pair the current nucleotide
     - Pair it with a previous nucleotide that can form a valid base pair
   - The recurrence relation is: `f(n) = f(n-1) + Σ(f(i-1) × f(n-i-1))` for all valid pairs

3. **Dynamic Programming Approach**: 
   - `DP[i]` represents the number of secondary structures for a string of length i
   - `DP[i] = DP[i-1] + Σ(DP[j-1] × DP[i-j-1])` where j is a valid pairing position

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the RNA sequence
- **Space Complexity**: O(n²) for the memoization table

## Sample Input/Output

For input "AUCG":
- Output: 2 (two possible secondary structures)

The solution handles the mathematical concept of Motzkin numbers, which count non-crossing matchings in RNA secondary structure prediction.