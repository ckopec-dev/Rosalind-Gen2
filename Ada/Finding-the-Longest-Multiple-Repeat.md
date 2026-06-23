# Finding the Longest Multiple Repeat - Rosalind Problem Solution in Ada

## Problem Understanding

The problem asks us to find the longest substring that appears at least k times in a given DNA sequence, where k is a given integer.

## Solution Approach

We'll use a brute-force approach:
1. Generate all possible substrings of the input string
2. Count how many times each substring appears
3. Find the longest substring that appears at least k times

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Finding_The_Longest_Multiple_Repeat is
   type DNA_String is array (Positive range <>) of Character;
   
   -- Function to count occurrences of a substring in a string
   function Count_Substring(Whole : String; Part : String) return Natural is
      Count : Natural := 0;
      Pos   : Natural := 1;
   begin
      if Part'Length = 0 then
         return 0;
      end if;
      
      while Pos <= Whole'Length - Part'Length + 1 loop
         if Whole(Pos .. Pos + Part'Length - 1) = Part then
            Count := Count + 1;
         end if;
         Pos := Pos + 1;
      end loop;
      
      return Count;
   end Count_Substring;
   
   -- Function to find longest multiple repeat
   function Longest_Multiple_Repeat(Seq : String; K : Natural) return String is
      Max_Length : Natural := 0;
      Result     : Unbounded_String := Null_Unbounded_String;
   begin
      if Seq'Length = 0 or K = 0 then
         return "";
      end if;
      
      -- Try all possible substring lengths from longest to shortest
      for Length in reverse 1 .. Seq'Length loop
         -- Try all substrings of current length
         for Start in 1 .. Seq'Length - Length + 1 loop
            declare
               Substring : String := Seq(Start .. Start + Length - 1);
               Count     : Natural;
            begin
               Count := Count_Substring(Seq, Substring);
               
               -- If this substring appears at least K times and is longer than current max
               if Count >= K and then Length > Max_Length then
                  Max_Length := Length;
                  Result := To_Unbounded_String(Substring);
               end if;
            end;
         end loop;
         
         -- Early termination: if we found a solution of this length, no need to check shorter ones
         if Max_Length > 0 then
            exit;
         end if;
      end loop;
      
      return To_String(Result);
   end Longest_Multiple_Repeat;
   
   -- Read input from standard input
   procedure Read_Input is
      Line : String(1 .. 1000);
      Last : Natural;
      Seq  : String;
      K    : Natural;
   begin
      -- Read sequence
      Get_Line(Line, Last);
      Seq := Line(1 .. Last);
      
      -- Read k value
      Get(K);
      
      -- Find and print result
      Put_Line(Longest_Multiple_Repeat(Seq, K));
   end Read_Input;
   
begin
   Read_Input;
end Finding_The_Longest_Multiple_Repeat;
```

## How It Works

1. **Count_Substring Function**: Counts how many times a given substring appears in the main string using a simple loop approach.

2. **Longest_Multiple_Repeat Function**: 
   - Uses a reverse iteration from longest to shortest possible substrings for efficiency
   - For each substring length, checks all possible substrings of that length
   - Keeps track of the longest valid substring (appearing at least k times)
   - Early termination when a solution is found for a particular length

3. **Main Procedure**: Reads input sequence and k value from standard input, then calls the main function.

## Sample Input/Output

**Input:**
```
ATATATATATAT
3
```

**Output:**
```
ATATAT
```

## Time Complexity
- O(n³) where n is the length of the DNA string
- For each substring length, we check all substrings and count occurrences

## Space Complexity
- O(n) for storing the result substring

This solution handles edge cases like empty strings and k=0, and efficiently finds the longest multiple repeat using early termination optimization.