# Rosalind Problem: Find All Approximate Occurrences of a Pattern in a String (Ada Solution)

## Problem Understanding

The task is to find all starting positions where a pattern appears in a string with at most `d` mismatches (Hamming distance).

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Find_All_Approximate_Occurrences_of_a_Pattern_in_a_String is
   
   -- Function to calculate Hamming distance between two strings
   function Hamming_Distance(Pattern, Text : String) return Natural is
      Distance : Natural := 0;
   begin
      for I in Pattern'Range loop
         if Pattern(I) /= Text(I) then
            Distance := Distance + 1;
         end if;
      end loop;
      return Distance;
   end Hamming_Distance;
   
   -- Function to check if pattern matches text with at most d mismatches
   function Is_Approximate_Match(Pattern, Text : String; d : Natural) return Boolean is
   begin
      return Hamming_Distance(Pattern, Text) <= d;
   end Is_Approximate_Match;
   
   -- Main function to find all approximate occurrences
   procedure Find_Approximate_Occurrences(Pattern, Text : String; d : Natural) is
      Positions : array(1..Text'Length) of Natural;
      Count : Natural := 0;
      Pattern_Length : constant Natural := Pattern'Length;
   begin
      -- Check each possible starting position in text
      for I in 1..Text'Length - Pattern_Length + 1 loop
         -- Extract substring of same length as pattern
         declare
            Substring : constant String := Text(I..I + Pattern_Length - 1);
         begin
            if Is_Approximate_Match(Pattern, Substring, d) then
               Count := Count + 1;
               Positions(Count) := I;
            end if;
         end;
      end loop;
      
      -- Output all positions (1-indexed)
      for I in 1..Count loop
         Put(Positions(I)); Put(" ");
      end loop;
      New_Line;
   end Find_Approximate_Occurrences;
   
   -- Read input from stdin
   Pattern : String(1..100);
   Text : String(1..1000);
   d : Natural;
   Pattern_Length, Text_Length : Natural;
   
begin
   -- Read pattern and text
   Get_Line(Pattern, Pattern_Length);
   Get_Line(Text, Text_Length);
   
   -- Read d value
   Get(d);
   
   -- Find and output approximate occurrences
   Find_Approximate_Occurrences(Pattern(1..Pattern_Length), Text(1..Text_Length), d);
end Find_All_Approximate_Occurrences_of_a_Pattern_in_a_String;
```

## Example Usage

**Input:**
```
ATTCTGGA
CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCGGCACGATCAGAACCTGAGGCCGTCTNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN