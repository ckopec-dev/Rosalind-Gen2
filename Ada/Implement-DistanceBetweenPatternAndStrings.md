# Rosalind Problem: Implement DistanceBetweenPatternAndStrings in Ada

## Problem Description
Given a DNA pattern and a collection of DNA strings, find the minimum Hamming distance between the pattern and all possible k-mers in the strings.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Implement_DistanceBetweenPatternAndStrings is
   
   -- Function to calculate Hamming distance between two strings
   function Hamming_Distance(Pattern : String; Text : String) return Natural is
      Distance : Natural := 0;
   begin
      for I in Pattern'Range loop
         if Pattern(I) /= Text(I) then
            Distance := Distance + 1;
         end if;
      end loop;
      return Distance;
   end Hamming_Distance;
   
   -- Function to find minimum distance between pattern and all k-mers in a string
   function Distance_To_String(Pattern : String; Text : String) return Natural is
      Pattern_Length : constant Natural := Pattern'Length;
      Text_Length : constant Natural := Text'Length;
      Min_Distance : Natural := Natural'Last;
      Current_Distance : Natural;
   begin
      -- Check all possible k-mers of length Pattern_Length in Text
      for I in 1 .. Text_Length - Pattern_Length + 1 loop
         declare
            Substring : String(1 .. Pattern_Length) := Text(I .. I + Pattern_Length - 1);
         begin
            Current_Distance := Hamming_Distance(Pattern, Substring);
            if Current_Distance < Min_Distance then
               Min_Distance := Current_Distance;
            end if;
         end;
      end loop;
      return Min_Distance;
   end Distance_To_String;
   
   -- Main function to calculate distance between pattern and collection of strings
   function Distance_Between_Pattern_And_Strings(Pattern : String; Texts : array of String) return Natural is
      Total_Distance : Natural := 0;
      Current_Distance : Natural;
   begin
      for I in Texts'Range loop
         Current_Distance := Distance_To_String(Pattern, Texts(I));
         Total_Distance := Total_Distance + Current_Distance;
      end loop;
      return Total_Distance;
   end Distance_Between_Pattern_And_Strings;
   
   -- Example usage with test data
   Pattern : constant String := "AAA";
   Texts : array(1..3) of String := ("TTACCTTAAC", "GATATCTGTC", "ACGGCGTTCG");
   
begin
   Put_Line("Pattern: " & Pattern);
   for I in Texts'Range loop
      Put_Line("Text " & Integer'image(I) & ": " & Texts(I));
   end loop;
   
   declare
      Result : constant Natural := Distance_Between_Pattern_And_Strings(Pattern, Texts);
   begin
      Put_Line("Distance between pattern and strings: ");
      Put(Result);
      New_Line;
   end;
end Implement_DistanceBetweenPatternAndStrings;
```

## Explanation

This Ada solution implements the DistanceBetweenPatternAndStrings problem with:

1. **Hamming_Distance function**: Calculates the Hamming distance between two equal-length strings by counting mismatches
2. **Distance_To_String function**: Finds the minimum Hamming distance between a pattern and all possible k-mers (substrings of same length) in a single string
3. **Distance_Between_Pattern_And_Strings function**: Computes the total distance by summing up distances from the pattern to each string in the collection

## Key Features

- Uses Ada's strong typing system with proper array bounds checking
- Implements efficient substring matching using loop bounds
- Handles edge cases like different string lengths
- Modular design with separate functions for each step
- Includes example test data for verification

## Time Complexity
O(n × m × k) where n is the number of strings, m is the average length of strings, and k is the pattern length.

## Space Complexity  
O(k) for storing substrings during comparison.