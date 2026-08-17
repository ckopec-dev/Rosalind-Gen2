# Rosalind Problem: Find All Approximate Occurrences of a Collection of Patterns in a String

## Problem Description
Given a string Text and a collection of strings Patterns, find all approximate occurrences of each pattern in Text with at most d mismatches.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Find_All_Approximate_Occurrences_of_a_Collection_of_Patterns_in_a_String is
   
   type String_Array is array (Positive range <>) of Unbounded_String;
   
   function Hamming_Distance(s1, s2 : Unbounded_String) return Natural is
      count : Natural := 0;
   begin
      for i in 1 .. Length(s1) loop
         if Element(s1, i) /= Element(s2, i) then
            count := count + 1;
         end if;
      end loop;
      return count;
   end Hamming_Distance;
   
   function Approximate_Match(pattern, text : Unbounded_String; d : Natural) 
      return String_Array is
      result : String_Array(1..Length(text));
      count : Natural := 0;
      pattern_len : constant Natural := Length(pattern);
   begin
      for i in 1 .. Length(text) - pattern_len + 1 loop
         declare
            substring : Unbounded_String := 
               Slice(text, i, i + pattern_len - 1);
         begin
            if Hamming_Distance(pattern, substring) <= d then
               count := count + 1;
               result(count) := To_Unbounded_String(Integer'Image(i-1));
            end if;
         end;
      end loop;
      
      return result(1..count);
   end Approximate_Match;
   
   procedure Print_Array(arr : String_Array; length : Natural) is
   begin
      for i in 1 .. length loop
         Put(arr(i));
         if i < length then
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Print_Array;
   
   -- Main program
   Text : constant Unbounded_String := To_Unbounded_String("ACAACTATGCATACTATCGGGAACTATCCT");
   Patterns : constant String_Array(1..3) := 
     (To_Unbounded_String("ATT"), 
      To_Unbounded_String("ACT"), 
      To_Unbounded_String("AGT"));
   d : constant Natural := 2;
   
   result : String_Array(1..100);
   total_count : Natural := 0;
   
begin
   for i in Patterns'Range loop
      declare
         pattern_result : String_Array := Approximate_Match(Patterns(i), Text, d);
      begin
         for j in pattern_result'Range loop
            if j <= 100 then  -- Prevent overflow
               total_count := total_count + 1;
               result(total_count) := pattern_result(j);
            end if;
         end loop;
      end;
   end loop;
   
   Print_Array(result, total_count);
end Find_All_Approximate_Occurrences_of_a_Collection_of_Patterns_in_a_String;
```

## Explanation

This Ada solution works as follows:

1. **Data Structures**: 
   - Uses `Unbounded_String` for flexible string handling
   - Defines a `String_Array` type for storing results

2. **Hamming Distance Function**:
   - Calculates the number of mismatches between two strings
   - Compares each character position and counts differences

3. **Approximate Match Function**:
   - Finds all occurrences of a pattern in text with at most d mismatches
   - Slides through the text and checks each substring against the pattern
   - Uses Hamming distance to determine if match is acceptable

4. **Main Logic**:
   - Processes each pattern in the collection
   - Collects all approximate matches for each pattern
   - Outputs the results as space-separated indices

## Input/Output Example

For input:
- Text: "ACAACTATGCATACTATCGGGAACTATCCT"
- Patterns: ["ATT", "ACT", "AGT"]  
- d: 2

The program would find all positions where each pattern occurs with at most 2 mismatches and output the indices.

## Key Features

- **Robust String Handling**: Uses Ada's `Unbounded_String` for dynamic string operations
- **Modular Design**: Separate functions for Hamming distance and approximate matching
- **Error Prevention**: Includes bounds checking to prevent array overflow
- **Clear Output Format**: Produces space-separated indices as expected by Rosalind

This solution efficiently handles the approximate pattern matching problem while maintaining Ada's strong typing and safety features.