# Rosalind Problem: Generate the d-Neighborhood of a String (Ada Solution)

## Problem Description
The d-neighborhood of a string is the set of all strings that have Hamming distance at most d from the original string.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure D_Neighborhood is
   
   type DNA_String is array (Positive range <>) of Character;
   
   function Hamming_Distance(s1, s2 : DNA_String) return Natural is
      count : Natural := 0;
   begin
      for i in s1'Range loop
         if s1(i) /= s2(i) then
            count := count + 1;
         end if;
      end loop;
      return count;
   end Hamming_Distance;
   
   procedure Generate_Neighbors(
      pattern : DNA_String;
      d       : Natural;
      neighbors : in out Unbounded_String;
      current : DNA_String;
      pos     : Positive := 1
   ) is
      nucleotides : constant array (1..4) of Character := ('A', 'C', 'G', 'T');
   begin
      if pos > pattern'Length then
         if Hamming_Distance(pattern, current) <= d then
            Put_Line(current);
         end if;
         return;
      end if;
      
      -- If we haven't exceeded the distance yet, continue with current character
      if Hamming_Distance(pattern, current) <= d then
         current(pos) := pattern(pos);
         Generate_Neighbors(pattern, d, neighbors, current, pos + 1);
      end if;
      
      -- Try all possible mutations at current position
      for i in nucleotides'Range loop
         if nucleotides(i) /= pattern(pos) then
            current(pos) := nucleotides(i);
            Generate_Neighbors(pattern, d, neighbors, current, pos + 1);
         end if;
      end loop;
   end Generate_Neighbors;
   
   function Get_All_Neighbors(pattern : DNA_String; d : Natural) return Unbounded_String is
      neighbors : Unbounded_String;
      current   : DNA_String(pattern'Range);
   begin
      Generate_Neighbors(pattern, d, neighbors, current);
      return neighbors;
   end Get_All_Neighbors;
   
   -- Main procedure to read input and solve the problem
   procedure Solve is
      pattern : Unbounded_String := To_Unbounded_String("ACGT");
      d       : Natural := 2;
      neighbors : Unbounded_String;
   begin
      -- Read inputs from stdin (or use defaults for demonstration)
      Put_Line("Enter DNA string:");
      Get_Line(pattern);
      
      Put_Line("Enter distance d:");
      Get(d);
      
      -- For demonstration, we'll also show the function approach
      Put_Line("d-neighborhood of " & To_String(pattern) & " with d=" & Integer'Image(d));
      Put_Line("---");
      
      -- Simple approach for small inputs - generate all strings of length n and check distance
      declare
         n : constant Natural := Length(pattern);
         nucleotides : constant array (1..4) of Character := ('A', 'C', 'G', 'T');
         result : array (1..(4**n)) of DNA_String(1..n);
         count  : Natural := 0;
      begin
         -- Generate all possible strings of length n
         declare
            procedure Generate_All(
               current : in out DNA_String;
               pos     : Positive := 1
            ) is
            begin
               if pos > n then
                  -- Check Hamming distance
                  if Hamming_Distance(pattern, current) <= d then
                     count := count + 1;
                     result(count) := current;
                  end if;
                  return;
               end if;
               
               for i in nucleotides'Range loop
                  current(pos) := nucleotides(i);
                  Generate_All(current, pos + 1);
               end loop;
            end Generate_All;
            
            current : DNA_String(1..n);
         begin
            Generate_All(current);
            
            -- Output results
            for i in 1..count loop
               Put_Line(result(i));
            end loop;
         end;
      end;
   end Solve;
   
begin
   Solve;
end D_Neighborhood;
```

## Explanation

This Ada solution generates the d-neighborhood of a DNA string by:

1. **Hamming Distance Function**: Calculates the number of positions where two strings differ
2. **Recursive Generation**: Uses recursion to build all possible strings that are at most distance d from the original
3. **Backtracking Approach**: 
   - At each position, either keep the original character or try all 3 other nucleotides
   - Only continues if the current Hamming distance doesn't exceed d

## Key Features

- Uses Ada's strong typing system with arrays and proper bounds checking
- Implements recursive backtracking algorithm
- Handles DNA strings of any length
- Efficiently prunes search space by checking Hamming distance early

## Sample Input/Output

For input:
```
ACGT
2
```

Output would be all strings that differ from "ACGT" in at most 2 positions.

Note: The solution includes both a direct recursive approach and a more systematic generation of all possible strings to demonstrate the concept clearly.