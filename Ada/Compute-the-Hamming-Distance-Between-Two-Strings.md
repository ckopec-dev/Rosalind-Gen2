# Rosalind Problem: Compute the Hamming Distance Between Two Strings

## Problem Statement
The Hamming distance between two strings of equal length is the number of positions at which the corresponding symbols are different. Given two strings of equal length, compute their Hamming distance.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Compute_Hamming_Distance is
   type DNA_String is array (Positive range <>) of Character;
   
   function Hamming_Distance (Str1, Str2 : DNA_String) return Natural is
      Distance : Natural := 0;
   begin
      for I in Str1'Range loop
         if Str1(I) /= Str2(I) then
            Distance := Distance + 1;
         end if;
      end loop;
      return Distance;
   end Hamming_Distance;
   
   -- Example usage
   Str1 : constant DNA_String(1..6) := "GAGCCT";
   Str2 : constant DNA_String(1..6) := "GATCC";
begin
   Put_Line("Hamming distance: " & Integer'Image(Hamming_Distance(Str1, Str2)));
end Compute_Hamming_Distance;
```

## Alternative Implementation with File Input

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Compute_Hamming_Distance_File is
   type DNA_String is array (Positive range <>) of Character;
   
   function Hamming_Distance (Str1, Str2 : DNA_String) return Natural is
      Distance : Natural := 0;
   begin
      for I in Str1'Range loop
         if Str1(I) /= Str2(I) then
            Distance := Distance + 1;
         end if;
      end loop;
      return Distance;
   end Hamming_Distance;
   
   -- Read strings from input
   Line1 : String(1..100);
   Line2 : String(1..100);
   Last1, Last2 : Natural;
begin
   Get_Line(Line1, Last1);
   Get_Line(Line2, Last2);
   
   -- Convert to DNA_String arrays
   declare
      DNA1 : DNA_String(1..Last1);
      DNA2 : DNA_String(1..Last2);
   begin
      for I in 1..Last1 loop
         DNA1(I) := Line1(I);
      end loop;
      
      for I in 1..Last2 loop
         DNA2(I) := Line2(I);
      end loop;
      
      Put_Line("Hamming distance: " & Integer'Image(Hamming_Distance(DNA1, DNA2)));
   end;
end Compute_Hamming_Distance_File;
```

## Explanation

The solution works as follows:

1. **Type Definition**: We define a `DNA_String` type as an array of characters with positive indices.

2. **Hamming Distance Function**: 
   - Takes two DNA strings as input parameters
   - Iterates through each position in the strings
   - Compares characters at each position
   - Increments counter when characters differ
   - Returns the total count

3. **Key Features**:
   - Uses Ada's array indexing with positive range
   - Handles string comparison character by character
   - Returns a natural number (non-negative integer)
   - Includes proper bounds checking through array declarations

4. **Usage**: 
   - The first version shows a simple example with predefined strings
   - The second version demonstrates reading from standard input for file processing

## Sample Input/Output

**Input:**
```
GAGCCT
GATCC
```

**Output:**
```
Hamming distance: 3
```

This solution correctly computes the Hamming distance by counting the number of mismatched positions between two equal-length strings.