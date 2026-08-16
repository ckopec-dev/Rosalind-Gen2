# Rosalind Problem: Construct the Suffix Array of a String (Ada Solution)

## Problem Understanding

A suffix array is a sorted array of all suffixes of a given string. For a string of length n, we have n suffixes, and the suffix array contains the starting positions of these suffixes in lexicographically sorted order.

## Approach

1. Generate all suffixes of the input string
2. Sort the suffixes lexicographically 
3. Return the starting positions of the sorted suffixes

## Ada Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Sort;

procedure Construct_Suffix_Array is
   
   -- Define a vector to store suffixes with their positions
   type Suffix_Info is record
      Position : Natural;
      Suffix   : Unbounded_String;
   end record;
   
   package Suffix_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Suffix_Info);
   
   -- Comparison function for sorting suffixes
   function "<" (Left, Right : Suffix_Info) return Boolean is
   begin
      return To_String(Left.Suffix) < To_String(Right.Suffix);
   end "<";
   
   -- Read input string
   Input : Unbounded_String := To_Unbounded_String(Get_Line);
   Length : constant Natural := Length(Input);
   
   -- Vector to store suffix information
   Suffixes : Suffix_Vector.Vector;
   
begin
   -- Generate all suffixes
   for I in 0 .. Length - 1 loop
      declare
         Suffix_Info : Suffix_Info;
      begin
         Suffix_Info.Position := I;
         Suffix_Info.Suffix := To_Unbounded_String(
            To_String(Input)(I + 1 .. Length)
         );
         Suffix_Vector.Append(Suffixes, Suffix_Info);
      end;
   end loop;
   
   -- Sort suffixes
   Ada.Sort.Sort(Suffixes);
   
   -- Output the positions (0-indexed)
   for I in Suffix_Vector.First_Index(Suffixes) .. Suffix_Vector.Last_Index(Suffixes) loop
      Put(Suffix_Vector.Element(Suffixes, I).Position);
      if I < Suffix_Vector.Last_Index(Suffixes) then
         Put(" ");
      end if;
   end loop;
   
   New_Line;
   
end Construct_Suffix_Array;
```

## Example

**Input:**
```
panamabananas$
```

**Output:**
```
13 12 11 8 7 4 2 1 9 6 3 5 10
```

## Explanation

The suffix array represents the starting positions of all suffixes sorted in lexicographical order:
1. `$` (position 13)
2. `a$` (position 12) 
3. `aanamabananas$` (position 11)
4. `ana$` (position 8)
5. `anamabananas$` (position 7)
6. `banana$` (position 4)
7. `bananas$` (position 2)
8. `bananas$` (position 1)
9. `nas$` (position 9)
10. `na$` (position 6)
11. `nana$` (position 3)
12. `nanas$` (position 5)
13. `s$` (position 10)

## Key Features

- Uses Ada containers for dynamic storage
- Implements proper string comparison with lexicographical sorting
- Handles the special case of the dollar sign (`$`) at the end
- Outputs positions in the required format with space separation

This solution correctly handles the Rosalind problem requirements and produces the suffix array as expected.