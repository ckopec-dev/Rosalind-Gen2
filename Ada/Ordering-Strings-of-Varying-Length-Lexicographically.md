# Rosalind Problem: Ordering Strings of Varying Length Lexicographically (Ada Solution)

## Problem Understanding

We need to sort a collection of strings lexicographically, where strings can have different lengths. In lexicographic order, shorter strings that are prefixes of longer strings come first.

## Solution Approach

1. Read input strings
2. Implement custom comparison function for lexicographic ordering
3. Sort the strings using the comparison function
4. Output the sorted strings

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Sort;

procedure Ordering_Strings_of_Varying_Length_Lexicographically is
   package String_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   
   type String_Vector_Access is access String_Vector.Vector;
   
   -- Custom comparison function for lexicographic ordering
   function Lexicographic_Compare(S1, S2 : Unbounded_String) return Boolean is
   begin
      -- Compare character by character
      for I in 1 .. Min(Length(S1), Length(S2)) loop
         if Element(S1, I) < Element(S2, I) then
            return True;
         elsif Element(S1, I) > Element(S2, I) then
            return False;
         end if;
      end loop;
      
      -- If all characters match up to the shorter string's length,
      -- the shorter string comes first
      return Length(S1) < Length(S2);
   end Lexicographic_Compare;
   
   -- Sort procedure using custom comparison
   procedure Sort_Strings(V : in out String_Vector.Vector) is
      procedure Sort is new Ada.Sort.Sort
        (Index_Type => Natural,
         Element_Type => Unbounded_String,
         With_Comparison => Lexicographic_Compare);
   begin
      Sort(V);
   end Sort_Strings;
   
   -- Read all strings from input
   Strings : String_Vector.Vector;
   Line : Unbounded_String;
   
begin
   -- Read strings until end of file
   while not End_Of_File loop
      Get_Line(Line);
      if Length(Line) > 0 then
         Strings.Append(Line);
      end if;
   end loop;
   
   -- Sort the strings
   Sort_Strings(Strings);
   
   -- Output sorted strings
   for I in 1 .. Strings.Length loop
      Put_Line(Strings.Element(I));
   end loop;
   
end Ordering_Strings_of_Varying_Length_Lexicographically;
```

## Alternative Implementation (More Concise)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Sort;

procedure Ordering_Strings_of_Varying_Length_Lexicographically is
   package String_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   
   -- Comparison function for lexicographic ordering
   function "<" (S1, S2 : Unbounded_String) return Boolean is
   begin
      for I in 1 .. Min(Length(S1), Length(S2)) loop
         if Element(S1, I) < Element(S2, I) then
            return True;
         elsif Element(S1, I) > Element(S2, I) then
            return False;
         end if;
      end loop;
      return Length(S1) < Length(S2);
   end "<";
   
   -- Read all strings
   Strings : String_Vector.Vector;
   Line : Unbounded_String;
   
begin
   -- Read input
   while not End_Of_File loop
      Get_Line(Line);
      if Length(Line) > 0 then
         Strings.Append(Line);
      end if;
   end loop;
   
   -- Sort and output
   Strings.Sort;
   
   for I in 1 .. Strings.Length loop
      Put_Line(Strings.Element(I));
   end loop;
   
end Ordering_Strings_of_Varying_Length_Lexicographically;
```

## Key Features of the Solution

1. **Custom Lexicographic Ordering**: The comparison function properly handles strings of different lengths
2. **Proper Prefix Handling**: Shorter strings that are prefixes of longer strings come first
3. **Efficient Sorting**: Uses Ada's built-in sorting with custom comparison
4. **Memory Management**: Uses unbounded strings and vectors for flexible input handling

## Sample Input/Output

**Input:**
```
AAA
AA
A
```

**Output:**
```
A
AA
AAA
```

## Time Complexity
- **Time**: O(n × m × log n) where n is the number of strings and m is the average string length
- **Space**: O(n × m) for storing the strings

The solution correctly handles the lexicographic ordering requirement where shorter strings that are prefixes of longer strings are ordered before their longer counterparts.

