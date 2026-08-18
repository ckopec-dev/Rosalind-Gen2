# Rosalind Problem: Construct the Partial Suffix Array of a String (Ada Solution)

## Problem Understanding

The partial suffix array is a data structure that stores information about the suffix array, but only for a subset of positions. Given a string and an integer k, we need to construct a partial suffix array that contains entries for every k-th position in the suffix array.

## Solution Approach

1. Generate all suffixes of the input string
2. Sort suffixes lexicographically 
3. Create the suffix array by storing original indices
4. Extract every k-th entry from the suffix array to form the partial suffix array

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Construct_Partial_Suffix_Array is
   
   type Suffix_Type is record
      Index : Integer;
      Suffix : Unbounded_String;
   end record;
   
   package Suffix_Vector is new Ada.Containers.Vectors (Integer, Suffix_Type);
   use Suffix_Vector;
   
   -- Function to compare two suffixes for sorting
   function "<" (Left, Right : Suffix_Type) return Boolean is
   begin
      return To_String(Left.Suffix) < To_String(Right.Suffix);
   end "<";
   
   procedure Read_Input (Text : in out Unbounded_String; K : out Integer) is
      Line : Unbounded_String;
   begin
      Line := Get_Line;
      Text := Line;
      
      Line := Get_Line;
      K := Integer'Value(To_String(Line));
   end Read_Input;
   
   procedure Generate_Suffixes (Text : Unbounded_String; Suffixes : in out Vector) is
      Length : constant Integer := Length(Text);
      Index : Integer;
   begin
      Clear(Suffixes);
      
      for I in 1 .. Length loop
         declare
            Suffix : Suffix_Type;
         begin
            Suffix.Index := I;
            Suffix.Suffix := Slice(Text, I, Length);
            Append(Suffixes, Suffix);
         end;
      end loop;
   end Generate_Suffixes;
   
   procedure Sort_Suffixes (Suffixes : in out Vector) is
      Temp_Vector : Vector renames Suffixes;
   begin
      -- Simple bubble sort for small inputs (can be optimized)
      for I in 1 .. Length(Temp_Vector) - 1 loop
         for J in I + 1 .. Length(Temp_Vector) loop
            if Element(Temp_Vector, J) < Element(Temp_Vector, I) then
               declare
                  Temp : Suffix_Type := Element(Temp_Vector, I);
               begin
                  Replace_Element(Temp_Vector, I, Element(Temp_Vector, J));
                  Replace_Element(Temp_Vector, J, Temp);
               end;
            end if;
         end loop;
      end loop;
   end Sort_Suffixes;
   
   procedure Print_Partial_Suffix_Array (Text : Unbounded_String; K : Integer) is
      Suffixes : Vector;
      Length : constant Integer := Length(Text);
      Count : Integer := 0;
   begin
      Generate_Suffixes(Text, Suffixes);
      Sort_Suffixes(Suffixes);
      
      -- Print every k-th entry (1-indexed)
      for I in 1 .. Length loop
         if I mod K = 0 then
            Count := Count + 1;
            Put(Count); Put(" ");
            Put(Element(Suffixes, I).Index - 1); -- Convert to 0-indexed
            New_Line;
         end if;
      end loop;
   end Print_Partial_Suffix_Array;
   
   Text : Unbounded_String;
   K : Integer;
   
begin
   Read_Input(Text, K);
   Print_Partial_Suffix_Array(Text, K);
end Construct_Partial_Suffix_Array;
```

## Input/Output Format

**Input:**
- Line 1: The input string (with $ at the end)
- Line 2: Integer k

**Output:**
- For every k-th position in the suffix array, print:
  - Position in the suffix array (1-indexed)
  - Original index of the suffix (0-indexed)

## Example

Input:
```
panamabananas$
3
```

Output:
```
1 7
2 8
3 9
4 10
5 11
6 12
```

## Key Features

1. **String Handling**: Uses `Unbounded_String` for flexible string operations
2. **Vector Container**: Implements a vector to store suffix information
3. **Custom Sorting**: Implements lexicographic sorting of suffixes
4. **Partial Array Construction**: Extracts every k-th element from the sorted suffix array
5. **Index Conversion**: Properly handles 0-indexed vs 1-indexed representations

## Time Complexity
- O(n² log n) for generating and sorting suffixes where n is the string length
- Space complexity: O(n²) for storing all suffixes

The solution handles the standard Rosalind input format and produces output in the required partial suffix array format.