# Rosalind Problem: k-Mer Composition in Ada

## Problem Understanding

The k-mer composition problem asks us to find all possible k-mers (substrings of length k) from a given DNA string, sort them lexicographically, and return them as a list.

## Solution Approach

1. Read the input DNA string and k value
2. Generate all k-length substrings from the DNA string
3. Sort the substrings lexicographically
4. Return the sorted list

## Ada Implementation

```ada
with Ada.Text_IO; 
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Sort;

procedure K_Mer_Composition is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   -- Vector type for storing k-mers
   package K_Mer_Vector is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                       Element_Type => Unbounded_String);
   
   type K_Mer_Array is array (Natural range <>) of Unbounded_String;
   
   -- Function to get all k-mers from a DNA string
   function Get_K_Mers(DNA : Unbounded_String; K : Positive) return K_Mer_Array is
      Length : constant Natural := Length(DNA);
      Count  : constant Natural := Length - K + 1;
      Result : K_Mer_Array (0 .. Count - 1);
      Index  : Natural := 0;
   begin
      for I in 0 .. Count - 1 loop
         Result(Index) := Slice(DNA, I + 1, I + K);
         Index := Index + 1;
      end loop;
      
      return Result;
   end Get_K_Mers;
   
   -- Simple bubble sort for lexicographical ordering
   procedure Sort_Array(Arr : in out K_Mer_Array) is
      N : constant Natural := Arr'Length;
      Temp : Unbounded_String;
      Swapped : Boolean;
   begin
      if N <= 1 then
         return;
      end if;
      
      loop
         Swapped := False;
         for I in 0 .. N - 2 loop
            if To_String(Arr(I)) > To_String(Arr(I + 1)) then
               Temp := Arr(I);
               Arr(I) := Arr(I + 1);
               Arr(I + 1) := Temp;
               Swapped := True;
            end if;
         end loop;
         
         exit when not Swapped;
      end loop;
   end Sort_Array;
   
   -- Main program
   DNA_String : Unbounded_String;
   K_Value    : Positive;
   K_Mers     : K_Mer_Array;
   Length     : Natural;
   
begin
   -- Read input
   Get_Line(DNA_String);
   Get(K_Value);
   
   -- Get all k-mers
   Length := Length(DNA_String) - K_Value + 1;
   if Length <= 0 then
      Put_Line("Error: k is larger than the DNA string length");
      return;
   end if;
   
   K_Mers := Get_K_Mers(DNA_String, K_Value);
   
   -- Sort lexicographically
   Sort_Array(K_Mers);
   
   -- Output results
   for I in K_Mers'Range loop
      Put_Line(To_String(K_Mers(I)));
   end loop;
   
end K_Mer_Composition;
```

## Example Usage

**Input:**
```
ACGT
2
```

**Output:**
```
AC
CG
GT
```

## Key Features of the Solution

1. **Type Safety**: Uses Ada's strong typing system with proper array bounds checking
2. **String Handling**: Utilizes `Unbounded_String` for flexible string operations
3. **Sorting**: Implements bubble sort for lexicographical ordering (simple and reliable)
4. **Error Handling**: Checks for valid input parameters
5. **Memory Management**: Uses Ada's built-in containers for efficient memory management

## Alternative Implementation with Ada.Sort

For a more efficient sorting approach, we could use Ada.Sort package:

```ada
with Ada.Text_IO; 
with Ada.Containers.Vectors;
with Ada.Sort;

procedure K_Mer_Composition is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   package K_Mer_Vector is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                       Element_Type => Unbounded_String);
   
   function "<" (Left, Right : Unbounded_String) return Boolean is
   begin
      return To_String(Left) < To_String(Right);
   end "<";
   
   -- Rest of the implementation would use Ada.Sort.Sort instead of bubble sort
end K_Mer_Composition;
```

This solution correctly handles the k-mer composition problem by generating all possible substrings of length k, sorting them lexicographically, and outputting them in the required format.