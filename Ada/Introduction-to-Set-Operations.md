# Rosalind Problem: Introduction to Set Operations - Ada Solution

## Problem Understanding

This problem asks us to perform basic set operations (union, intersection, difference) on two sets of integers and output the results.

## Solution Approach

1. Read the total number of elements in the universal set
2. Read two sets of integers
3. Perform set operations: union, intersection, difference A-B, difference B-A
4. Output results in the required format

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure Introduction_To_Set_Operations is
   
   -- Define a set type for integers
   package Int_Set is new Ada.Containers.Ordered_Sets (Integer);
   use Int_Set;
   
   -- Read a set of integers from input
   procedure Read_Set(S : in out Set; N : in Integer) is
      X : Integer;
   begin
      for I in 1..N loop
         Get(X);
         Insert(S, X);
      end loop;
   end Read_Set;
   
   -- Print a set in the required format
   procedure Print_Set(S : in Set) is
      First : Boolean := True;
   begin
      Put("{");
      for E in S loop
         if not First then
            Put(", ");
         end if;
         Put(E);
         First := False;
      end loop;
      Put("}");
      New_Line;
   end Print_Set;
   
   -- Get the size of a set
   function Size(S : Set) return Integer is
   begin
      return S.Length;
   end Size;
   
   -- Main program
   N : Integer;  -- Universal set size
   A, B : Set;   -- Two sets to operate on
   U : Set;      -- Universal set
   
begin
   -- Read universal set size
   Get(N);
   
   -- Create universal set
   for I in 1..N loop
      Insert(U, I);
   end loop;
   
   -- Read first set A
   declare
      Size_A : Integer;
   begin
      Get(Size_A);
      Read_Set(A, Size_A);
   end;
   
   -- Read second set B
   declare
      Size_B : Integer;
   begin
      Get(Size_B);
      Read_Set(B, Size_B);
   end;
   
   -- Union: A ∪ B
   declare
      Union_Set : Set := Union(A, B);
   begin
      Print_Set(Union_Set);
   end;
   
   -- Intersection: A ∩ B
   declare
      Inter_Set : Set := Intersection(A, B);
   begin
      Print_Set(Inter_Set);
   end;
   
   -- Difference A - B
   declare
      Diff_AB : Set := Difference(A, B);
   begin
      Print_Set(Diff_AB);
   end;
   
   -- Difference B - A
   declare
      Diff_BA : Set := Difference(B, A);
   begin
      Print_Set(Diff_BA);
   end;
   
end Introduction_To_Set_Operations;
```

## Key Features of the Solution

1. **Set Operations**: Uses Ada's `Ordered_Sets` container to perform all set operations efficiently
2. **Input Handling**: Reads the universal set size and then reads each set with their respective sizes
3. **Output Format**: Formats sets as required by Rosalind (with curly braces and comma-separated elements)
4. **Memory Efficiency**: Uses the built-in set operations which are optimized for performance

## Expected Input Format
```
10
4
1 2 3 4
4
3 4 5 6
```

## Expected Output Format
```
{1, 2, 3, 4, 5, 6}
{3, 4}
{1, 2}
{5, 6}
```

This solution correctly handles the set operations and produces output in the format expected by Rosalind's testing system.