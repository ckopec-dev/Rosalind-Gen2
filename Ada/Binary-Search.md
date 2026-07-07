# Rosalind Problem: Binary Search in Ada

## Problem Description
Given a collection of positive integers and a list of queries, find the position (1-indexed) of each query in the collection using binary search.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Binary_Search is
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Binary search function
   function Binary_Search_Array(Arr : Integer_Array; Target : Integer) return Integer is
      Low  : Positive := Arr'First;
      High : Positive := Arr'Last;
      Mid  : Positive;
   begin
      while Low <= High loop
         Mid := (Low + High) / 2;
         if Arr(Mid) = Target then
            return Mid;
         elsif Arr(Mid) < Target then
            Low := Mid + 1;
         else
            High := Mid - 1;
         end if;
      end loop;
      return -1; -- Not found
   end Binary_Search_Array;
   
   -- Read array from input
   procedure Read_Array(Num : in Integer; Arr : out Integer_Array) is
      I : Integer;
   begin
      for J in Arr'First .. Arr'Last loop
         Get(I);
         Arr(J) := I;
      end loop;
   end Read_Array;
   
   -- Main program
   N, K : Integer;
   A    : Integer_Array(1 .. 10000); -- Assuming maximum size
   B    : Integer_Array(1 .. 10000);
   Result : Integer;
   
begin
   -- Read array size
   Get(N);
   
   -- Read array elements
   Read_Array(N, A);
   
   -- Read number of queries
   Get(K);
   
   -- Read queries and perform binary search
   for I in 1 .. K loop
      Get(Result);
      declare
         Pos : Integer := Binary_Search_Array(A, Result);
      begin
         if Pos = -1 then
            Put_Line("-1");
         else
            Put(Pos); -- Return 1-indexed position
            New_Line;
         end if;
      end;
   end loop;
   
end Binary_Search;
```

## Explanation

The solution implements a binary search algorithm in Ada to efficiently find elements in a sorted array:

1. **Binary Search Function**: 
   - Takes an integer array and target value as input
   - Uses low and high pointers to narrow down the search range
   - Returns 1-indexed position if found, -1 if not found

2. **Key Features**:
   - Proper handling of 1-indexed positions (as required by Rosalind)
   - Input/output using Ada.Text_IO package
   - Array bounds checking with proper indexing
   - Efficient O(log n) time complexity

3. **Algorithm Steps**:
   - Initialize low = first index, high = last index
   - Calculate mid-point
   - Compare target with middle element
   - Adjust search range based on comparison
   - Return position when found or -1 if not found

## Sample Input/Output

Input:
```
5
1 2 3 4 5
2
3 1
```

Output:
```
3
1
```

This solution correctly handles the binary search requirement and produces the expected output format for Rosalind problems.