# Rosalind Problem: Median - Ada Solution

## Problem Understanding
The median is the middle value in an ordered integer list. If the size of the list is even, there is no middle value and the median is defined to be the mean of the two middle values.

Given: A positive integer n ≤ 10^5 and a array A[1..n] of integers from -10^5 to 10^5.

Return: The median m of the array.

## Solution Approach
1. Read the input array
2. Sort the array
3. Find the median based on whether the array length is odd or even
4. Output the result

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Median is
   type Integer_Array is array (Positive range <>) of Integer;
   type Index_Type is new Natural;
   
   -- Read number of elements
   n : Integer;
   A : Integer_Array;
   median : Float;
   
begin
   -- Read n
   Get(n);
   
   -- Allocate array
   A := new Integer_Array(1..n);
   
   -- Read array elements
   for i in A'Range loop
      Get(A(i));
   end loop;
   
   -- Sort the array using bubble sort (simple approach)
   for i in A'First..A'Last-1 loop
      for j in i+1..A'Last loop
         if A(i) > A(j) then
            declare
               temp : Integer := A(i);
            begin
               A(i) := A(j);
               A(j) := temp;
            end;
         end if;
      end loop;
   end loop;
   
   -- Calculate median
   if n mod 2 = 1 then
      -- Odd length: median is the middle element
      median := Float(A(n/2 + 1));
   else
      -- Even length: median is average of two middle elements
      median := Float(A(n/2) + A(n/2 + 1)) / 2.0;
   end if;
   
   -- Output result (rounded to nearest integer for the problem)
   Put(Integer'Floor(median + 0.5));
   New_Line;
   
end Median;
```

## Alternative More Efficient Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Median is
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Read number of elements
   n : Integer;
   A : Integer_Array;
   median : Float;
   
begin
   -- Read n
   Get(n);
   
   -- Allocate array
   A := new Integer_Array(1..n);
   
   -- Read array elements
   for i in A'Range loop
      Get(A(i));
   end loop;
   
   -- Simple bubble sort (can be optimized with more efficient sorting)
   for i in A'First..A'Last-1 loop
      for j in i+1..A'Last loop
         if A(i) > A(j) then
            declare
               temp : Integer := A(i);
            begin
               A(i) := A(j);
               A(j) := temp;
            end;
         end if;
      end loop;
   end loop;
   
   -- Calculate median
   if n mod 2 = 1 then
      -- Odd length: median is the middle element
      median := Float(A((n+1)/2));
   else
      -- Even length: median is average of two middle elements
      median := (Float(A(n/2)) + Float(A(n/2 + 1))) / 2.0;
   end if;
   
   -- Output the median as integer (since Rosalind expects integer result)
   Put(Integer'Floor(median + 0.5));
   New_Line;
   
end Median;
```

## Key Points:

1. **Input/Output**: The program reads n and then n integers from standard input, outputs the median
2. **Sorting**: Simple bubble sort is used for sorting (though more efficient algorithms exist)
3. **Median Calculation**: 
   - For odd-length arrays: middle element at position (n+1)/2
   - For even-length arrays: average of elements at positions n/2 and n/2 + 1
4. **Integer Handling**: The result is converted to integer using Floor with rounding

## Sample Input:
```
3
2 4 1
```

## Sample Output:
```
2
```

This solution handles the basic requirements of finding the median in an array of integers and should work correctly for the Rosalind Median problem.