# Rosalind Problem: Merge Two Sorted Arrays (Ada Solution)

## Problem Description
Given two sorted arrays A and B of length n each, merge them into a single sorted array.

## Solution

```ada
with Ada.Text_IO;
procedure Merge_Two_Sorted_Arrays is
   type Array_Type is array (Positive range <>) of Integer;
   
   -- Function to merge two sorted arrays
   function Merge(A, B : Array_Type) return Array_Type is
      C : Array_Type(1 .. A'Length + B'Length);
      I, J, K : Positive := 1;
   begin
      -- Merge elements while both arrays have remaining elements
      while I <= A'Length and then J <= B'Length loop
         if A(I) <= B(J) then
            C(K) := A(I);
            I := I + 1;
         else
            C(K) := B(J);
            J := J + 1;
         end if;
         K := K + 1;
      end loop;
      
      -- Copy remaining elements from array A (if any)
      while I <= A'Length loop
         C(K) := A(I);
         I := I + 1;
         K := K + 1;
      end loop;
      
      -- Copy remaining elements from array B (if any)
      while J <= B'Length loop
         C(K) := B(J);
         J := J + 1;
         K := K + 1;
      end loop;
      
      return C;
   end Merge;
   
   -- Input arrays
   A : constant Array_Type := (1, 3, 5, 7, 9);
   B : constant Array_Type := (2, 4, 6, 8, 10);
   
   -- Result array
   C : Array_Type;
   
begin
   -- Merge the arrays
   C := Merge(A, B);
   
   -- Output the result
   for I in C'Range loop
      Ada.Text_IO.Put(Integer'image(C(I)));
      if I < C'Length then
         Ada.Text_IO.Put(" ");
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end Merge_Two_Sorted_Arrays;
```

## Explanation

This Ada solution implements the classic merge algorithm for two sorted arrays:

1. **Data Types**: 
   - Uses `Array_Type` to define a generic array of integers
   - Arrays are indexed starting from 1 (Ada default)

2. **Merge Function**:
   - Takes two sorted input arrays A and B
   - Creates result array C with combined size
   - Uses three pointers: I for A, J for B, and K for C
   - Compares elements and places the smaller one in C
   - Handles remaining elements from either array after comparison

3. **Algorithm Steps**:
   - Compare current elements of both arrays
   - Place the smaller element in result array
   - Advance the corresponding pointer
   - Continue until one array is exhausted
   - Copy remaining elements from the other array

4. **Output**: 
   - Prints merged sorted array elements separated by spaces

## Time and Space Complexity
- **Time Complexity**: O(n + m) where n and m are the lengths of arrays A and B
- **Space Complexity**: O(n + m) for the result array

## Sample Output
```
1 2 3 4 5 6 7 8 9 10
```