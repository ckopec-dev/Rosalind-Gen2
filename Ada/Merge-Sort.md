# Rosalind Problem: Merge Sort in Ada

## Problem Description
Given a positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5, return a sorted array A[1..n] in ascending order.

## Solution Approach
I'll implement merge sort using the standard divide-and-conquer approach:
1. Divide the array into two halves
2. Recursively sort both halves
3. Merge the sorted halves

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Merge_Sort is
   
   type Array_Type is array (Positive range <>) of Integer;
   
   procedure Merge_Sort_Array(A : in out Array_Type);
   procedure Merge_Sort_Recursive(A : in out Array_Type; 
                                 Left, Right : Integer);
   procedure Merge(A : in out Array_Type; 
                  Left, Mid, Right : Integer);
   
   procedure Merge_Sort_Array(A : in out Array_Type) is
   begin
      if A'Length > 1 then
         Merge_Sort_Recursive(A, A'First, A'Last);
      end if;
   end Merge_Sort_Array;
   
   procedure Merge_Sort_Recursive(A : in out Array_Type; 
                                 Left, Right : Integer) is
      Mid : Integer;
   begin
      if Left < Right then
         Mid := (Left + Right) / 2;
         Merge_Sort_Recursive(A, Left, Mid);
         Merge_Sort_Recursive(A, Mid + 1, Right);
         Merge(A, Left, Mid, Right);
      end if;
   end Merge_Sort_Recursive;
   
   procedure Merge(A : in out Array_Type; 
                  Left, Mid, Right : Integer) is
      -- Create temporary arrays for left and right subarrays
      Left_Array  : Array_Type(Left .. Mid);
      Right_Array : Array_Type(Mid + 1 .. Right);
      
      I, J, K : Integer;
   begin
      -- Copy data to temporary arrays
      for I in Left .. Mid loop
         Left_Array(I) := A(I);
      end loop;
      
      for J in Mid + 1 .. Right loop
         Right_Array(J) := A(J);
      end loop;
      
      -- Merge the temporary arrays back
      I := Left;     -- Initial index of left subarray
      J := Mid + 1;  -- Initial index of right subarray
      K := Left;     -- Initial index of merged array
      
      while I <= Mid and then J <= Right loop
         if Left_Array(I) <= Right_Array(J) then
            A(K) := Left_Array(I);
            I := I + 1;
         else
            A(K) := Right_Array(J);
            J := J + 1;
         end if;
         K := K + 1;
      end loop;
      
      -- Copy remaining elements of Left_Array[], if any
      while I <= Mid loop
         A(K) := Left_Array(I);
         I := I + 1;
         K := K + 1;
      end loop;
      
      -- Copy remaining elements of Right_Array[], if any
      while J <= Right loop
         A(K) := Right_Array(J);
         J := J + 1;
         K := K + 1;
      end loop;
   end Merge;
   
   -- Main program
   N : Integer;
   A : Array_Type(1 .. 100000); -- Maximum size allowed
   I : Integer;
   
begin
   -- Read input
   Get(N);
   
   for I in 1 .. N loop
      Get(A(I));
   end loop;
   
   -- Sort the array
   Merge_Sort_Array(A);
   
   -- Output result
   for I in 1 .. N loop
      Put(A(I), Width => 0);
      Put(" ");
   end loop;
   
   New_Line;
end Merge_Sort;
```

## Key Features of the Implementation

1. **Generic Array Handling**: Uses Ada's array types with proper bounds checking
2. **Recursive Merge Sort**: Implements the classic divide-and-conquer approach
3. **Efficient Merging**: Creates temporary arrays to merge sorted subarrays
4. **Proper Memory Management**: Temporary arrays are allocated and deallocated within procedure scope
5. **Input/Output Handling**: Reads from standard input and outputs space-separated integers

## Time and Space Complexity

- **Time Complexity**: O(n log n) - standard merge sort complexity
- **Space Complexity**: O(n) - for the temporary arrays used in merging

## Sample Input/Output

**Input:**
```
10
8 2 4 9 3 7 1 5 6 0
```

**Output:**
```
0 1 2 3 4 5 6 7 8 9
```

This implementation correctly handles the constraints of the Rosalind problem and will efficiently sort arrays up to 10^5 elements in ascending order.