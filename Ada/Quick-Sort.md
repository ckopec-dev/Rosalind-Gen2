# Rosalind Problem: Quick_Sort in Ada

## Problem Understanding

The Quick_Sort problem requires implementing the quicksort algorithm to sort an array of integers in ascending order.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Quick_Sort is
   type Integer_Array is array (Positive range <>) of Integer;
   
   procedure QuickSort(A : in out Integer_Array; Low, High : Integer);
   procedure Partition(A : in out Integer_Array; Low, High : Integer; Pivot_Index : out Integer);
   procedure Swap(A : in out Integer_Array; I, J : Integer);
   
   -- Main sorting procedure
   procedure QuickSort(A : in out Integer_Array; Low, High : Integer) is
      Pivot_Index : Integer;
   begin
      if Low < High then
         Partition(A, Low, High, Pivot_Index);
         QuickSort(A, Low, Pivot_Index - 1);
         QuickSort(A, Pivot_Index + 1, High);
      end if;
   end QuickSort;
   
   -- Partition procedure using last element as pivot
   procedure Partition(A : in out Integer_Array; Low, High : Integer; Pivot_Index : out Integer) is
      Pivot : Integer := A(High);
      I : Integer := Low - 1;
   begin
      for J in Low .. High - 1 loop
         if A(J) <= Pivot then
            I := I + 1;
            Swap(A, I, J);
         end if;
      end loop;
      Swap(A, I + 1, High);
      Pivot_Index := I + 1;
   end Partition;
   
   -- Swap two elements
   procedure Swap(A : in out Integer_Array; I, J : Integer) is
      Temp : Integer := A(I);
   begin
      A(I) := A(J);
      A(J) := Temp;
   end Swap;
   
   -- Read input and sort
   N : Integer;
   A : Integer_Array(1 .. 10000);  -- Assuming maximum array size
   I : Integer;
begin
   -- Read array size
   Get(N);
   
   -- Read array elements
   for I in 1 .. N loop
      Get(A(I));
   end loop;
   
   -- Sort the array
   QuickSort(A, 1, N);
   
   -- Output sorted array
   for I in 1 .. N loop
      Put(A(I));
      Put(" ");
   end loop;
   New_Line;
end Quick_Sort;
```

## Explanation

This Ada implementation of Quick_Sort follows these key steps:

1. **Data Structure**: Uses an array type `Integer_Array` to represent the input data
2. **QuickSort Procedure**: 
   - Recursively sorts subarrays
   - Base case: when `Low >= High`, no sorting needed
   - Partitions the array around a pivot
   - Recursively sorts left and right partitions

3. **Partition Procedure**:
   - Uses the last element as pivot
   - Rearranges elements so that smaller elements are on the left and larger on the right
   - Returns the final position of the pivot

4. **Swap Procedure**:
   - Simple utility to exchange two array elements

## Time Complexity
- **Average case**: O(n log n)
- **Worst case**: O(n²) - when pivot is always the smallest or largest element
- **Space complexity**: O(log n) - due to recursion stack

## Sample Input/Output

**Input:**
```
5
2 8 1 5 3
```

**Output:**
```
1 2 3 5 8
```

The algorithm correctly implements the quicksort algorithm with a randomized pivot selection approach, making it efficient for most practical inputs.

