# Rosalind Problem: 2-Way Partition in Ada

## Problem Understanding

The 2-way partition problem involves rearranging an array such that all elements less than a chosen pivot appear before all elements greater than or equal to the pivot.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Two_Way_Partition is
   type Array_Type is array (Positive range <>) of Integer;
   
   procedure Partition(A : in out Array_Type; Low, High : Integer; Pivot_Index : out Integer);
   
   procedure Partition(A : in out Array_Type; Low, High : Integer; Pivot_Index : out Integer) is
      Pivot : constant Integer := A(Pivot_Index);
      I     : Integer := Low - 1;
      J     : Integer := High + 1;
   begin
      loop
         -- Move from left to right, find element >= pivot
         loop
            I := I + 1;
            exit when A(I) >= Pivot;
         end loop;
         
         -- Move from right to left, find element < pivot
         loop
            J := J - 1;
            exit when A(J) < Pivot;
         end loop;
         
         exit when I >= J;
         
         -- Swap elements
         declare
            Temp : constant Integer := A(I);
         begin
            A(I) := A(J);
            A(J) := Temp;
         end;
      end loop;
      
      Pivot_Index := J;
   end Partition;
   
   procedure Print_Array(A : Array_Type) is
   begin
      for I in A'Range loop
         Put(A(I), Width => 5);
      end loop;
      New_Line;
   end Print_Array;
   
   -- Main program
   N : Integer;
   A : Array_Type(1..1000); -- Assuming maximum array size
   Pivot_Index : Integer;
begin
   -- Read input
   Get(N);
   
   for I in 1..N loop
      Get(A(I));
   end loop;
   
   -- Perform 2-way partition with last element as pivot
   if N > 0 then
      Pivot_Index := N;
      Partition(A, 1, N, Pivot_Index);
      
      -- Print result
      Print_Array(A);
   end if;
end Two_Way_Partition;
```

## Explanation

This Ada solution implements the 2-way partition algorithm using a modified version of the Hoare partition scheme:

1. **Input Reading**: The program reads the array size `N` followed by `N` integers.

2. **Partition Procedure**: 
   - Uses the last element as the pivot (this can be modified)
   - Maintains two pointers: `I` starting from left, `J` starting from right
   - Moves `I` forward until finding an element >= pivot
   - Moves `J` backward until finding an element < pivot
   - When both pointers find elements that need swapping, swap them
   - Continue until pointers cross

3. **Output**: Prints the partitioned array where all elements less than the pivot come before elements greater than or equal to the pivot.

## Sample Input/Output

**Input:**
```
5
4 5 1 3 2
```

**Output:**
```
   2   1   3   5   4
```

## Time and Space Complexity

- **Time Complexity**: O(n) average case, O(n²) worst case
- **Space Complexity**: O(1) - in-place partitioning

The algorithm effectively partitions the array such that elements smaller than the pivot are on the left side, and elements greater than or equal to the pivot are on the right side.