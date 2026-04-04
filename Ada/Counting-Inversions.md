# Rosalind Problem: Counting Inversions in Ada

## Problem Understanding

An inversion in an array A[1...n] is a pair of indices (i, j) such that i < j and A[i] > A[j]. The task is to count the number of such inversions in the given array.

## Approach

I'll use a modified merge sort algorithm to count inversions efficiently in O(n log n) time complexity. During the merge process, when we merge two sorted subarrays, we can count inversions by:
- When an element from the right subarray is smaller than an element from the left subarray, it forms inversions with all remaining elements in the left subarray

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Counting_Inversions is
   type Array_Type is array (Positive range <>) of Integer;
   
   procedure Merge_Sort (A : in out Array_Type; 
                        temp : in out Array_Type;
                        left : Integer; 
                        right : Integer;
                        inv_count : in out Integer);
   
   procedure Merge (A : in out Array_Type;
                   temp : in out Array_Type;
                   left : Integer;
                   mid : Integer;
                   right : Integer;
                   inv_count : in out Integer);
   
   procedure Merge_Sort (A : in out Array_Type; 
                        temp : in out Array_Type;
                        left : Integer; 
                        right : Integer;
                        inv_count : in out Integer) is
      mid : Integer;
   begin
      if left < right then
         mid := (left + right) / 2;
         
         Merge_Sort(A, temp, left, mid, inv_count);
         Merge_Sort(A, temp, mid + 1, right, inv_count);
         Merge(A, temp, left, mid, right, inv_count);
      end if;
   end Merge_Sort;
   
   procedure Merge (A : in out Array_Type;
                   temp : in out Array_Type;
                   left : Integer;
                   mid : Integer;
                   right : Integer;
                   inv_count : in out Integer) is
      i, j, k : Integer;
      inversions : Integer := 0;
   begin
      -- Copy data to temp array
      for index in left .. right loop
         temp(index) := A(index);
      end loop;
      
      i := left;
      j := mid + 1;
      k := left;
      
      -- Merge the two subarrays while counting inversions
      while i <= mid and j <= right loop
         if temp(i) <= temp(j) then
            A(k) := temp(i);
            i := i + 1;
         else
            A(k) := temp(j);
            -- All elements from i to mid are greater than temp(j)
            -- So we have (mid - i + 1) inversions
            inversions := inversions + (mid - i + 1);
            j := j + 1;
         end if;
         k := k + 1;
      end loop;
      
      -- Copy remaining elements
      while i <= mid loop
         A(k) := temp(i);
         i := i + 1;
         k := k + 1;
      end loop;
      
      while j <= right loop
         A(k) := temp(j);
         j := j + 1;
         k := k + 1;
      end loop;
      
      inv_count := inv_count + inversions;
   end Merge;
   
   procedure Count_Inversions (A : in out Array_Type; 
                              n : Integer;
                              result : out Integer) is
      temp : Array_Type(1 .. n);
      inv_count : Integer := 0;
   begin
      Merge_Sort(A, temp, 1, n, inv_count);
      result := inv_count;
   end Count_Inversions;
   
   -- Main program
   n : Integer;
   a : Array_Type(1 .. 10000);
   result : Integer;
begin
   -- Read input
   Get(n);
   
   for i in 1 .. n loop
      Get(a(i));
   end loop;
   
   -- Count inversions
   Count_Inversions(a, n, result);
   
   -- Output result
   Put(result);
   New_Line;
end Counting_Inversions;
```

## Explanation

1. **Merge Sort Approach**: The solution uses a modified merge sort algorithm where during the merge phase, we count inversions.

2. **Inversion Counting Logic**: 
   - When merging two sorted subarrays, if an element from the right subarray is smaller than an element from the left subarray, it means this element forms inversions with all remaining elements in the left subarray.
   - This happens because both subarrays are sorted, so if `temp(i) > temp(j)`, then `temp(i)` is greater than all elements from `temp(i)` to `temp(mid)`.

3. **Time Complexity**: O(n log n) - same as merge sort
4. **Space Complexity**: O(n) for the temporary array

## Sample Input/Output

**Input:**
```
5
2 3 8 6 1
```

**Output:**
```
5
```

**Explanation**: The inversions are (1,5), (2,5), (3,4), (3,5), (4,5) where the pairs represent indices (i,j) such that A[i] > A[j].

The algorithm correctly identifies that there are 5 inversions in the array [2, 3, 8, 6, 1].

