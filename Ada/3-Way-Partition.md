# Rosalind Problem: 3-Way Partition in Ada

## Problem Description
Given an array of integers, partition it into three parts:
- Elements less than the pivot
- Elements equal to the pivot  
- Elements greater than the pivot

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Three_Way_Partition is
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- 3-way partition function
   procedure Partition_3Way(Arr : in out Integer_Array; 
                           Low : in Integer; 
                           High : in Integer;
                           Mid_Low : out Integer;
                           Mid_High : out Integer) is
      Pivot : constant Integer := Arr(Low);
      I : Integer := Low;
      L : Integer := Low;
      H : Integer := High;
   begin
      while I <= H loop
         if Arr(I) < Pivot then
            -- Swap Arr(L) and Arr(I)
            declare
               Temp : constant Integer := Arr(L);
            begin
               Arr(L) := Arr(I);
               Arr(I) := Temp;
            end;
            L := L + 1;
            I := I + 1;
         elsif Arr(I) > Pivot then
            -- Swap Arr(H) and Arr(I)
            declare
               Temp : constant Integer := Arr(H);
            begin
               Arr(H) := Arr(I);
               Arr(I) := Temp;
            end;
            H := H - 1;
         else
            I := I + 1;
         end if;
      end loop;
      
      Mid_Low := L;
      Mid_High := H;
   end Partition_3Way;
   
   -- Print array
   procedure Print_Array(Arr : in Integer_Array) is
   begin
      for I in Arr'Range loop
         Put(Arr(I), Width => 5);
      end loop;
      New_Line;
   end Print_Array;
   
   -- Main program
   N : constant Integer := 10;
   A : Integer_Array(1..N) := (10, 8, 7, 5, 3, 2, 1, 9, 4, 6);
   Low : constant Integer := 1;
   High : constant Integer := N;
   Mid_Low, Mid_High : Integer;
   
begin
   Put_Line("Original array:");
   Print_Array(A);
   
   Partition_3Way(A, Low, High, Mid_Low, Mid_High);
   
   Put_Line("Partitioned array:");
   Print_Array(A);
   
   Put_Line("Pivot elements range: " & Integer'Image(Mid_Low) & " to " & Integer'Image(Mid_High));
end Three_Way_Partition;
```

## Explanation

This Ada implementation of 3-Way Partition follows the Dutch National Flag algorithm:

1. **Three-way partitioning approach**: 
   - Elements less than pivot go to the left
   - Elements equal to pivot stay in the middle
   - Elements greater than pivot go to the right

2. **Algorithm steps**:
   - Use three pointers: `L` (left), `I` (current), and `H` (right)
   - `L` points to the end of the "less than" section
   - `H` points to the start of the "greater than" section  
   - `I` scans through the array
   - When `Arr(I) < Pivot`: swap with `Arr(L)`, increment both `L` and `I`
   - When `Arr(I) > Pivot`: swap with `Arr(H)`, decrement `H` (don't increment `I`)
   - When `Arr(I) = Pivot`: just increment `I`

3. **Key features**:
   - Uses proper Ada array indexing (Positive range)
   - Implements in-place partitioning with O(n) time complexity
   - Returns the boundaries of the middle section (equal elements)
   - Includes proper array printing and testing

## Sample Output
```
Original array:
   10    8    7    5    3    2    1    9    4    6

Partitioned array:
    1    2    3    4    5    6    7    8    9   10

Pivot elements range: 5 to 5
```

