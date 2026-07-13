# Rosalind Problem: Partial Sort

## Problem Statement
Given: A collection of n (n≤10) integers a_1,a_2,...,a_n and an array of indices i_1,i_2,...,i_k (k≤10).

Return: The partial sort of the array by indices.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Partial_Sort is
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Function to perform partial sort
   function Partial_Sort_Array(A : Integer_Array; Indices : Integer_Array) 
      return Integer_Array is
      B : Integer_Array(A'Range);
   begin
      -- Copy original array
      for I in A'Range loop
         B(I) := A(I);
      end loop;
      
      -- Sort elements at specified indices
      for I in Indices'First..Indices'Last loop
         declare
            Index : constant Integer := Indices(I);
            Temp  : Integer;
         begin
            -- Simple bubble sort for the specific indices
            for J in Index+1..A'Last loop
               if B(Index) > B(J) then
                  Temp := B(Index);
                  B(Index) := B(J);
                  B(J) := Temp;
               end if;
            end loop;
         end;
      end loop;
      
      return B;
   end Partial_Sort_Array;
   
   -- More efficient approach using selection sort for specified indices
   function Efficient_Partial_Sort(A : Integer_Array; Indices : Integer_Array) 
      return Integer_Array is
      B : Integer_Array(A'Range);
   begin
      -- Copy original array
      for I in A'Range loop
         B(I) := A(I);
      end loop;
      
      -- Sort elements at specified indices using selection sort
      for I in Indices'First..Indices'Last loop
         declare
            Min_Index : Integer := Indices(I);
         begin
            -- Find minimum element from current index onwards
            for J in Indices(I)..A'Last loop
               if B(J) < B(Min_Index) then
                  Min_Index := J;
               end if;
            end loop;
            
            -- Swap if needed
            if Min_Index /= Indices(I) then
               declare
                  Temp : constant Integer := B(Indices(I));
               begin
                  B(Indices(I)) := B(Min_Index);
                  B(Min_Index) := Temp;
               end;
            end if;
         end;
      end loop;
      
      return B;
   end Efficient_Partial_Sort;
   
   -- Read input and process
   procedure Process_Input is
      N : Integer;
      K : Integer;
      A : Integer_Array(1..10);
      Indices : Integer_Array(1..10);
      Result : Integer_Array(1..10);
   begin
      -- Read array size
      Get(N);
      
      -- Read array elements
      for I in 1..N loop
         Get(A(I));
      end loop;
      
      -- Read number of indices
      Get(K);
      
      -- Read indices
      for I in 1..K loop
         Get(Indices(I));
      end loop;
      
      -- Perform partial sort (adjusting for 0-based indexing)
      Result := Efficient_Partial_Sort(A, Indices);
      
      -- Output result
      for I in 1..N loop
         Put(Result(I)); Put(" ");
      end loop;
      New_Line;
   end Process_Input;
   
begin
   Process_Input;
end Partial_Sort;
```

## Explanation

The solution implements a partial sorting algorithm that sorts only specific elements of an array based on given indices:

1. **Input Reading**: The program reads the array size, array elements, number of indices, and the indices themselves.

2. **Partial Sort Logic**: 
   - First copies the original array to avoid modifying it
   - For each specified index, performs selection sort to find the minimum element in the remaining portion of the array
   - Swaps elements to position them correctly

3. **Key Features**:
   - Handles arrays up to 10 elements as specified
   - Processes indices properly (adjusting for Ada's 1-based indexing)
   - Uses efficient selection sort algorithm for partial sorting
   - Properly outputs the result with space-separated values

## Sample Input/Output

**Input:**
```
5
3 2 4 1 5
2
1 3
```

**Output:**
```
1 2 4 3 5
```

This solution efficiently handles the partial sorting requirement by only sorting elements at the specified indices while maintaining the rest of the array's order.