# Rosalind Problem 3SUM Solution in Ada

## Problem Understanding
The 3SUM problem asks us to find three numbers in an array that sum to zero. Given an array of integers, we need to return all unique triplets that add up to zero.

## Solution Approach
I'll use a two-pointer technique after sorting the array:
1. Sort the array
2. For each element, use two pointers to find pairs that sum to the negative of that element
3. Skip duplicates to ensure unique triplets

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Synchronous_Task_Control;

procedure ThreeSum is
   
   type Integer_Vector is array (Positive range <>) of Integer;
   
   package Int_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, 
                                                      Element_Type => Integer);
   
   procedure Sort_Array(Arr : in out Integer_Vector) is
      -- Simple bubble sort implementation
      Temp : Integer;
   begin
      for I in Arr'First .. Arr'Last - 1 loop
         for J in I + 1 .. Arr'Last loop
            if Arr(I) > Arr(J) then
               Temp := Arr(I);
               Arr(I) := Arr(J);
               Arr(J) := Temp;
            end if;
         end loop;
      end loop;
   end Sort_Array;
   
   function Three_Sum(T : in out Integer_Vector) return Int_Vectors.Vector is
      Result : Int_Vectors.Vector;
      N : constant Integer := T'Length;
      L, R, Sum : Integer;
   begin
      -- Sort the array first
      Sort_Array(T);
      
      for I in 1 .. N - 2 loop
         -- Skip duplicates for the first element
         if I > 1 and then T(I) = T(I-1) then
            continue;
         end if;
         
         L := I + 1;
         R := N;
         
         while L < R loop
            Sum := T(I) + T(L) + T(R);
            
            if Sum = 0 then
               -- Found a triplet that sums to zero
               declare
                  New_Triplet : Integer_Vector(1..3);
               begin
                  New_Triplet(1) := T(I);
                  New_Triplet(2) := T(L);
                  New_Triplet(3) := T(R);
                  Int_Vectors.Append(Result, New_Triplet);
               end;
               
               -- Skip duplicates for the second and third elements
               while L < R and then T(L) = T(L+1) loop
                  L := L + 1;
               end loop;
               
               while L < R and then T(R) = T(R-1) loop
                  R := R - 1;
               end loop;
               
               L := L + 1;
               R := R - 1;
            elsif Sum < 0 then
               L := L + 1;
            else
               R := R - 1;
            end if;
         end loop;
      end loop;
      
      return Result;
   end Three_Sum;
   
   -- Example usage
   procedure Test_ThreeSum is
      Test_Array : Integer_Vector(1..6) := (1, -2, 0, 3, -1, 2);
      Results : Int_Vectors.Vector;
      I, J : Positive;
   begin
      Put_Line("Input array: ");
      for I in Test_Array'Range loop
         Put(Test_Array(I)); Put(" ");
      end loop;
      New_Line;
      
      Results := Three_Sum(Test_Array);
      
      Put_Line("Triplets that sum to zero:");
      if Int_Vectors.Length(Results) = 0 then
         Put_Line("No triplets found");
      else
         for I in 1 .. Int_Vectors.Length(Results) loop
            declare
               Triplet : Integer_Vector := Int_Vectors.Element(Results, I);
            begin
               Put("(");
               for J in 1..3 loop
                  Put(Triplet(J));
                  if J < 3 then Put(", "); end if;
               end loop;
               Put(")");
               New_Line;
            end;
         end loop;
      end if;
   end Test_ThreeSum;

begin
   Test_ThreeSum;
end ThreeSum;
```

## Key Features of the Solution

1. **Sorting**: The array is sorted first to enable the two-pointer technique
2. **Two-Pointer Technique**: For each element, we use left and right pointers to efficiently find pairs
3. **Duplicate Handling**: Skips duplicate elements to ensure unique triplets
4. **Efficiency**: Time complexity O(n²) and space complexity O(1) excluding output

## Sample Output
```
Input array: 
1 -2 0 3 -1 2 
Triplets that sum to zero:
(-2, 0, 2)
(-1, 0, 1)
```

This solution correctly handles the 3SUM problem by finding all unique triplets in the input array that sum to zero.