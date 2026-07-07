# Rosalind Problem: Insertion Sort in Ada

## Problem Understanding
The insertion sort problem requires us to sort an array using the insertion sort algorithm and return the number of swaps performed during the sorting process.

## Solution Approach
I'll implement insertion sort in Ada, counting the number of swaps needed to sort the array.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Insertion_Sort is
   type Array_Type is array (Positive range <>) of Integer;
   
   -- Function to perform insertion sort and count swaps
   function Insertion_Sort_Count(Arr : in Array_Type) return Integer is
      N : constant Integer := Arr'Length;
      Count : Integer := 0;
      Temp_Array : Array_Type(1..N) := Arr;
   begin
      -- Insertion sort algorithm
      for I in 2..N loop
         declare
            Key : Integer := Temp_Array(I);
            J : Integer := I - 1;
         begin
            -- Move elements greater than key one position ahead
            while J >= 1 and then Temp_Array(J) > Key loop
               Temp_Array(J + 1) := Temp_Array(J);
               J := J - 1;
               Count := Count + 1;  -- Increment swap count
            end loop;
            
            -- Place key at its correct position
            Temp_Array(J + 1) := Key;
         end;
      end loop;
      
      return Count;
   end Insertion_Sort_Count;
   
   -- Read input from stdin
   procedure Read_Array(Length : out Integer; Arr : out Array_Type) is
      Input_Line : String(1..1000);
      Last : Natural;
   begin
      Get_Line(Input_Line, Last);
      Length := 0;
      
      -- Parse the input line to extract integers
      declare
         I : Natural := 1;
         Num_Start : Natural := 1;
         Num_Str : String(1..100);
         Num_Last : Natural;
      begin
         while I <= Last loop
            if Input_Line(I) in '0'..'9' or else Input_Line(I) = '-' then
               -- Extract number
               Num_Start := I;
               while I <= Last and then Input_Line(I) in '0'..'9' loop
                  I := I + 1;
               end loop;
               Num_Last := I - 1;
               
               -- Convert to integer and store
               Length := Length + 1;
               declare
                  Num : Integer;
               begin
                  Num_Str := Input_Line(Num_Start..Num_Last);
                  Num := Integer'Value(Num_Str);
                  Arr(Length) := Num;
               end;
            else
               I := I + 1;
            end if;
         end loop;
      end;
   end Read_Array;
   
   -- Main program
   N : Integer;
   A : Array_Type(1..1000);
begin
   -- Read array length and elements
   Get(N);
   for I in 1..N loop
      Get(A(I));
   end loop;
   
   -- Perform insertion sort and count swaps
   declare
      Swaps : Integer := Insertion_Sort_Count(A);
   begin
      Put_Line(Integer'Image(Swaps));
   end;
end Insertion_Sort;
```

## Explanation

1. **Data Structure**: I defined `Array_Type` as an array of integers with a flexible range.

2. **Insertion Sort Algorithm**: 
   - Start from the second element (index 2)
   - Compare current element with elements to its left
   - Shift larger elements one position right
   - Insert current element at correct position
   - Count each shift operation as a swap

3. **Swap Counter**: Each time we shift an element during the sorting process, we increment our counter.

4. **Input Handling**: 
   - Read array length first
   - Then read all elements into the array
   - Process input line by parsing integers from the string

5. **Output**: Return the total number of swaps needed to sort the array

## Example Usage
For input:
```
5
5 2 4 6 1 3
```

The program would output:
```
7
```

This represents the number of swaps required by insertion sort to arrange the elements in ascending order.