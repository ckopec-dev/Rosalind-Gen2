# Rosalind Problem: Heap Sort in Ada

## Problem Understanding

The Heap Sort problem requires implementing a heap sort algorithm that takes an array of integers and returns it sorted in ascending order using the heap sort technique.

## Solution Approach

I'll implement heap sort using:
1. Build a max heap from the input array
2. Repeatedly extract the maximum element and rebuild the heap
3. Return the sorted array

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Heap_Sort is
   
   -- Maximum size of array (based on typical Rosalind constraints)
   MAX_SIZE : constant := 10000;
   
   type Array_Type is array (Positive range <>) of Integer;
   
   -- Function to maintain max heap property
   procedure Heapify(A : in out Array_Type; n, i : Integer) is
      largest : Integer := i;
      left    : Integer := 2 * i;
      right   : Integer := 2 * i + 1;
   begin
      -- If left child exists and is greater than root
      if left <= n and then A(left) > A(largest) then
         largest := left;
      end if;
      
      -- If right child exists and is greater than largest so far
      if right <= n and then A(right) > A(largest) then
         largest := right;
      end if;
      
      -- If largest is not root
      if largest /= i then
         -- Swap
         declare
            temp : Integer := A(i);
         begin
            A(i) := A(largest);
            A(largest) := temp;
         end;
         
         -- Recursively heapify the affected sub-tree
         Heapify(A, n, largest);
      end if;
   end Heapify;
   
   -- Main heap sort function
   procedure Heap_Sort_Array(A : in out Array_Type; n : Integer) is
   begin
      -- Build max heap
      for i in reverse 1 .. n/2 loop
         Heapify(A, n, i);
      end loop;
      
      -- Extract elements from heap one by one
      for i in reverse 2 .. n loop
         -- Move current root to end
         declare
            temp : Integer := A(1);
         begin
            A(1) := A(i);
            A(i) := temp;
         end;
         
         -- Call heapify on the reduced heap
         Heapify(A, i - 1, 1);
      end loop;
   end Heap_Sort_Array;
   
   -- Read array from input
   procedure Read_Array(A : in out Array_Type; n : out Integer) is
      line : String(1..1000);
      pos  : Natural := 1;
      num  : Integer;
      found : Boolean;
   begin
      Get_Line(line);
      n := 0;
      
      -- Parse numbers from string
      while pos <= line'Last and then line(pos) /= ' ' loop
         if line(pos) = '-' or else (line(pos) >= '0' and then line(pos) <= '9') then
            num := 0;
            found := False;
            
            while pos <= line'Last and then (line(pos) = '-' or else (line(pos) >= '0' and then line(pos) <= '9')) loop
               if line(pos) = '-' then
                  num := -num;
               else
                  num := num * 10 + Integer'Value(line(pos..pos));
               end if;
               pos := pos + 1;
               found := True;
            end loop;
            
            if found then
               n := n + 1;
               A(n) := num;
            end if;
         else
            pos := pos + 1;
         end if;
      end loop;
   end Read_Array;
   
   -- Print array
   procedure Print_Array(A : Array_Type; n : Integer) is
   begin
      for i in 1 .. n loop
         Put(A(i), Width => 0);
         if i < n then
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Print_Array;
   
   -- Main program
   A : Array_Type(1..MAX_SIZE);
   n : Integer;
   
begin
   -- Read input array size (first line)
   Get(n);
   
   -- Read the array elements
   Read_Array(A, n);
   
   -- Perform heap sort
   Heap_Sort_Array(A, n);
   
   -- Print result
   Print_Array(A, n);
   
end Heap_Sort;
```

## Alternative Simpler Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Heap_Sort is
   
   MAX_SIZE : constant := 10000;
   type Int_Array is array (Positive range <>) of Integer;
   
   -- Heapify function to maintain max heap property
   procedure Heapify(A : in out Int_Array; n, i : Integer) is
      largest : Integer := i;
      left    : Integer := 2 * i;
      right   : Integer := 2 * i + 1;
   begin
      if left <= n and then A(left) > A(largest) then
         largest := left;
      end if;
      
      if right <= n and then A(right) > A(largest) then
         largest := right;
      end if;
      
      if largest /= i then
         -- Swap elements
         declare
            temp : Integer := A(i);
         begin
            A(i) := A(largest);
            A(largest) := temp;
         end;
         Heapify(A, n, largest);
      end if;
   end Heapify;
   
   -- Heap sort implementation
   procedure Heap_Sort_Function(A : in out Int_Array; size : Integer) is
      i : Integer;
   begin
      -- Build max heap
      for i in reverse 1 .. size/2 loop
         Heapify(A, size, i);
      end loop;
      
      -- Extract elements from heap one by one
      for i in reverse 2 .. size loop
         -- Move current root to end
         declare
            temp : Integer := A(1);
         begin
            A(1) := A(i);
            A(i) := temp;
         end;
         
         -- Call heapify on the reduced heap
         Heapify(A, i - 1, 1);
      end loop;
   end Heap_Sort_Function;
   
   -- Main program
   A : Int_Array(1..MAX_SIZE);
   n : Integer;
   
begin
   Get(n);
   for i in 1 .. n loop
      Get(A(i));
   end loop;
   
   Heap_Sort_Function(A, n);
   
   for i in 1 .. n loop
      Put(A(i)); Put(" ");
   end loop;
   New_Line;
   
end Heap_Sort;
```

## Explanation of Key Components

1. **Heapify Procedure**: Maintains the max heap property by comparing a node with its children and swapping if necessary.

2. **Build Max Heap**: Converts the input array into a max heap by calling heapify on all non-leaf nodes starting from the last parent.

3. **Extract Elements**: Repeatedly removes the maximum element (root) and rebuilds the heap for the remaining elements.

4. **Time Complexity**: O(n log n)
5. **Space Complexity**: O(1) - sorts in place

The algorithm correctly implements the heap sort technique as required by Rosalind's Heap_Sort problem.