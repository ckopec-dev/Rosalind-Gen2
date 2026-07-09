# Rosalind Problem: Building a Heap (in Ada)

## Problem Understanding

The task is to build a max heap from an array of integers. A max heap is a complete binary tree where each parent node is greater than or equal to its children.

## Solution Approach

1. Start with the given array
2. Apply the heapify operation from the last non-leaf node up to the root
3. Use the sift-down method to maintain heap property

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Building_A_Heap is
   type Array_Type is array (Positive range <>) of Integer;
   
   procedure Sift_Down(A : in out Array_Type; i, n : Integer) is
      largest : Integer := i;
      left    : Integer := 2 * i;
      right   : Integer := 2 * i + 1;
   begin
      -- Check if left child exists and is greater than root
      if left <= n and then A(left) > A(largest) then
         largest := left;
      end if;
      
      -- Check if right child exists and is greater than largest so far
      if right <= n and then A(right) > A(largest) then
         largest := right;
      end if;
      
      -- If largest is not root, swap and continue sifting down
      if largest /= i then
         A(i) := A(i) + A(largest);
         A(largest) := A(i) - A(largest);
         A(i) := A(i) - A(largest);
         Sift_Down(A, largest, n);
      end if;
   end Sift_Down;
   
   procedure Build_Heap(A : in out Array_Type; n : Integer) is
   begin
      -- Start from the last non-leaf node and heapify each node
      for i in reverse 1 .. n/2 loop
         Sift_Down(A, i, n);
      end loop;
   end Build_Heap;
   
   procedure Print_Array(A : Array_Type) is
   begin
      for i in A'First .. A'Last loop
         Put(A(i));
         if i < A'Last then
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Print_Array;
   
   -- Read input
   N : Integer;
   A : Array_Type(1 .. 1000); -- Assuming maximum size
   I : Integer := 1;
   
begin
   -- Read the array size
   Get(N);
   
   -- Read the array elements
   for i in 1 .. N loop
      Get(A(i));
   end loop;
   
   -- Build the heap
   Build_Heap(A, N);
   
   -- Print the result
   Print_Array(A);
end Building_A_Heap;
```

## Key Components Explained

### Sift_Down Procedure
- Implements the heapify operation to maintain max heap property
- Compares a node with its children and swaps if necessary
- Recursively sifts down until heap property is restored

### Build_Heap Procedure  
- Starts from the last non-leaf node (at index N/2)
- Applies sift_down to all nodes from bottom up
- This ensures that the entire array becomes a valid max heap

### Array Indexing
- Uses 1-based indexing for simplicity (as in the problem description)
- Properly handles boundary conditions for children

## Example Usage

**Input:**
```
5
1 3 5 7 2
```

**Output:**
```
7 3 5 1 2
```

The algorithm correctly transforms the input array into a max heap where each parent node is greater than or equal to its children.