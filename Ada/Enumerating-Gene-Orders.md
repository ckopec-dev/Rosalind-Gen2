# Rosalind Problem: Enumerating Gene Orders (Ada Solution)

## Problem Understanding

The problem asks us to find all possible permutations of a given set of genes (represented as integers) and count them.

## Solution Approach

1. Generate all permutations of the given numbers
2. Count the total number of permutations
3. Output each permutation

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;

procedure Enumerating_Gene_Orders is
   
   type Integer_Vector is array (Positive range <>) of Integer;
   package Vector is new Ada.Containers.Vectors (Positive, Integer);
   
   procedure Permute (Numbers : in out Integer_Vector; 
                      Current : in out Integer_Vector; 
                      Used : in out Vector.Vector;
                      Results : in out Vector.Vector;
                      Index : in out Positive);
   
   procedure Permute (Numbers : in out Integer_Vector; 
                      Current : in out Integer_Vector; 
                      Used : in out Vector.Vector;
                      Results : in out Vector.Vector;
                      Index : in out Positive) is
   begin
      if Index > Numbers.Length then
         -- Add current permutation to results
         declare
            Result : Integer_Vector (1..Numbers.Length);
         begin
            for I in 1..Numbers.Length loop
               Result(I) := Current(I);
            end loop;
            Results.Append (Result);
         end;
      else
         for I in 1..Numbers.Length loop
            if not Used.Element (I) then
               Used.Replace_Element (I, True);
               Current(Index) := Numbers(I);
               Permute (Numbers, Current, Used, Results, Index + 1);
               Used.Replace_Element (I, False);
            end if;
         end loop;
      end if;
   end Permute;
   
   procedure Print_Vector (V : in Integer_Vector) is
   begin
      for I in V'First..V'Last loop
         Put (V(I), Width => 3);
      end loop;
      New_Line;
   end Print_Vector;
   
   N : Integer;
   Numbers : Integer_Vector (1..10);  -- Assuming maximum 10 genes
   Current : Integer_Vector (1..10);
   Used : Vector.Vector;
   Results : Vector.Vector;
   Index : Positive := 1;
   
begin
   -- Read input
   Get (N);
   
   -- Initialize numbers
   for I in 1..N loop
      Numbers(I) := I;
   end loop;
   
   -- Initialize used vector
   Used.Clear;
   for I in 1..N loop
      Used.Append (False);
   end loop;
   
   -- Generate all permutations
   Permute (Numbers, Current, Used, Results, Index);
   
   -- Output number of permutations
   Put (Results.Length);
   New_Line;
   
   -- Output all permutations
   for I in 1..Results.Length loop
      Print_Vector (Results.Element (I));
   end loop;
   
end Enumerating_Gene_Orders;
```

## Alternative Simpler Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Enumerating_Gene_Orders is
   
   procedure Print_Permutation (Array : in array (1..10) of Integer; 
                                Length : in Integer);
   procedure Generate_Permutations (Array : in out array (1..10) of Integer; 
                                    Position : in Integer; 
                                    Length : in Integer);
   
   procedure Print_Permutation (Array : in array (1..10) of Integer; 
                                Length : in Integer) is
   begin
      for I in 1..Length loop
         Put (Array(I), Width => 3);
      end loop;
      New_Line;
   end Print_Permutation;
   
   procedure Generate_Permutations (Array : in out array (1..10) of Integer; 
                                    Position : in Integer; 
                                    Length : in Integer) is
      Temp : Integer;
      Count : Integer := 0;
   begin
      if Position = Length + 1 then
         Print_Permutation (Array, Length);
         Count := Count + 1;
      else
         for I in Position..Length loop
            -- Swap elements
            Temp := Array(Position);
            Array(Position) := Array(I);
            Array(I) := Temp;
            
            -- Recursively generate permutations
            Generate_Permutations (Array, Position + 1, Length);
            
            -- Backtrack
            Temp := Array(Position);
            Array(Position) := Array(I);
            Array(I) := Temp;
         end loop;
      end if;
   end Generate_Permutations;
   
   N : Integer;
   Numbers : array (1..10) of Integer;
   
begin
   Get (N);
   
   -- Initialize array with numbers 1 to N
   for I in 1..N loop
      Numbers(I) := I;
   end loop;
   
   -- Calculate and print number of permutations
   declare
      Factorial : Integer := 1;
   begin
      for I in 1..N loop
         Factorial := Factorial * I;
      end loop;
      Put (Factorial);
      New_Line;
   end;
   
   -- Generate and print all permutations
   Generate_Permutations (Numbers, 1, N);
   
end Enumerating_Gene_Orders;
```

## Key Points

1. **Input**: A single integer N representing the number of genes (1 to N)
2. **Output**: 
   - First line: Total number of permutations (N!)
   - Subsequent lines: All permutations, one per line
3. **Algorithm**: Recursive backtracking to generate all permutations
4. **Time Complexity**: O(N! × N) for generating all permutations
5. **Space Complexity**: O(N! × N) for storing all permutations

## Sample Input/Output

**Input:**
```
3
```

**Output:**
```
6
  1   2   3
  1   3   2
  2   1   3
  2   3   1
  3   1   2
  3   2   1
```

This solution correctly generates all permutations of the numbers 1 through N and outputs them in the required format.

