# Rosalind Problem: Enumerating Oriented Gene Orderings (PERM)

## Problem Description
Given a positive integer n ≤ 6, return the total number of signed permutations of length n, followed by a list of all such permutations.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Enumerating_Oriented_Gene_Orderings is
   
   type Permutation is array (Positive range <>) of Integer;
   package Perm_Vector is new Ada.Containers.Indefinite_Vectors (Positive, Permutation);
   use Perm_Vector;
   
   procedure Generate_Permutations (N : Integer; Current : Permutation; Index : Positive; 
                                   Results : in out Vector; Used : in out array (1..2*N) of Boolean);
   
   procedure Print_Permutation (P : Permutation);
   
   N : Integer;
   Results : Vector;
   Used : array (1..12) of Boolean := (others => False);
   
begin
   -- Read input
   Put_Line("Enter the value of n (1-6): ");
   Get(N);
   
   -- Validate input
   if N < 1 or N > 6 then
      Put_Line("Error: n must be between 1 and 6");
      return;
   end if;
   
   -- Generate all signed permutations
   declare
      Current : Permutation(1..2*N);
   begin
      Generate_Permutations(N, Current, 1, Results, Used);
   end;
   
   -- Output results
   Put_Line(Integer'Image(Results.Length));
   
   for I in 1..Results.Length loop
      Print_Permutation(Results.Element(I));
   end loop;
   
end Enumerating_Oriented_Gene_Orderings;

procedure Generate_Permutations (N : Integer; Current : Permutation; Index : Positive; 
                                Results : in out Vector; Used : in out array (1..2*N) of Boolean) is
   procedure Add_Permutation (P : Permutation);
   
   begin
      if Index > 2*N then
         -- Add the permutation to results
         Append(Results, Current);
         return;
      end if;
      
      -- Try each possible value (positive and negative)
      for I in 1..2*N loop
         if not Used(I) then
            Used(I) := True;
            Current(Index) := (if I <= N then I else -(I-N));
            Generate_Permutations(N, Current, Index + 1, Results, Used);
            Used(I) := False;
         end if;
      end loop;
   end Generate_Permutations;

procedure Print_Permutation (P : Permutation) is
begin
   for I in P'First..P'Last loop
      Put(Integer'Image(P(I)));
      if I < P'Last then
         Put(" ");
      end if;
   end loop;
   New_Line;
end Print_Permutation;
```

## Explanation

This solution uses a recursive backtracking approach to generate all signed permutations:

1. **Input Handling**: Reads the integer n (1-6) from standard input
2. **Permutation Generation**: 
   - For each position, try both positive and negative values
   - Use a boolean array to track which numbers have been used
   - Recursive backtracking to build all valid permutations
3. **Output**: 
   - First line: total count of permutations
   - Following lines: all permutations, one per line

## Key Concepts

- **Signed Permutations**: Each number can appear in either positive or negative form
- **Backtracking**: Recursive approach that builds permutations incrementally
- **State Tracking**: Boolean array to prevent reuse of numbers in current permutation

## Example Output for n=2:
```
8
1 2
1 -2
-1 2
-1 -2
2 1
2 -1
-2 1
-2 -1
```

## Time and Space Complexity
- **Time Complexity**: O(2^n × n!) - exponential due to signed permutations
- **Space Complexity**: O(2^n × n!) - for storing all permutations

The solution handles the constraint that n ≤ 6 efficiently, as the number of signed permutations grows rapidly (2^n × n!) but remains manageable within reasonable bounds.

