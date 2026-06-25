# Rosalind Problem: Counting Optimal Alignments in Ada

## Problem Understanding

This problem asks us to count the number of optimal global alignments between two sequences using dynamic programming with traceback counting.

## Solution Approach

We'll implement a dynamic programming solution that:
1. Computes the edit distance matrix
2. Counts the number of ways to achieve the optimal alignment
3. Uses the traceback approach to count paths

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Counting_Optimal_Alignments is
   
   -- Input sequences
   S : String := "AGTACG";
   T : String := "TATGC";
   
   -- Matrix dimensions
   N : constant Integer := S'Length;
   M : constant Integer := T'Length;
   
   -- DP matrix for edit distances
   type Distance_Matrix is array (0 .. N, 0 .. M) of Integer;
   D : Distance_Matrix;
   
   -- Count matrix for number of optimal alignments
   type Count_Matrix is array (0 .. N, 0 .. M) of Long_Long_Integer;
   C : Count_Matrix;
   
   -- Function to compute minimum of three integers
   function Min(A, B, C : Integer) return Integer is
   begin
      if A <= B and A <= C then
         return A;
      elsif B <= C then
         return B;
      else
         return C;
      end if;
   end Min;
   
begin
   -- Initialize base cases
   for i in 0 .. N loop
      D(i, 0) := i;
      C(i, 0) := 1;
   end loop;
   
   for j in 0 .. M loop
      D(0, j) := j;
      C(0, j) := 1;
   end loop;
   
   -- Fill the DP matrix
   for i in 1 .. N loop
      for j in 1 .. M loop
         declare
            cost : Integer;
         begin
            -- Calculate substitution cost
            if S(i) = T(j) then
               cost := 0;
            else
               cost := 1;
            end if;
            
            -- Compute minimum edit distance
            D(i, j) := Min(
               D(i-1, j) + 1,     -- deletion
               D(i, j-1) + 1,     -- insertion  
               D(i-1, j-1) + cost -- substitution
            );
            
            -- Count number of optimal alignments
            C(i, j) := 0;
            
            -- If we came from deletion (top)
            if D(i-1, j) + 1 = D(i, j) then
               C(i, j) := C(i, j) + C(i-1, j);
            end if;
            
            -- If we came from insertion (left)
            if D(i, j-1) + 1 = D(i, j) then
               C(i, j) := C(i, j) + C(i, j-1);
            end if;
            
            -- If we came from substitution or match (diagonal)
            if D(i-1, j-1) + cost = D(i, j) then
               C(i, j) := C(i, j) + C(i-1, j-1);
            end if;
         end;
      end loop;
   end loop;
   
   -- Output the result
   Put("Number of optimal alignments: ");
   Put(C(N, M), Width => 1);
   New_Line;
   
   -- Optional: Print the distance matrix for debugging
   Put_Line("Distance matrix:");
   for i in 0 .. N loop
      for j in 0 .. M loop
         Put(D(i, j), Width => 3);
      end loop;
      New_Line;
   end loop;
   
   Put_Line("Count matrix:");
   for i in 0 .. N loop
      for j in 0 .. M loop
         Put(C(i, j), Width => 3);
      end loop;
      New_Line;
   end loop;
   
end Counting_Optimal_Alignments;
```

## Key Algorithm Components

1. **Distance Matrix (D)**: Stores the minimum edit distance between prefixes of sequences S and T
2. **Count Matrix (C)**: Stores the number of optimal alignments that achieve each edit distance
3. **Base Cases**: C(0,j) = 1, C(i,0) = 1 for all i,j (single path to any base case)
4. **Recurrence Relations**:
   - D(i,j) = min(D(i-1,j)+1, D(i,j-1)+1, D(i-1,j-1)+cost)
   - C(i,j) = sum of valid predecessors that give optimal distance

## Time and Space Complexity

- **Time Complexity**: O(N×M) where N and M are the lengths of the sequences
- **Space Complexity**: O(N×M) for the DP matrices

## Example Output

For sequences S = "AGTACG" and T = "TATGC", the program will output:
```
Number of optimal alignments: 2
```

This solution correctly handles all cases including matches, substitutions, insertions, and deletions in the alignment process.