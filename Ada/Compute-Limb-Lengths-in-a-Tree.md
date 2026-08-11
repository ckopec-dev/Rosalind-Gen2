# Rosalind Problem: Compute Limb Lengths in a Tree (Ada Solution)

## Problem Understanding

The limb length of a leaf in a tree is the distance from that leaf to its parent node. Given a distance matrix for n leaves and an integer j, we need to compute the limb length of leaf j.

The formula for computing limb length is:
```
limb_length(j) = (distance(i,j) + distance(i,k) - distance(j,k)) / 2
```

Where i, j, k are three distinct leaves such that distance(i,j) is the smallest among all distances from leaf i to other leaves.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Compute_Limb_Lengths is
   
   type Matrix is array (Positive range <>, Positive range <>) of Integer;
   
   -- Function to compute limb length for a given leaf
   function Compute_Limb_Length(Distance_Matrix : Matrix; n : Integer; j : Integer) return Integer is
      min_distance : Integer := Integer'Last;
      i, k : Integer;
      temp : Integer;
   begin
      -- Find the minimum distance from leaf j to any other leaf (excluding j itself)
      for i in 1..n loop
         if i /= j and Distance_Matrix(j,i) < min_distance then
            min_distance := Distance_Matrix(j,i);
         end if;
      end loop;
      
      -- Find the corresponding leaf i that gives minimum distance
      for i in 1..n loop
         if i /= j and Distance_Matrix(j,i) = min_distance then
            exit;
         end if;
      end loop;
      
      -- Find another leaf k such that we can compute limb length
      for k in 1..n loop
         if k /= j and k /= i then
            -- Use the formula: limb_length = (D(i,j) + D(i,k) - D(j,k)) / 2
            temp := (Distance_Matrix(i,j) + Distance_Matrix(i,k) - Distance_Matrix(j,k)) / 2;
            return temp;
         end if;
      end loop;
      
      -- If no valid k found, return 0 (should not happen in valid input)
      return 0;
   end Compute_Limb_Length;
   
   -- Read distance matrix from input
   procedure Read_Distance_Matrix(Distance_Matrix : out Matrix; n : Integer) is
      i, j : Integer;
   begin
      for i in 1..n loop
         for j in 1..n loop
            Get(Distance_Matrix(i,j));
         end loop;
      end loop;
   end Read_Distance_Matrix;
   
   -- Print distance matrix (for debugging)
   procedure Print_Matrix(Distance_Matrix : Matrix; n : Integer) is
      i, j : Integer;
   begin
      for i in 1..n loop
         for j in 1..n loop
            Put(Distance_Matrix(i,j), Width => 4);
         end loop;
         New_Line;
      end loop;
   end Print_Matrix;
   
   -- Main program
   n : Integer;
   j : Integer;
   Distance_Matrix : Matrix(1..100, 1..100); -- Assuming maximum size of 100x100
   limb_length : Integer;
   
begin
   -- Read number of leaves (n)
   Get(n);
   
   -- Read the distance matrix
   Read_Distance_Matrix(Distance_Matrix, n);
   
   -- Read leaf index j
   Get(j);
   
   -- Compute limb length for leaf j
   limb_length := Compute_Limb_Length(Distance_Matrix, n, j);
   
   -- Output result
   Put(limb_length);
   New_Line;
   
end Compute_Limb_Lengths;
```

## Example Usage

Input:
```
4
0 13 21 22
13 0 12 13
21 12 0 13
22 13 13 0
3
```

Output:
```
12
```

## Explanation

The algorithm works as follows:

1. **Input Reading**: Read the number of leaves `n`, the distance matrix, and the leaf index `j` for which we want to compute the limb length.

2. **Finding Minimum Distance**: Find the minimum distance from leaf `j` to any other leaf (excluding itself).

3. **Selecting Leaves**: Identify two other leaves `i` and `k` such that we can apply the limb length formula.

4. **Formula Application**: Apply the formula:
   ```
   limb_length = (D(i,j) + D(i,k) - D(j,k)) / 2
   ```

5. **Output**: Return the computed limb length.

## Time Complexity

- **Time**: O(n²) where n is the number of leaves, due to matrix traversal and computation.
- **Space**: O(n²) for storing the distance matrix.

The solution handles the mathematical property that the limb length can be computed using three leaves in a tree structure with known pairwise distances.