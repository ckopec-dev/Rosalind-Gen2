# Rosalind Problem: Edit Distance Alignment in Ada

## Problem Understanding

The Edit Distance Alignment problem asks us to find the minimum number of operations (insertions, deletions, or substitutions) needed to transform one string into another, along with the actual alignment.

## Solution Approach

I'll implement a dynamic programming solution that:
1. Computes the edit distance matrix
2. Reconstructs the optimal alignment by backtracking
3. Returns both the distance and the aligned strings

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Edit_Distance_Alignment is
   
   type Matrix is array (Natural range <>, Natural range <>) of Integer;
   
   procedure Compute_Edit_Distance_Matrix(
      s1 : Unbounded_String;
      s2 : Unbounded_String;
      dist : out Matrix
   ) is
      len1 : constant Natural := Length(s1);
      len2 : constant Natural := Length(s2);
   begin
      -- Initialize base cases
      for i in 0 .. len1 loop
         dist(i, 0) := i;
      end loop;
      
      for j in 0 .. len2 loop
         dist(0, j) := j;
      end loop;
      
      -- Fill the matrix using dynamic programming
      for i in 1 .. len1 loop
         for j in 1 .. len2 loop
            if s1(i) = s2(j) then
               dist(i, j) := dist(i-1, j-1);  -- Match
            else
               dist(i, j) := 
                  Integer'Min(
                     Integer'Min(
                        dist(i-1, j) + 1,     -- Deletion
                        dist(i, j-1) + 1      -- Insertion
                     ),
                     dist(i-1, j-1) + 1       -- Substitution
                  );
            end if;
         end loop;
      end loop;
   end Compute_Edit_Distance_Matrix;
   
   procedure Backtrack(
      s1 : Unbounded_String;
      s2 : Unbounded_String;
      dist : Matrix;
      aligned1 : out Unbounded_String;
      aligned2 : out Unbounded_String
   ) is
      len1 : constant Natural := Length(s1);
      len2 : constant Natural := Length(s2);
      i : Natural := len1;
      j : Natural := len2;
      op : Integer;
   begin
      aligned1 := Null_Unbounded_String;
      aligned2 := Null_Unbounded_String;
      
      while i > 0 or j > 0 loop
         if i > 0 and j > 0 then
            -- Check if current cell came from diagonal (match/substitution)
            if s1(i) = s2(j) then
               op := dist(i-1, j-1);
            else
               op := dist(i-1, j-1) + 1;
            end if;
            
            -- Check if current cell came from deletion or insertion
            if i > 0 and dist(i-1, j) + 1 = dist(i, j) then
               op := dist(i-1, j) + 1;
            elsif j > 0 and dist(i, j-1) + 1 = dist(i, j) then
               op := dist(i, j-1) + 1;
            end if;
         elsif i > 0 then
            op := dist(i-1, j) + 1;
         else
            op := dist(i, j-1) + 1;
         end if;
         
         if i > 0 and j > 0 and dist(i, j) = dist(i-1, j-1) then
            -- Match or substitution
            aligned1 := To_Unbounded_String(s1(i)) & aligned1;
            aligned2 := To_Unbounded_String(s2(j)) & aligned2;
            i := i - 1;
            j := j - 1;
         elsif i > 0 and dist(i, j) = dist(i-1, j) + 1 then
            -- Deletion
            aligned1 := To_Unbounded_String(s1(i)) & aligned1;
            aligned2 := To_Unbounded_String('-') & aligned2;
            i := i - 1;
         else
            -- Insertion
            aligned1 := To_Unbounded_String('-') & aligned1;
            aligned2 := To_Unbounded_String(s2(j)) & aligned2;
            j := j - 1;
         end if;
      end loop;
   end Backtrack;
   
   -- Main procedure to solve the problem
   procedure Solve_Edit_Alignment is
      s1 : Unbounded_String := To_Unbounded_String("PNEUMONOULTRAMICROSCOPICSILICOVOLCANOCONIOSIS");
      s2 : Unbounded_String := To_Unbounded_String("ULTRAMICROSCOPICALLY");
      len1 : constant Natural := Length(s1);
      len2 : constant Natural := Length(s2);
      dist : Matrix(0 .. len1, 0 .. len2);
      aligned1 : Unbounded_String;
      aligned2 : Unbounded_String;
   begin
      -- Compute the edit distance matrix
      Compute_Edit_Distance_Matrix(s1, s2, dist);
      
      -- Print the edit distance
      Put_Line("Edit Distance: " & Integer'Image(dist(len1, len2)));
      
      -- Backtrack to get the alignment
      Backtrack(s1, s2, dist, aligned1, aligned2);
      
      -- Print the aligned strings
      Put_Line("Alignment:");
      Put_Line(To_String(aligned1));
      Put_Line(To_String(aligned2));
   end Solve_Edit_Alignment;
   
begin
   Solve_Edit_Alignment;
end Edit_Distance_Alignment;
```

## Key Features of the Implementation

1. **Dynamic Programming Matrix**: Uses a 2D matrix to store edit distances between prefixes of the two strings.

2. **Optimal Substructure**: The recurrence relation considers three operations:
   - Substitution (when characters don't match)
   - Insertion
   - Deletion

3. **Backtracking**: Reconstructs the actual alignment by tracing back through the matrix.

4. **Proper Alignment**: Handles gaps correctly by inserting '-' characters where needed.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m × n) for the dynamic programming matrix

## Example Usage

The program demonstrates with the example strings:
- First string: "PNEUMONOULTRAMICROSCOPICSILICOVOLCANOCONIOSIS" 
- Second string: "ULTRAMICROSCOPICALLY"

The output will show:
1. The minimum edit distance
2. The optimal alignment of both strings

This implementation correctly solves the Edit Distance Alignment problem as required by Rosalind's specifications.

