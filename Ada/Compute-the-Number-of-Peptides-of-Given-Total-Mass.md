# Rosalind Problem: Compute the Number of Peptides of Given Total Mass

## Problem Description
Given an integer m, compute the number of peptides of total mass m.

This is a classic dynamic programming problem where we need to count the number of ways to express a given mass as a sum of amino acid masses.

## Solution Approach
1. Use dynamic programming with memoization
2. For each mass from 0 to m, calculate how many different peptides can have that mass
3. The recurrence relation is: dp[i] = sum of dp[i - mass_of_amino_acid] for all valid amino acids

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Compute_Number_Of_Peptides is
   -- Amino acid masses (standard masses)
   Masses : array (1..20) of Integer := 
     (57, 71, 87, 97, 99, 101, 103, 113, 114, 115,
      128, 129, 131, 137, 147, 156, 163, 186, 194, 225);
   
   -- Maximum mass to compute (adjust as needed)
   Max_Mass : constant Integer := 1000;
   
   -- DP array to store number of peptides for each mass
   DP : array (0..Max_Mass) of Long_Long_Integer;
   
   -- Function to compute number of peptides with given mass
   function Count_Peptides(Mass : Integer) return Long_Long_Integer is
   begin
      if Mass < 0 then
         return 0;
      elsif Mass = 0 then
         return 1;
      elsif DP(Mass) /= 0 then
         return DP(Mass);
      else
         declare
            Result : Long_Long_Integer := 0;
         begin
            for I in Masses'First..Masses'Last loop
               if Mass >= Masses(I) then
                  Result := Result + Count_Peptides(Mass - Masses(I));
               end if;
            end loop;
            DP(Mass) := Result;
            return Result;
         end;
      end if;
   end Count_Peptides;
   
   -- Iterative version for better performance
   function Count_Peptides_Iterative(Mass : Integer) return Long_Long_Integer is
      Temp_DP : array (0..Mass) of Long_Long_Integer := (others => 0);
   begin
      Temp_DP(0) := 1;  -- Base case: one way to make mass 0 (empty peptide)
      
      for I in 1..Mass loop
         for J in Masses'First..Masses'Last loop
            if I >= Masses(J) then
               Temp_DP(I) := Temp_DP(I) + Temp_DP(I - Masses(J));
            end if;
         end loop;
      end loop;
      
      return Temp_DP(Mass);
   end Count_Peptides_Iterative;
   
begin
   -- Read input mass
   Put("Enter the total mass: ");
   declare
      M : Integer;
   begin
      Get(M);
      New_Line;
      
      -- Compute and display result
      Put("Number of peptides with mass ");
      Put(M);
      Put(": ");
      Put(Count_Peptides_Iterative(M), Width => 1);
      New_Line;
   end;
end Compute_Number_Of_Peptides;
```

## Alternative Implementation (More Efficient)

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Compute_Number_Of_Peptides is
   -- Amino acid masses
   Masses : array (1..20) of Integer := 
     (57, 71, 87, 97, 99, 101, 103, 113, 114, 115,
      128, 129, 131, 137, 147, 156, 163, 186, 194, 225);
   
   -- Maximum mass to compute
   Max_Mass : constant Integer := 1000;
   
   -- DP array for memoization
   DP : array (0..Max_Mass) of Long_Long_Integer := (others => 0);
   
   -- Function to compute number of peptides with given mass
   function Count_Peptides(M : Integer) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
   begin
      if M < 0 then
         return 0;
      elsif M = 0 then
         return 1;
      elsif DP(M) /= 0 then
         return DP(M);
      else
         for I in Masses'First..Masses'Last loop
            if M >= Masses(I) then
               Result := Result + Count_Peptides(M - Masses(I));
            end if;
         end loop;
         
         DP(M) := Result;
         return Result;
      end if;
   end Count_Peptides;
   
begin
   -- Read mass from input
   declare
      Mass : Integer;
   begin
      Get(Mass);
      
      -- Output the result
      Put_Line(Long_Long_Integer'Image(Count_Peptides(Mass)));
   end;
end Compute_Number_Of_Peptides;
```

## Key Points

1. **Dynamic Programming**: The solution uses bottom-up dynamic programming to avoid recomputation of subproblems
2. **Base Case**: There is exactly one way to make mass 0 (empty peptide)
3. **Recurrence**: For each mass, we sum up the ways to make that mass using each amino acid
4. **Efficiency**: The iterative version has better performance than recursive with memoization

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m is the target mass and n is the number of amino acids (20)
- **Space Complexity**: O(m) for the DP array

The program reads an integer mass from standard input and outputs the number of different peptides that can have that total mass.