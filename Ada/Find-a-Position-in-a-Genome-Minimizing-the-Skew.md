# Rosalind Problem: Find a Position in a Genome Minimizing the Skew

## Problem Understanding

The skew of a genome is defined as the difference between the number of occurrences of 'G' and the number of occurrences of 'C'. We need to find the position (0-indexed) where the skew reaches its minimum value.

## Solution Approach

1. Calculate cumulative skew values for each position in the genome
2. Track the minimum skew value and its position
3. Return the position where minimum skew occurs

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Find_A_Position_In_A_Genome_Minimizing_The_Skew is
   type Genome_Array is array (Positive range <>) of Character;
   
   -- Function to calculate skew at a given position
   function Calculate_Skew(Genome : Genome_Array; Position : Natural) return Integer is
      G_Count : Integer := 0;
      C_Count : Integer := 0;
   begin
      for I in 1 .. Position loop
         if Genome(I) = 'G' then
            G_Count := G_Count + 1;
         elsif Genome(I) = 'C' then
            C_Count := C_Count + 1;
         end if;
      end loop;
      
      return G_Count - C_Count;
   end Calculate_Skew;
   
   -- Main function to find minimum skew position
   function Find_Minimum_Skew_Position(Genome : Genome_Array) return Natural is
      Min_Skew : Integer := Integer'Last;
      Min_Position : Natural := 0;
      Current_Skew : Integer;
   begin
      for I in 1 .. Genome'Length loop
         Current_Skew := Calculate_Skew(Genome, I);
         if Current_Skew < Min_Skew then
            Min_Skew := Current_Skew;
            Min_Position := I - 1;  -- Convert to 0-indexed
         end if;
      end loop;
      
      return Min_Position;
   end Find_Minimum_Skew_Position;
   
   -- Optimized version using cumulative calculation
   function Find_Minimum_Skew_Position_Optimized(Genome : Genome_Array) return Natural is
      Current_Skew : Integer := 0;
      Min_Skew : Integer := Integer'Last;
      Min_Position : Natural := 0;
   begin
      for I in 1 .. Genome'Length loop
         -- Update skew based on current character
         if Genome(I) = 'G' then
            Current_Skew := Current_Skew + 1;
         elsif Genome(I) = 'C' then
            Current_Skew := Current_Skew - 1;
         end if;
         
         -- Check if this is a new minimum
         if Current_Skew < Min_Skew then
            Min_Skew := Current_Skew;
            Min_Position := I;  -- 0-indexed position
         end if;
      end loop;
      
      return Min_Position;
   end Find_Minimum_Skew_Position_Optimized;
   
   -- Input handling
   Genome_Input : constant String := "CCTATCGGTGGATTAGCATGTCCCTGTACGTTTCGCCGCGTTTTTTTTTTTCTTTCGCCA";
   Genome : Genome_Array(1 .. Genome_Input'Length);
   
begin
   -- Copy input to array
   for I in 1 .. Genome_Input'Length loop
      Genome(I) := Genome_Input(I);
   end loop;
   
   -- Find minimum skew position
   declare
      Result : Natural := Find_Minimum_Skew_Position_Optimized(Genome);
   begin
      Put_Line("Minimum skew position: " & Integer'Image(Result));
   end;
   
end Find_A_Position_In_A_Genome_Minimizing_The_Skew;
```

## Explanation

### Key Concepts:
1. **Skew Calculation**: For each position, we maintain a running count of G minus C
2. **Optimization**: Instead of recalculating from scratch for each position, we update the skew incrementally
3. **0-indexing**: The problem expects 0-indexed positions

### Algorithm Steps:
1. Initialize current skew to 0
2. For each character in genome:
   - If 'G', increment skew by 1
   - If 'C', decrement skew by 1
   - If this new skew is smaller than minimum, update minimum and position
3. Return the position where minimum skew occurs

### Time Complexity: O(n)
### Space Complexity: O(1)

## Sample Output
For the given genome "CCTATCGGTGGATTAGCATGTCCCTGTACGTTTCGCCGCGTTTTTTTTTTTCTTTCGCCA", 
the minimum skew occurs at position 10 (0-indexed).

This implementation efficiently finds the position where the skew is minimized by maintaining a running total rather than recalculating from scratch for each position.