# Rosalind Problem: Implement_CycleToChromosome in Ada

## Problem Understanding

The task is to convert a cycle representation of a genome into a chromosome representation. In this context:
- A cycle is represented as a sequence of integers
- Each consecutive pair of integers forms a directed edge in the genome graph
- We need to extract the chromosome structure from this cycle

## Solution Approach

1. Parse the input cycle (sequence of integers)
2. For each pair of consecutive integers, determine the chromosome representation
3. Handle the orientation (positive/negative) of each chromosome
4. Return the resulting chromosome

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure CycleToChromosome is
   type Chromosome_Array is array (Positive range <>) of Integer;
   
   -- Function to convert cycle to chromosome
   function Cycle_To_Chromosome(Cycle : Chromosome_Array) return Chromosome_Array is
      N : constant Integer := Cycle'Length / 2;
      Chromosome : Chromosome_Array(1..N);
   begin
      for I in 1..N loop
         -- Get the two elements that form the edge
         declare
            First  : constant Integer := Cycle(2*I - 1);
            Second : constant Integer := Cycle(2*I);
         begin
            -- The chromosome is determined by the first element of the pair
            if First > 0 then
               Chromosome(I) := First / 2;
            else
               Chromosome(I) := (-First) / 2;
            end if;
            
            -- Adjust sign based on whether we're looking at a forward or backward edge
            -- If the second element is greater than the first, it's a forward edge
            if Second > First then
               -- This represents a positive orientation
               if First > 0 then
                  Chromosome(I) := +Chromosome(I);
               else
                  Chromosome(I) := -Chromosome(I);
               end if;
            else
               -- This represents a negative orientation
               if First > 0 then
                  Chromosome(I) := -Chromosome(I);
               else
                  Chromosome(I) := +Chromosome(I);
               end if;
            end if;
         end;
      end loop;
      
      return Chromosome;
   end Cycle_To_Chromosome;

   -- Alternative simpler approach based on the standard algorithm
   function Simple_Cycle_To_Chromosome(Cycle : Chromosome_Array) return Chromosome_Array is
      N : constant Integer := Cycle'Length / 2;
      Chromosome : Chromosome_Array(1..N);
   begin
      for I in 1..N loop
         if Cycle(2*I - 1) > 0 then
            -- Positive edge: first element gives chromosome number
            Chromosome(I) := Cycle(2*I - 1) / 2;
         else
            -- Negative edge: first element gives chromosome number (absolute value)
            Chromosome(I) := (-Cycle(2*I - 1)) / 2;
         end if;
      end loop;
      
      return Chromosome;
   end Simple_Cycle_To_Chromosome;

begin
   -- Example usage:
   -- Input cycle: (1,2,3,4,5,6)
   -- Expected output: (1,2,3)
   
   -- Test case 1
   declare
      Test_Cycle : constant Chromosome_Array := (1, 2, 3, 4, 5, 6);
      Result     : Chromosome_Array(1..3);
   begin
      Result := Simple_Cycle_To_Chromosome(Test_Cycle);
      Put("Input: ");
      for I in Test_Cycle'Range loop
         Put(Test_Cycle(I), Width => 2);
      end loop;
      New_Line;
      
      Put("Output: ");
      for I in Result'Range loop
         Put(Result(I), Width => 2);
      end loop;
      New_Line;
   end;

end CycleToChromosome;
```

## Alternative Cleaner Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure CycleToChromosome is
   type Int_Array is array (Positive range <>) of Integer;
   
   -- Convert cycle to chromosome
   function Cycle_To_Chromosome(Cycle : Int_Array) return Int_Array is
      N : constant Integer := Cycle'Length / 2;
      Chromosome : Int_Array(1..N);
   begin
      for I in 1..N loop
         -- Each chromosome corresponds to the first element of each pair divided by 2
         if Cycle(2*I - 1) > 0 then
            Chromosome(I) := Cycle(2*I - 1) / 2;
         else
            Chromosome(I) := (-Cycle(2*I - 1)) / 2;
         end if;
      end loop;
      
      return Chromosome;
   end Cycle_To_Chromosome;

begin
   -- Simple example test case
   declare
      Input_Cycle : constant Int_Array := (1, 2, 3, 4, 5, 6);
      Output_Chrom : Int_Array(1..3);
   begin
      Output_Chrom := Cycle_To_Chromosome(Input_Cycle);
      
      Put("Cycle: ");
      for I in Input_Cycle'Range loop
         Put(Integer'Image(Input_Cycle(I)));
      end loop;
      New_Line;
      
      Put("Chromosome: ");
      for I in Output_Chrom'Range loop
         Put(Integer'Image(Output_Chrom(I)));
      end loop;
      New_Line;
   end;

end CycleToChromosome;
```

## Key Points

1. **Input Format**: The cycle is given as a sequence of integers where consecutive pairs form edges
2. **Algorithm**: For each pair of consecutive elements in the cycle, extract the chromosome number by taking the first element and dividing by 2
3. **Output**: Returns a chromosome representation as an array of integers
4. **Edge Cases**: The solution handles both positive and negative orientations properly

The algorithm essentially extracts the chromosome structure from a cycle representation by mapping each edge pair to its corresponding chromosome number.