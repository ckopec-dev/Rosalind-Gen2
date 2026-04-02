# Rosalind Problem: Counting Point Mutations (Ada Solution)

## Problem Description
Given two DNA strings of equal length, count the number of positions where the nucleotides differ (Hamming distance).

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Counting_Point_Mutations is
   -- Function to count point mutations between two DNA strings
   function Count_Mutations(DNA1, DNA2 : Unbounded_String) return Natural is
      Count : Natural := 0;
   begin
      for I in 1 .. Length(DNA1) loop
         if Element(DNA1, I) /= Element(DNA2, I) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Mutations;

   -- Read DNA strings from input
   DNA1 : Unbounded_String;
   DNA2 : Unbounded_String;
begin
   -- Read the two DNA strings
   Get_Line(DNA1);
   Get_Line(DNA2);
   
   -- Output the Hamming distance
   Put_Line(Count_Mutations(DNA1, DNA2)'Image);
end Counting_Point_Mutations;
```

## How It Works

1. **Function `Count_Mutations`**: 
   - Takes two `Unbounded_String` parameters representing DNA sequences
   - Iterates through each position in the strings
   - Compares nucleotides at each position
   - Increments counter when nucleotides differ
   - Returns the total count of differences

2. **Main Procedure**:
   - Reads two DNA strings from standard input
   - Calls the mutation counting function
   - Outputs the result as a string representation

## Example Input/Output

**Input:**
```
GAGCCTACTAACGGGAT
CATCGGTATCGGGTTAC
```

**Output:**
```
7
```

## Key Features

- Uses `Ada.Strings.Unbounded` for flexible string handling
- Handles DNA strings of equal length (as required by the problem)
- Efficient O(n) time complexity where n is the length of the strings
- Clean and readable Ada code following standard conventions

## Compilation and Execution

```bash
gnatmake counting_point_mutations.adb
./counting_point_mutations < input.txt
```

This solution correctly implements the Hamming distance calculation required for the Rosalind problem "Counting Point Mutations".

