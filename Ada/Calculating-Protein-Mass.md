# Rosalind Problem: Calculating Protein Mass in Ada

## Problem Description
Given a protein string of length at most 1000 aa, return the total weight of the protein.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Calculating_Protein_Mass is
   
   -- Define a vector to store amino acid masses
   type Mass_Type is digits 10;
   
   -- Amino acid masses (from Rosalind problem)
   Mass_Map : array ('A' .. 'Z') of Mass_Type := 
     ('A' => 71.03711,
      'C' => 103.00919,
      'D' => 115.02694,
      'E' => 129.04259,
      'F' => 147.06841,
      'G' => 57.02137,
      'H' => 137.05891,
      'I' => 113.08406,
      'K' => 128.09496,
      'L' => 113.08406,
      'M' => 131.04049,
      'N' => 114.04293,
      'P' => 97.05276,
      'Q' => 128.05858,
      'R' => 156.10111,
      'S' => 87.03203,
      'T' => 101.04768,
      'V' => 99.06841,
      'W' => 186.07931,
      'Y' => 163.06333,
      others => 0.0);
   
   -- Read input protein string
   Protein : Unbounded_String;
   Total_Mass : Mass_Type := 0.0;
   
begin
   -- Read the protein string from standard input
   Get_Line(Protein);
   
   -- Calculate total mass
   for I in 1 .. Length(Protein) loop
      declare
         Amino_Acid : Character := Element(Protein, I);
      begin
         Total_Mass := Total_Mass + Mass_Map(Amino_Acid);
      end;
   end loop;
   
   -- Output the result with appropriate precision
   Put(Total_Mass, Fore => 1, Aft => 3, Exp => 0);
   New_Line;
   
end Calculating_Protein_Mass;
```

## Explanation

This Ada solution:

1. **Defines mass mapping**: Creates an array mapping each amino acid character to its exact mass value
2. **Reads input**: Uses `Get_Line` to read the protein string from standard input
3. **Calculates total mass**: Iterates through each character in the protein string and accumulates the mass values
4. **Outputs result**: Prints the total mass with 3 decimal places precision

## Key Features

- Uses Ada's strong typing system with `Mass_Type` for precise calculations
- Handles input/output using standard Ada text I/O procedures
- Efficient character-by-character processing of the protein string
- Proper rounding to required decimal precision (3 decimal places)

## Sample Input/Output

**Input:**
```
SKADYEK
```

**Output:**
```
821.392
```

The solution correctly handles the standard amino acid masses and provides precise floating-point arithmetic as required by the Rosalind problem specifications.