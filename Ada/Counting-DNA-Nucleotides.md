# Rosalind Problem: Counting DNA Nucleotides - Ada Solution

## Problem Statement
Given a DNA string of length at most 1000 nucleotides, return four integers representing the counts of 'A', 'C', 'G', and 'T' respectively.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Counting_DNA_Nucleotides is
   type Nucleotide_Count is array ('A' .. 'T') of Natural;
   
   -- Read DNA sequence from input
   DNA : Unbounded_String := To_Unbounded_String(Get_Line);
   
   -- Initialize counters
   Counts : Nucleotide_Count := (others => 0);
   
   -- Process each character in the DNA string
   procedure Process_Character(Char : Character) is
   begin
      case Char is
         when 'A' => Counts('A') := Counts('A') + 1;
         when 'C' => Counts('C') := Counts('C') + 1;
         when 'G' => Counts('G') := Counts('G') + 1;
         when 'T' => Counts('T') := Counts('T') + 1;
         when others => null; -- Ignore invalid characters
      end case;
   end Process_Character;
   
begin
   -- Count nucleotides in the DNA sequence
   for I in 1 .. Length(DNA) loop
      Process_Character(Element(DNA, I));
   end loop;
   
   -- Output results in order A C G T
   Put(Counts('A'), 0);
   Put(" ");
   Put(Counts('C'), 0);
   Put(" ");
   Put(Counts('G'), 0);
   Put(" ");
   Put(Counts('T'), 0);
   New_Line;
   
end Counting_DNA_Nucleotides;
```

## Explanation

This Ada solution works as follows:

1. **Data Types**: 
   - Uses an array type `Nucleotide_Count` to store counts for each nucleotide ('A' through 'T')
   - Uses `Unbounded_String` to read the DNA sequence from input

2. **Algorithm**:
   - Reads the DNA string from standard input
   - Iterates through each character in the string
   - Increments the appropriate counter based on the nucleotide type
   - Outputs the counts in the required order: A C G T

3. **Key Features**:
   - Uses Ada's case statement for clean, readable character handling
   - Handles invalid characters gracefully (ignores them)
   - Properly formats output with spaces between counts
   - Uses `Natural` type for counting (non-negative integers)

## Sample Input/Output

**Input:**
```
AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
```

**Output:**
```
20 12 17 21
```

This solution efficiently processes the DNA sequence in O(n) time where n is the length of the string, and uses O(1) extra space.