# Rosalind Problem: Complementing a Strand of DNA in Ada

## Problem Statement
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'. The reverse complement of a DNA string is formed by reversing the string and then taking the complement of each symbol.

Given: A DNA string s of length at most 1000 bp.

Return: The reverse complement s^c of s.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Complementing_a_Strand_of_DNA is
   function Complement_Nucleotide(Nucleotide : Character) return Character is
   begin
      case Nucleotide is
         when 'A' => return 'T';
         when 'T' => return 'A';
         when 'C' => return 'G';
         when 'G' => return 'C';
         when others => return Nucleotide;  -- Handle invalid characters
      end case;
   end Complement_Nucleotide;

   function Reverse_Complement(DNA_String : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
      Length : constant Natural := DNA_String'Length;
   begin
      -- Process string from right to left and complement each nucleotide
      for I in reverse 1 .. Length loop
         Append(Result, Complement_Nucleotide(DNA_String(I)));
      end loop;
      
      return To_String(Result);
   end Reverse_Complement;

   DNA : String(1..1000);
   Length : Natural;
begin
   -- Read input DNA string
   Get_Line(DNA, Length);
   
   -- Output reverse complement
   Put_Line(Reverse_Complement(DNA(1..Length)));
end Complementing_a_Strand_of_DNA;
```

## Explanation

This Ada solution follows these steps:

1. **Complement Function**: `Complement_Nucleotide` takes a single nucleotide character and returns its complement:
   - 'A' → 'T'
   - 'T' → 'A' 
   - 'C' → 'G'
   - 'G' → 'C'

2. **Reverse Complement Function**: `Reverse_Complement` processes the DNA string by:
   - Iterating through the string from right to left (reverse order)
   - Complementing each nucleotide using the helper function
   - Building the result string

3. **Main Procedure**:
   - Reads the input DNA string from standard input
   - Calls the reverse complement function
   - Outputs the result

## Example

Input: `AAAACCCGGT`
Output: `ACC GG GAAAA`

The algorithm first reverses the string to get `TGGCCCAAAA`, then complements each nucleotide to produce `ACC GG GAAAA`.