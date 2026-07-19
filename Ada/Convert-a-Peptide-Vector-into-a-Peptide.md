# Rosalind Problem: Convert a Peptide Vector into a Peptide (Ada Solution)

## Problem Understanding

Given a peptide vector (a binary string representing a peptide), we need to convert it back into the original peptide sequence using the standard amino acid mapping.

The peptide vector has length 3n where n is the length of the peptide, and each 3-bit segment corresponds to an amino acid:
- 000 → G
- 001 → A  
- 010 → S
- 011 → P
- 100 → V
- 101 → T
- 110 → C
- 111 → Y

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Convert_Peptide_Vector is
   -- Function to convert binary string to integer
   function Binary_To_Integer(Bin_String : String) return Integer is
      Result : Integer := 0;
   begin
      for I in Bin_String'Range loop
         Result := Result * 2 + (if Bin_String(I) = '1' then 1 else 0);
      end loop;
      return Result;
   end Binary_To_Integer;

   -- Function to convert peptide vector to peptide string
   function Vector_To_Peptide(Vector : String) return Unbounded_String is
      Peptide : Unbounded_String := Null_Unbounded_String;
      Codon_Length : constant Integer := 3;
      Amino_Acids : array (0..7) of Character := 
        ('G', 'A', 'S', 'P', 'V', 'T', 'C', 'Y');
   begin
      -- Process the vector in chunks of 3 bits
      for I in 0 .. Vector'Length / Codon_Length - 1 loop
         declare
            Start_Index : constant Integer := I * Codon_Length + 1;
            Codon : String(1..Codon_Length) := 
              Vector(Start_Index .. Start_Index + Codon_Length - 1);
            Codon_Value : constant Integer := Binary_To_Integer(Codon);
         begin
            Append(Peptide, Amino_Acids(Codon_Value));
         end;
      end loop;
      
      return Peptide;
   end Vector_To_Peptide;

   -- Main program
   Vector_Input : String(1..15) := "1011100110000";
   Result : Unbounded_String;
begin
   Result := Vector_To_Peptide(Vector_Input);
   Put_Line("Input vector: " & Vector_Input);
   Put_Line("Peptide: " & To_String(Result));
end Convert_Peptide_Vector;
```

## Example

For input vector "1011100110000":
- 101 → T (5)
- 110 → C (6) 
- 011 → P (3)
- 000 → G (0)

Output: "TCPG"

## Key Features of the Solution

1. **Binary to Integer Conversion**: The `Binary_To_Integer` function converts a binary string to its decimal equivalent
2. **Codon Processing**: The vector is processed in chunks of 3 bits (codons)
3. **Amino Acid Mapping**: Uses predefined mapping from codon values to amino acids
4. **Ada String Handling**: Utilizes `Unbounded_String` for dynamic string manipulation

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the input vector
- **Space Complexity**: O(n) for storing the result peptide string

This solution efficiently converts the binary peptide vector back into its amino acid sequence representation.