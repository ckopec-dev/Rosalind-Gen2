# Rosalind Problem: Matching a Spectrum to a Protein

## Problem Description
Given a cyclic spectrum and a protein string, determine if the protein's theoretical spectrum matches the given spectrum.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
procedure Matching_A_Spectrum_to_a_Protein is
   
   -- Amino acid masses
   type Amino_Acid is ('A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 
                      'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'Y');
   
   type Mass_Array is array (Amino_Acid) of Integer;
   
   -- Predefined amino acid masses
   Amino_Masses : constant Mass_Array := (
      'A' => 71, 'C' => 103, 'D' => 115, 'E' => 129, 'F' => 147,
      'G' => 57,  'H' => 137, 'I' => 113, 'K' => 128, 'L' => 113,
      'M' => 131, 'N' => 114, 'P' => 97,  'Q' => 128, 'R' => 156,
      'S' => 87,  'T' => 101, 'V' => 99,  'W' => 186, 'Y' => 163
   );
   
   -- Vector type for storing masses
   package Mass_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                       Element_Type => Integer);
   
   function Get_Mass (AA : Amino_Acid) return Integer is
   begin
      return Amino_Masses(AA);
   end Get_Mass;
   
   function Protein_To_Spectrum (Protein : String) return Mass_Vectors.Vector is
      Spectrum : Mass_Vectors.Vector;
      Total_Mass : Integer := 0;
   begin
      for I in Protein'First .. Protein'Last loop
         declare
            AA : constant Amino_Acid := Amino_Acid'Value(Protein(I..I));
         begin
            Total_Mass := Total_Mass + Get_Mass(AA);
            Spectrum.Append(Total_Mass);
         end;
      end loop;
      
      return Spectrum;
   end Protein_To_Spectrum;
   
   function Is_Spectrum_Match (Spectrum : Mass_Vectors.Vector; 
                              Protein  : String) return Boolean is
      Protein_Spectrum : constant Mass_Vectors.Vector := Protein_To_Spectrum(Protein);
   begin
      if Spectrum.Length /= Protein_Spectrum.Length then
         return False;
      end if;
      
      for I in Spectrum.First_Index .. Spectrum.Last_Index loop
         if Spectrum.Element(I) /= Protein_Spectrum.Element(I) then
            return False;
         end if;
      end loop;
      
      return True;
   end Is_Spectrum_Match;
   
   -- Read input from stdin
   procedure Read_Input (Protein : out String; 
                        Spectrum : out Mass_Vectors.Vector) is
      Line : Unbounded_String;
      N : Integer;
   begin
      -- Read protein string
      Get_Line(Protein);
      
      -- Read spectrum size
      Get(N);
      
      -- Read spectrum values
      for I in 1 .. N loop
         declare
            Mass : Integer;
         begin
            Get(Mass);
            Spectrum.Append(Mass);
         end;
      end loop;
   end Read_Input;
   
   -- Main execution
   procedure Main is
      Protein : String(1..100);
      Spectrum : Mass_Vectors.Vector;
      Match : Boolean;
   begin
      Read_Input(Protein, Spectrum);
      
      Match := Is_Spectrum_Match(Spectrum, Protein);
      
      if Match then
         Put_Line("Yes");
      else
         Put_Line("No");
      end if;
   end Main;

begin
   Main;
end Matching_A_Spectrum_to_a_Protein;
```

## Explanation

This Ada solution addresses the Rosalind problem by:

1. **Defining amino acid masses**: Creates a mapping of each amino acid to its mass value using an array indexed by amino acid characters.

2. **Spectrum generation function**: `Protein_To_Spectrum` converts a protein string into its theoretical spectrum by calculating cumulative masses at each position.

3. **Matching function**: `Is_Spectrum_Match` compares the given spectrum with the protein's theoretical spectrum to check for exact matches.

4. **Input handling**: Reads the protein sequence and spectrum values from standard input.

5. **Output**: Returns "Yes" if the spectrum matches, "No" otherwise.

## Key Features

- Uses Ada containers for dynamic arrays (Mass_Vectors)
- Implements proper error checking for spectrum size matching
- Handles string-to-character conversion correctly
- Follows Ada best practices with proper package organization
- Includes comprehensive type definitions and functions

The solution efficiently compares theoretical spectra with experimental data to determine protein identification matches.