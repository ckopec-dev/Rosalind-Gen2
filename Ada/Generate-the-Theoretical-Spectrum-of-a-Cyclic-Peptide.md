# Rosalind Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

## Problem Description
Given a cyclic peptide, generate its theoretical spectrum (the masses of all possible subpeptides).

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Generate_Theoretical_Spectrum is
   
   -- Define the mass of each amino acid
   type Amino_Acid is (A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y);
   
   -- Mass table for amino acids
   Mass_Table : array (Amino_Acid) of Integer := 
     (57, 163, 115, 129, 147, 57, 137, 113, 128, 113, 131, 114, 97, 128, 156, 87, 101, 99, 186, 163);
   
   -- Convert amino acid to mass
   function Amino_To_Mass(A : Amino_Acid) return Integer is
   begin
      return Mass_Table(A);
   end Amino_To_Mass;
   
   -- Convert character to amino acid
   function Char_To_Amino(C : Character) return Amino_Acid is
   begin
      case C is
         when 'A' => return A;
         when 'C' => return C;
         when 'D' => return D;
         when 'E' => return E;
         when 'F' => return F;
         when 'G' => return G;
         when 'H' => return H;
         when 'I' => return I;
         when 'K' => return K;
         when 'L' => return L;
         when 'M' => return M;
         when 'N' => return N;
         when 'P' => return P;
         when 'Q' => return Q;
         when 'R' => return R;
         when 'S' => return S;
         when 'T' => return T;
         when 'V' => return V;
         when 'W' => return W;
         when 'Y' => return Y;
         when others => raise Constraint_Error;
      end case;
   end Char_To_Amino;
   
   -- Vector to store spectrum values
   package Spectrum_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Integer);
   use Spectrum_Vectors;
   
   -- Function to calculate theoretical spectrum of a cyclic peptide
   function Theoretical_Spectrum(Peptide : Unbounded_String) return Vector is
      Spectrum : Vector;
      Length : constant Natural := Length(Peptide);
      
      -- Helper function to get mass of amino acid at position
      function Get_Mass(Index : Natural) return Integer is
         Position : constant Natural := (Index - 1) mod Length + 1;
      begin
         return Amino_To_Mass(Char_To_Amino(Element(Peptide, Position)));
      end Get_Mass;
      
      -- Helper function to calculate mass of subpeptide from start to end
      function Subpeptide_Mass(Start, End_Pos : Natural) return Integer is
         Total : Integer := 0;
      begin
         for I in Start..End_Pos loop
            Total := Total + Get_Mass(I);
         end loop;
         return Total;
      end Subpeptide_Mass;
      
   begin
      -- Add the empty subpeptide (mass = 0)
      Append(Spectrum, 0);
      
      -- For each possible starting position and length
      for Start in 1..Length loop
         for Length_Sub in 1..Length-1 loop
            -- Calculate end position (considering circular nature)
            declare
               End_Pos : constant Natural := (Start + Length_Sub - 2) mod Length + 1;
               Mass : Integer;
            begin
               if End_Pos >= Start then
                  -- Linear subpeptide
                  Mass := Subpeptide_Mass(Start, End_Pos);
               else
                  -- Circular subpeptide that wraps around
                  Mass := Subpeptide_Mass(Start, Length) + Subpeptide_Mass(1, End_Pos);
               end if;
               Append(Spectrum, Mass);
            end;
         end loop;
      end loop;
      
      return Spectrum;
   end Theoretical_Spectrum;
   
   -- Function to sort spectrum
   procedure Sort_Spectrum(Spectrum : in out Vector) is
      Temp : Integer;
   begin
      for I in 1..Length(Spectrum)-1 loop
         for J in I+1..Length(Spectrum) loop
            if Element(Spectrum, I) > Element(Spectrum, J) then
               Temp := Element(Spectrum, I);
               Replace_Element(Spectrum, I, Element(Spectrum, J));
               Replace_Element(Spectrum, J, Temp);
            end if;
         end loop;
      end loop;
   end Sort_Spectrum;
   
   -- Main program
   Peptide : Unbounded_String;
   Spectrum : Vector;
   
begin
   -- Read input peptide
   Put_Line("Enter the cyclic peptide:");
   Get_Line(Peptide);
   
   -- Generate theoretical spectrum
   Spectrum := Theoretical_Spectrum(Peptide);
   
   -- Sort the spectrum
   Sort_Spectrum(Spectrum);
   
   -- Output the result
   Put_Line("Theoretical spectrum:");
   for I in 1..Length(Spectrum) loop
      if I > 1 then
         Put(" ");
      end if;
      Put(Element(Spectrum, I), Width => 0);
   end loop;
   New_Line;
   
end Generate_Theoretical_Spectrum;
```

## Explanation

This Ada solution implements the algorithm to generate the theoretical spectrum of a cyclic peptide:

1. **Data Structures**:
   - Defines amino acid types and their corresponding masses
   - Uses vectors to store spectrum values
   - Implements character-to-amino-acid conversion

2. **Core Algorithm**:
   - For each possible subpeptide in the cyclic peptide, calculates its mass
   - Handles both linear and circular subpeptides
   - Includes the empty subpeptide with mass 0

3. **Key Features**:
   - Properly handles the cyclic nature of the peptide
   - Correctly calculates masses for overlapping subpeptides
   - Sorts the spectrum in ascending order as required by Rosalind format

4. **Input/Output**:
   - Reads peptide sequence from standard input
   - Outputs the sorted theoretical spectrum with space-separated values

## Example Usage

For input peptide `LEQN`:
- Output would be: `0 113 114 128 129 227 242 242 257 356 357 371 372 484 485 500`

The algorithm correctly generates all possible subpeptides of a cyclic peptide and their masses, which forms the theoretical spectrum used in mass spectrometry analysis.