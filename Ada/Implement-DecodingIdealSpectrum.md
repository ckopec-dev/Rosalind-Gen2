# Rosalind Problem: Implement_DecodingIdealSpectrum in Ada

I'll solve this step by step, implementing the decoding of an ideal spectrum for amino acid masses.

## Problem Understanding

The task is to decode an ideal spectrum (which contains all possible subpeptide masses) back into the original peptide sequence. This requires:
1. Finding the mass differences between consecutive peaks
2. Converting these differences to amino acid masses
3. Reconstructing the peptide sequence

## Solution Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Implement_DecodingIdealSpectrum is
   
   -- Define amino acid masses
   type Amino_Acid is (A, R, N, D, C, Q, E, G, H, I, L, K, M, F, P, S, T, W, Y, V);
   
   -- Mass table for amino acids (rounded to integers)
   Mass_Table : array(Amino_Acid) of Integer :=
     (A => 71, R => 156, N => 114, D => 115, C => 103, Q => 128, 
      E => 129, G => 57, H => 137, I => 113, L => 113, K => 128,
      M => 131, F => 147, P => 97, S => 87, T => 101, W => 186,
      Y => 163, V => 99);
   
   -- Convert amino acid to mass
   function To_Mass(AA : Amino_Acid) return Integer is
   begin
      return Mass_Table(AA);
   end To_Mass;
   
   -- Convert mass to amino acid (reverse mapping)
   function To_Amino_Acid(Mass : Integer) return Amino_Acid is
   begin
      for AA in Amino_Acid loop
         if To_Mass(AA) = Mass then
            return AA;
         end if;
      end loop;
      raise Constraint_Error with "Unknown mass";
   end To_Amino_Acid;
   
   -- Vector to store spectrum peaks
   package Spectrum_Vector is new Ada.Containers.Vectors(1..1000, Integer);
   use Spectrum_Vector;
   
   -- Function to decode the ideal spectrum
   function Decode_Ideal_Spectrum(Spectrum : Vector) return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;
      N : constant Natural := Spectrum.Length;
      
      -- Get mass differences between consecutive peaks
      procedure Get_Differences(Diffs : out Vector) is
         Diff_Vector : Vector;
      begin
         if N < 2 then
            return;
         end if;
         
         for I in 1..N-1 loop
            Append(Diff_Vector, Spectrum(Element(Spectrum, I+1)) - 
                              Spectrum(Element(Spectrum, I)));
         end loop;
         
         Diffs := Diff_Vector;
      end Get_Differences;
      
      -- Convert differences to amino acid sequence
      procedure Convert_To_Sequence(Diffs : Vector) is
         Sequence : Unbounded_String := Null_Unbounded_String;
         Current_Mass : Integer := 0;
      begin
         for I in 1..Diffs.Length loop
            declare
               Diff : constant Integer := Element(Diffs, I);
            begin
               if Diff = 71 then   -- A
                  Append(Sequence, 'A');
               elsif Diff = 156 then -- R
                  Append(Sequence, 'R');
               elsif Diff = 114 then -- N
                  Append(Sequence, 'N');
               elsif Diff = 115 then -- D
                  Append(Sequence, 'D');
               elsif Diff = 103 then -- C
                  Append(Sequence, 'C');
               elsif Diff = 128 then -- Q
                  Append(Sequence, 'Q');
               elsif Diff = 129 then -- E
                  Append(Sequence, 'E');
               elsif Diff = 57 then  -- G
                  Append(Sequence, 'G');
               elsif Diff = 137 then -- H
                  Append(Sequence, 'H');
               elsif Diff = 113 then -- I or L (both are 113)
                  Append(Sequence, 'I'); -- Using I as default
               elsif Diff = 128 then -- K
                  Append(Sequence, 'K');
               elsif Diff = 131 then -- M
                  Append(Sequence, 'M');
               elsif Diff = 147 then -- F
                  Append(Sequence, 'F');
               elsif Diff = 97 then  -- P
                  Append(Sequence, 'P');
               elsif Diff = 87 then  -- S
                  Append(Sequence, 'S');
               elsif Diff = 101 then -- T
                  Append(Sequence, 'T');
               elsif Diff = 186 then -- W
                  Append(Sequence, 'W');
               elsif Diff = 163 then -- Y
                  Append(Sequence, 'Y');
               elsif Diff = 99 then  -- V
                  Append(Sequence, 'V');
               end if;
            end;
         end loop;
         
         Result := Sequence;
      end Convert_To_Sequence;
      
   begin
      if N < 2 then
         return Null_Unbounded_String;
      end if;
      
      -- Get mass differences
      declare
         Differences : Vector;
      begin
         Get_Differences(Differences);
         Convert_To_Sequence(Differences);
      end;
      
      return Result;
   end Decode_Ideal_Spectrum;
   
   -- Main procedure to test with sample data
   procedure Test_Decoding is
      Spectrum : Vector;
      Decoded_Peptide : Unbounded_String;
   begin
      -- Example spectrum (should be sorted)
      Append(Spectrum, 0);
      Append(Spectrum, 113);
      Append(Spectrum, 128);
      Append(Spectrum, 147);
      Append(Spectrum, 163);
      Append(Spectrum, 186);
      Append(Spectrum, 229);
      
      Decoded_Peptide := Decode_Ideal_Spectrum(Spectrum);
      
      Put_Line("Decoded peptide: ");
      Put_Line(To_String(Decoded_Peptide));
   end Test_Decoding;
   
begin
   Test_Decoding;
end Implement_DecodingIdealSpectrum;
```

## Key Points of the Solution

1. **Data Structures**: Used Ada containers for dynamic arrays to handle spectrum peaks
2. **Mass Mapping**: Created a mapping between amino acids and their masses
3. **Difference Calculation**: Computes mass differences between consecutive peaks
4. **Decoding Process**: Converts these differences back into amino acid sequence
5. **Error Handling**: Includes basic error checking for unknown masses

## Algorithm Steps

1. Calculate the differences between consecutive spectrum peaks
2. Match each difference to known amino acid masses
3. Convert the mass differences to amino acid letters
4. Construct the final peptide sequence

This implementation handles the core logic of decoding an ideal spectrum into its corresponding amino acid sequence, following the principles of mass spectrometry analysis used in bioinformatics problems like those found on Rosalind.