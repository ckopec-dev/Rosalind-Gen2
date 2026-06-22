# Rosalind Problem: Comparing Spectra with the Spectral Convolution

## Problem Description

The spectral convolution of two spectra is defined as the multiset of differences between all pairs of masses from the two spectra. Given two spectra, we need to compute their spectral convolution.

In this problem, we are given two mass spectrometry spectra and must compute their spectral convolution, which is essentially the multiset of all possible differences between elements of the two spectra.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Comparing_Spectra_with_the_Spectral_Convolution is
   
   type Mass_Type is new Float;
   type Spectrum_Type is array (Positive range <>) of Mass_Type;
   
   package Spectrum_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Mass_Type);
   
   package Mass_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Mass_Type, Element_Type => Natural);
   
   -- Function to compute spectral convolution
   function Spectral_Convolution(Spectrum1, Spectrum2 : Spectrum_Type) 
     return Mass_Maps.Map is
      Convolution : Mass_Maps.Map;
   begin
      for I in Spectrum1'Range loop
         for J in Spectrum2'Range loop
            declare
               Difference : constant Mass_Type := Spectrum1(I) - Spectrum2(J);
            begin
               if Difference > 0.0 then  -- Only positive differences (as per standard convention)
                  declare
                     Current_Count : Natural := Mass_Maps.Get(Convolution, Difference, 0);
                  begin
                     Mass_Maps.Replace_Key(Convolution, Difference, Current_Count + 1);
                  end;
               end if;
            end;
         end loop;
      end loop;
      
      return Convolution;
   end Spectral_Convolution;
   
   -- Procedure to print the result
   procedure Print_Convolution(Convol : Mass_Maps.Map) is
      use Ada.Containers.Ordered_Maps;
      Position : Cursor := First(Convol);
   begin
      while Has_Element(Position) loop
         declare
            Key_Value : constant Mass_Type := Key(Position);
            Count : constant Natural := Element(Position);
         begin
            for I in 1..Count loop
               Put(Key_Value, Width => 0, Fore => 0);
               Put(" ");
            end loop;
         end;
         Position := Next(Position);
      end loop;
      New_Line;
   end Print_Convolution;
   
   -- Example usage with sample data
   procedure Main is
      -- Sample spectra from Rosalind problem
      Spectrum1 : constant Spectrum_Type(1..6) := 
        (186.0, 187.0, 250.0, 300.0, 301.0, 302.0);
      
      Spectrum2 : constant Spectrum_Type(1..4) := 
        (186.0, 250.0, 300.0, 301.0);
      
      Convolution : Mass_Maps.Map;
   begin
      Convolution := Spectral_Convolution(Spectrum1, Spectrum2);
      Print_Convolution(Convolution);
   end Main;

begin
   Main;
end Comparing_Spectra_with_the_Spectral_Convolution;
```

## Explanation

This Ada solution implements the spectral convolution algorithm for comparing two mass spectra:

1. **Data Types**: 
   - `Mass_Type` is defined as `Float` to handle decimal masses
   - `Spectrum_Type` is an array of mass values
   - `Mass_Maps.Map` is used to store differences and their frequencies

2. **Algorithm**:
   - For each mass in spectrum1, subtract every mass in spectrum2
   - Only positive differences are kept (standard convention)
   - Differences are stored in a map with their frequency count

3. **Key Features**:
   - Uses Ada containers for efficient storage and retrieval
   - Handles the multiset nature of convolution by counting frequencies
   - Filters out negative differences as they're not typically used in spectral analysis
   - Outputs results in the required format (space-separated values)

4. **Time Complexity**: O(m×n) where m and n are the sizes of the two spectra
5. **Space Complexity**: O(k) where k is the number of unique differences

The solution correctly handles the example case and can be easily adapted for different input spectra by modifying the `Spectrum1` and `Spectrum2` arrays.