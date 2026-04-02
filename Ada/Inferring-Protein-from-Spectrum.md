# Rosalind Problem: Inferring Protein from Spectrum (Ada Solution)

## Problem Understanding

The problem asks us to determine the protein sequence that corresponds to a given mass spectrum. We need to find a sequence of amino acid masses that sum to the given total mass.

## Solution Approach

We'll use dynamic programming to solve this problem:
1. Create a mapping of amino acid masses to their symbols
2. Use dynamic programming to find all possible sequences that sum to the target mass
3. Return the first valid sequence found

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Inferring_Protein_From_Spectrum is
   
   -- Amino acid masses and their corresponding symbols
   type Amino_Acid is record
      Mass : Integer;
      Symbol : String(1..1);
   end record;
   
   -- Define all amino acids with their masses
   Amino_Acids : constant array(1..20) of Amino_Acid := (
      (113, "A"), (114, "C"), (128, "D"), (129, "E"), (131, "F"),
      (132, "G"), (133, "H"), (134, "I"), (135, "K"), (136, "L"),
      (137, "M"), (138, "N"), (147, "P"), (156, "Q"), (163, "R"),
      (186, "S"), (194, "T"), (202, "V"), (204, "W"), (227, "Y")
   );
   
   -- Vector to store protein sequences
   package Sequence_Vector is new Ada.Containers.Vectors(
      Index_Type => Positive,
      Element_Type => Unbounded_String
   );
   
   -- Function to check if a mass is valid (within reasonable range)
   function Is_Valid_Mass(Mass : Integer) return Boolean is
   begin
      return Mass >= 0 and Mass <= 2500;  -- Reasonable upper bound
   end Is_Valid_Mass;
   
   -- Function to find protein sequence from spectrum
   function Find_Protein_Sequence(Target_Mass : Integer) return String is
      -- Dynamic programming array to store sequences
      type Sequence_Array is array(0..2500) of Unbounded_String;
      DP : Sequence_Array := (others => Null_Unbounded_String);
      Result : Unbounded_String;
   begin
      -- Base case: empty sequence for mass 0
      DP(0) := Null_Unbounded_String;
      
      -- Fill DP array
      for Mass in 1..Target_Mass loop
         for Acid in Amino_Acids'Range loop
            if Mass >= Acid.Mass and 
               (Mass = Acid.Mass or else 
                DP(Mass - Acid.Mass) /= Null_Unbounded_String) then
               if DP(Mass) = Null_Unbounded_String then
                  DP(Mass) := Concatenate(DP(Mass - Acid.Mass), Acid.Symbol);
               else
                  -- Choose the lexicographically smaller sequence
                  declare
                     New_Sequence : Unbounded_String := 
                        Concatenate(DP(Mass - Acid.Mass), Acid.Symbol);
                  begin
                     if New_Sequence < DP(Mass) then
                        DP(Mass) := New_Sequence;
                     end if;
                  end;
               end if;
            end if;
         end loop;
      end loop;
      
      -- Return the result
      if DP(Target_Mass) /= Null_Unbounded_String then
         return To_String(DP(Target_Mass));
      else
         return "No solution found";
      end if;
   end Find_Protein_Sequence;
   
   -- Read input from stdin
   procedure Read_Input is
      Input_Line : Unbounded_String;
      Target_Mass : Integer;
   begin
      -- Read the target mass
      Get_Line(Input_Line);
      Target_Mass := Integer'Value(To_String(Input_Line));
      
      -- Find and print the protein sequence
      Put_Line(Find_Protein_Sequence(Target_Mass));
   end Read_Input;
   
begin
   Read_Input;
end Inferring_Protein_From_Spectrum;
```

## Alternative Simpler Approach

If we want a more straightforward approach with better handling of the problem constraints:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Inferring_Protein_From_Spectrum is
   
   -- Amino acid masses and symbols (standard masses)
   type Mass_Symbol is record
      Mass : Integer;
      Symbol : String(1..1);
   end record;
   
   Amino_Masses : constant array(1..20) of Mass_Symbol := (
      (113, "A"), (114, "C"), (128, "D"), (129, "E"), (131, "F"),
      (132, "G"), (133, "H"), (134, "I"), (135, "K"), (136, "L"),
      (137, "M"), (138, "N"), (147, "P"), (156, "Q"), (163, "R"),
      (186, "S"), (194, "T"), (202, "V"), (204, "W"), (227, "Y")
   );
   
   -- Simple recursive approach to find a valid sequence
   function Find_Sequence(Mass : Integer) return String is
      Result : String(1..100) := (others => ' ');
      Length : Integer := 0;
   begin
      if Mass = 0 then
         return "";
      elsif Mass < 0 then
         return "";
      end if;
      
      -- Try each amino acid
      for Acid in Amino_Masses'Range loop
         if Mass >= Acid.Mass then
            declare
               Sub_Result : constant String := Find_Sequence(Mass - Acid.Mass);
            begin
               if Sub_Result /= "" then
                  return Acid.Symbol & Sub_Result;
               end if;
            end;
         end if;
      end loop;
      
      return "";
   end Find_Sequence;
   
   Target_Mass : Integer;
   
begin
   -- Read the target mass
   Get(Target_Mass);
   
   -- Find and output the protein sequence
   Put_Line(Find_Sequence(Target_Mass));
end Inferring_Protein_From_Spectrum;
```

## Key Points

1. **Input/Output**: The program reads the target mass from standard input and outputs the protein sequence
2. **Amino Acid Mapping**: Uses standard amino acid masses with their single-letter symbols
3. **Dynamic Programming**: The first approach uses DP to efficiently find valid sequences
4. **Recursive Approach**: The second approach uses recursion with backtracking
5. **Error Handling**: Handles cases where no valid sequence exists

## Usage

Compile with:
```bash
gnatmake inferring_protein_from_spectrum.adb
```

Run with:
```bash
./inferring_protein_from_spectrum < input.txt
```

The program will output the protein sequence that sums to the given mass.

