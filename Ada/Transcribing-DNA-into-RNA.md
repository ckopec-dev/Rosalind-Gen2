# Rosalind Problem: Transcribing DNA into RNA (Ada Solution)

## Problem Statement
In Watson's DNA, we need to transcribe DNA into RNA by replacing each nucleotide:
- G → C
- C → G  
- T → A
- A → U

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Transcribing_DNA_into_RNA is
   function Transcribe_Nucleotide(Nucleotide : Character) return Character is
   begin
      case Nucleotide is
         when 'G' => return 'C';
         when 'C' => return 'G';
         when 'T' => return 'A';
         when 'A' => return 'U';
         when others => return Nucleotide;  -- For any other character
      end case;
   end Transcribe_Nucleotide;

   procedure Transcribe_DNA(DNA_String : in String) is
      RNA_String : Unbounded_String;
   begin
      for I in DNA_String'Range loop
         Append(RNA_String, Transcribe_Nucleotide(DNA_String(I)));
      end loop;
      
      Put_Line(To_String(RNA_String));
   end Transcribe_DNA;

   -- Example usage
   DNA_Input : constant String := "GCTAGCT";
begin
   Transcribe_DNA(DNA_Input);
end Transcribing_DNA_into_RNA;
```

## Explanation

1. **Function `Transcribe_Nucleotide`**: Takes a single DNA nucleotide character and returns its RNA complement:
   - G → C
   - C → G
   - T → A
   - A → U

2. **Procedure `Transcribe_DNA`**: 
   - Takes a DNA string as input
   - Processes each character through the transcribe function
   - Builds and outputs the RNA string

3. **Main execution**:
   - Demonstrates with example "GCTAGCT"
   - Output would be "CGAUCGA"

## Sample Input/Output
- Input: `GCTAGCT`
- Output: `CGAUCGA`

This solution handles the basic transcription requirements and can be easily modified to read from files or handle different input formats as needed for the Rosalind platform.