# Rosalind Problem RNA_Splicing Solution in Ada

## Problem Understanding
RNA splicing involves removing introns (non-coding sequences) from pre-mRNA to form mature mRNA. Given a DNA string and a list of intron sequences, we need to:
1. Transcribe DNA to RNA
2. Remove all intron sequences from the RNA
3. Translate the resulting RNA to protein

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure RNA_Splicing is
   
   -- DNA to RNA transcription table
   function DNA_to_RNA(DNA_Char : Character) return Character is
   begin
      case DNA_Char is
         when 'T' => return 'U';
         when 'A' => return 'A';
         when 'G' => return 'G';
         when 'C' => return 'C';
         when others => return DNA_Char;
      end case;
   end DNA_to_RNA;
   
   -- RNA to protein translation table
   function RNA_to_Protein(RNA_Codon : String) return Character is
   begin
      case RNA_Codon is
         when "UUU" | "UUC" => return 'F';
         when "UUA" | "UUG" => return 'L';
         when "UCU" | "UCC" | "UCA" | "UCG" => return 'S';
         when "UAU" | "UAC" => return 'Y';
         when "UGU" | "UGC" => return 'C';
         when "UGG" => return 'W';
         when "CUU" | "CUC" | "CUA" | "CUG" => return 'L';
         when "CCU" | "CCC" | "CCA" | "CCG" => return 'P';
         when "CAU" | "CAC" => return 'H';
         when "CAA" | "CAG" => return 'Q';
         when "CGU" | "CGC" | "CGA" | "CGG" => return 'R';
         when "AUU" | "AUC" | "AUA" => return 'I';
         when "AUG" => return 'M';
         when "ACU" | "ACC" | "ACA" | "ACG" => return 'T';
         when "AAU" | "AAC" => return 'N';
         when "AAA" | "AAG" => return 'K';
         when "AGU" | "AGC" => return 'S';
         when "AGA" | "AGG" => return 'R';
         when "GUU" | "GUC" | "GUA" | "GUG" => return 'V';
         when "GCU" | "GCC" | "GCA" | "GCG" => return 'A';
         when "GAU" | "GAC" => return 'D';
         when "GAA" | "GAG" => return 'E';
         when "GGU" | "GGC" | "GGA" | "GGG" => return 'G';
         when "UAA" | "UAG" | "UGA" => return '*';  -- Stop codon
         when others => return '?';
      end case;
   end RNA_to_Protein;
   
   -- Remove all occurrences of a substring from a string
   function Remove_Substring(Source : Unbounded_String; 
                            Pattern : String) return Unbounded_String is
      Result : Unbounded_String := Source;
      Position : Natural;
   begin
      loop
         Position := Index(Result, Pattern);
         exit when Position = 0;
         Result := Head(Result, Position - 1) & Tail(Result, Position + Pattern'Length);
      end loop;
      return Result;
   end Remove_Substring;
   
   -- Main processing function
   function Process_RNA_Splicing(DNA_String : String; 
                               Introns : String_Array) return String is
      RNA_String : Unbounded_String;
      Result : Unbounded_String;
      Codon : String(1..3);
      Protein : Unbounded_String;
   begin
      -- Transcribe DNA to RNA
      for I in DNA_String'Range loop
         RNA_String := RNA_String & DNA_to_RNA(DNA_String(I));
      end loop;
      
      -- Remove introns
      Result := RNA_String;
      for I in Introns'Range loop
         Result := Remove_Substring(Result, Introns(I));
      end loop;
      
      -- Translate to protein
      Protein := Null_Unbounded_String;
      for I in 1..(Length(Result) / 3) loop
         Codon := To_String(Slice(Result, (I-1)*3+1, I*3));
         declare
            Amino_Acid : Character := RNA_to_Protein(Codon);
         begin
            if Amino_Acid = '*' then
               exit;  -- Stop translation at stop codon
            end if;
            Protein := Protein & Amino_Acid;
         end;
      end loop;
      
      return To_String(Protein);
   end Process_RNA_Splicing;
   
   -- Test example
   DNA_Test : constant String := "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG";
   Introns_Test : constant String_Array(1..3) := 
      (1 => "ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG",
       2 => "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG",
       3 => "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG");
   
begin
   -- Main execution
   Put_Line("RNA Splicing Solution");
   Put_Line("=====================");
   
   -- Process the example
   declare
      Result : constant String := Process_RNA_Splicing(DNA_Test, Introns_Test);
   begin
      Put_Line("Protein sequence: " & Result);
   end;
   
   -- Additional test case
   Put_Line("Additional test case:");
   Put_Line("DNA: ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG");
   Put_Line("Expected: MVYIADKQHVASREAYLFGFV");
   
end RNA_Splicing;
```

## Explanation

This Ada solution addresses the RNA splicing problem through several key components:

1. **DNA to RNA Transcription**: The `DNA_to_RNA` function converts DNA nucleotides (T→U) while keeping others unchanged.

2. **RNA to Protein Translation**: The `RNA_to_Protein` function translates RNA codons (3-letter sequences) into amino acids using the standard genetic code.

3. **Substring Removal**: The `Remove_Substring` function efficiently removes all occurrences of intron sequences from the pre-mRNA.

4. **Main Processing**: The `Process_RNA_Splicing` function orchestrates the entire workflow:
   - Transcribes DNA to RNA
   - Removes introns from the RNA
   - Translates the remaining RNA to protein sequence

5. **Stop Codon Handling**: Translation stops when encountering a stop codon (*), as required in biological translation.

The solution handles the core requirements of RNA splicing: removing non-coding introns and producing the correct protein sequence from the remaining coding regions.

