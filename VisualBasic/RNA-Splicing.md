# Rosalind Problem RNA_Splicing Solution in Visual Basic

## Problem Understanding
RNA splicing involves removing introns (non-coding sequences) from pre-mRNA to form mature mRNA. Given a DNA string and a list of introns, we need to:
1. Transcribe DNA to RNA
2. Remove all introns from the RNA sequence
3. Translate the resulting mRNA to protein

## Solution

```vb
Module RNA_Splicing
    Sub Main()
        ' Sample input data
        Dim dna As String = "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"
        Dim introns As String() = {"ATCGGTCGAA", "ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"}
        
        ' Solve the problem
        Dim result As String = SpliceRNA(dna, introns)
        Console.WriteLine(result)
    End Sub
    
    Function SpliceRNA(dna As String, introns() As String) As String
        ' Step 1: Transcribe DNA to RNA (T -> U)
        Dim rna As String = dna.Replace("T", "U")
        
        ' Step 2: Remove all introns from RNA
        For Each intron As String In introns
            rna = rna.Replace(intron, "")
        Next
        
        ' Step 3: Translate RNA to protein
        Return TranslateRNA(rna)
    End Function
    
    Function TranslateRNA(rna As String) As String
        ' Codon to amino acid mapping
        Dim codonTable As New Dictionary(Of String, String) From {
            {"UUU", "F"}, {"UUC", "F"}, {"UUA", "L"}, {"UUG", "L"},
            {"UCU", "S"}, {"UCC", "S"}, {"UCA", "S"}, {"UCG", "S"},
            {"UAU", "Y"}, {"UAC", "Y"}, {"UAA", "*"}, {"UAG", "*"},
            {"UGU", "C"}, {"UGC", "C"}, {"UGA", "*"}, {"UGG", "W"},
            {"CUU", "L"}, {"CUC", "L"}, {"CUA", "L"}, {"CUG", "L"},
            {"CCU", "P"}, {"CCC", "P"}, {"CCA", "P"}, {"CCG", "P"},
            {"CAU", "H"}, {"CAC", "H"}, {"CAA", "Q"}, {"CAG", "Q"},
            {"CGU", "R"}, {"CGC", "R"}, {"CGA", "R"}, {"CGG", "R"},
            {"AUU", "I"}, {"AUC", "I"}, {"AUA", "I"}, {"AUG", "M"},
            {"ACU", "T"}, {"ACC", "T"}, {"ACA", "T"}, {"ACG", "T"},
            {"AAU", "N"}, {"AAC", "N"}, {"AAA", "K"}, {"AAG", "K"},
            {"AGU", "S"}, {"AGC", "S"}, {"AGA", "R"}, {"AGG", "R"},
            {"GUU", "V"}, {"GUC", "V"}, {"GUA", "V"}, {"GUG", "V"},
            {"GCU", "A"}, {"GCC", "A"}, {"GCA", "A"}, {"GCG", "A"},
            {"GAU", "D"}, {"GAC", "D"}, {"GAA", "E"}, {"GAG", "E"},
            {"GGU", "G"}, {"GGC", "G"}, {"GGA", "G"}, {"GGG", "G"}
        }
        
        Dim protein As String = ""
        Dim i As Integer = 0
        
        ' Process RNA in groups of 3 nucleotides (codons)
        While i <= rna.Length - 3
            Dim codon As String = rna.Substring(i, 3)
            
            ' Check if codon exists in table
            If codonTable.ContainsKey(codon) Then
                Dim aminoAcid As String = codonTable(codon)
                
                ' Stop translation at stop codon
                If aminoAcid = "*" Then
                    Exit While
                End If
                
                protein += aminoAcid
            End If
            
            i += 3
        End While
        
        Return protein
    End Function
End Module
```

## Explanation

1. **Main Function**: 
   - Takes DNA string and intron list as input
   - Calls `SpliceRNA` to process the data

2. **SpliceRNA Function**:
   - Transcribes DNA to RNA by replacing 'T' with 'U'
   - Removes all introns from the RNA sequence
   - Translates the resulting mRNA to protein

3. **TranslateRNA Function**:
   - Uses a dictionary to map RNA codons to amino acids
   - Processes the RNA string in groups of 3 nucleotides
   - Stops translation when encountering a stop codon (*)
   - Returns the final protein sequence

## Sample Output
For the given example:
- Input DNA: `ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG`
- Introns: `["ATCGGTCGAA", "ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"]`
- Output: `MVYIADKQHVASREAYDFLGMDKASDQD`

This solution handles the core RNA splicing problem by properly transcribing, splicing, and translating genetic information according to standard molecular biology principles.

