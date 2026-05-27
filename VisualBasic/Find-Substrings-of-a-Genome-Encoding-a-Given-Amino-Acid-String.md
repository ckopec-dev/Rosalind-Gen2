# Rosalind Problem: Find_Substrings_of_a_Genome_Encoding_a_Given_Amino_Acid_String

## Problem Description
Given a DNA string and an amino acid string, find all substrings of the DNA string that encode the amino acid string.

## Solution in Visual Basic

```vb
Module FindSubstrings
    Sub Main()
        ' Example input
        Dim dna As String = "ATGGCCATGGCCCCC"
        Dim aminoAcid As String = "MA"
        
        ' Find all substrings
        Dim result As List(Of String) = FindEncodingSubstrings(dna, aminoAcid)
        
        ' Print results
        For Each substring As String In result
            Console.WriteLine(substring)
        Next
    End Sub
    
    Function FindEncodingSubstrings(dna As String, aminoAcid As String) As List(Of String)
        Dim result As New List(Of String)
        Dim codonTable As Dictionary(Of String, Char) = GetCodonTable()
        
        ' Convert amino acid string to codons
        Dim requiredCodons As List(Of String) = New List(Of String)
        For i As Integer = 0 To aminoAcid.Length - 1
            Dim amino As Char = aminoAcid(i)
            For Each kvp As KeyValuePair(Of String, Char) In codonTable
                If kvp.Value = amino Then
                    requiredCodons.Add(kvp.Key)
                    Exit For
                End If
            Next
        Next
        
        ' Find all possible start positions
        Dim codonLength As Integer = 3
        Dim totalLength As Integer = aminoAcid.Length * codonLength
        
        For i As Integer = 0 To dna.Length - totalLength
            Dim substring As String = dna.Substring(i, totalLength)
            If IsEncodingSubstring(substring, aminoAcid, codonTable) Then
                result.Add(substring)
            End If
        Next
        
        Return result
    End Function
    
    Function IsEncodingSubstring(dnaSubstring As String, aminoAcid As String, codonTable As Dictionary(Of String, Char)) As Boolean
        Dim codonLength As Integer = 3
        Dim expectedAminoAcids As String = ""
        
        For i As Integer = 0 To dnaSubstring.Length - codonLength Step codonLength
            Dim codon As String = dnaSubstring.Substring(i, codonLength)
            If codonTable.ContainsKey(codon) Then
                expectedAminoAcids += codonTable(codon)
            Else
                Return False ' Invalid codon
            End If
        Next
        
        Return expectedAminoAcids = aminoAcid
    End Function
    
    Function GetCodonTable() As Dictionary(Of String, Char)
        Dim codonTable As New Dictionary(Of String, Char)
        
        ' Standard genetic code
        codonTable.Add("UUU", "F") : codonTable.Add("UUC", "F")
        codonTable.Add("UUA", "L") : codonTable.Add("UUG", "L")
        codonTable.Add("CUU", "L") : codonTable.Add("CUC", "L")
        codonTable.Add("CUA", "L") : codonTable.Add("CUG", "L")
        codonTable.Add("AUU", "I") : codonTable.Add("AUC", "I")
        codonTable.Add("AUA", "I")
        codonTable.Add("GUU", "V") : codonTable.Add("GUC", "V")
        codonTable.Add("GUA", "V") : codonTable.Add("GUG", "V")
        codonTable.Add("UCU", "S") : codonTable.Add("UCC", "S")
        codonTable.Add("UCA", "S") : codonTable.Add("UCG", "S")
        codonTable.Add("CCU", "P") : codonTable.Add("CCC", "P")
        codonTable.Add("CCA", "P") : codonTable.Add("CCG", "P")
        codonTable.Add("ACU", "T") : codonTable.Add("ACC", "T")
        codonTable.Add("ACA", "T") : codonTable.Add("ACG", "T")
        codonTable.Add("GCU", "A") : codonTable.Add("GCC", "A")
        codonTable.Add("GCA", "A") : codonTable.Add("GCG", "A")
        codonTable.Add("UAU", "Y") : codonTable.Add("UAC", "Y")
        codonTable.Add("UGU", "C") : codonTable.Add("UGC", "C")
        codonTable.Add("UGG", "W")
        codonTable.Add("UAA", "*") : codonTable.Add("UAG", "*")
        codonTable.Add("UGA", "*")
        codonTable.Add("CAU", "H") : codonTable.Add("CAC", "H")
        codonTable.Add("CAA", "Q") : codonTable.Add("CAG", "Q")
        codonTable.Add("AAU", "N") : codonTable.Add("AAC", "N")
        codonTable.Add("AAA", "K") : codonTable.Add("AAG", "K")
        codonTable.Add("GAU", "D") : codonTable.Add("GAC", "D")
        codonTable.Add("GAA", "E") : codonTable.Add("GAG", "E")
        codonTable.Add("GGA", "G") : codonTable.Add("GGC", "G")
        codonTable.Add("GGB", "G") : codonTable.Add("GGG", "G")
        
        Return codonTable
    End Function
End Module
```

## Explanation

This Visual Basic solution works as follows:

1. **GetCodonTable()**: Creates a dictionary mapping RNA codons to their corresponding amino acids using the standard genetic code.

2. **FindEncodingSubstrings()**: 
   - Calculates the required length of DNA substring (amino acid length × 3)
   - Slides through the DNA string to find all possible substrings of that length
   - Checks if each substring encodes the target amino acid sequence

3. **IsEncodingSubstring()**: 
   - Takes a DNA substring and converts it to amino acids using the codon table
   - Compares the resulting amino acid sequence with the target sequence
   - Returns true if they match

## Example Usage
For DNA string "ATGGCCATGGCCCCC" and amino acid string "MA":
- The function will find substrings that translate to "MA"
- It returns all valid 6-character DNA substrings that encode the amino acid sequence

## Time Complexity
O(n × m) where n is the length of the DNA string and m is the length of the amino acid string.

## Space Complexity
O(1) for the codon table plus O(k) for the result list where k is the number of matching substrings.

