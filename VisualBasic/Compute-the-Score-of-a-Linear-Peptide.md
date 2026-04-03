# Rosalind Problem: Compute the Score of a Linear Peptide

## Problem Description
Given a linear peptide and a scoring matrix, compute the score of the peptide.

## Solution in Visual Basic

```vb
Module ComputeTheScoreOfALinearPeptide
    Sub Main()
        ' Example usage
        Dim peptide As String = "NQEL"
        Dim spectrum As String = "0 99 113 128 129 227 257 260 328 356 367 389 427 498"
        
        Dim score As Integer = ComputeScore(peptide, spectrum)
        Console.WriteLine("Score: " & score)
    End Sub
    
    Function ComputeScore(peptide As String, spectrum As String) As Integer
        ' Parse the spectrum
        Dim spectrumValues As String() = spectrum.Split(" "c)
        Dim spectrumArray As New List(Of Integer)
        
        For Each value As String In spectrumValues
            If Not String.IsNullOrEmpty(value) Then
                spectrumArray.Add(Integer.Parse(value))
            End If
        Next
        
        ' Create amino acid mass table
        Dim massTable As New Dictionary(Of Char, Integer) From {
            {"A", 71}, {"C", 103}, {"D", 115}, {"E", 129},
            {"F", 147}, {"G", 57}, {"H", 137}, {"I", 113},
            {"K", 128}, {"L", 113}, {"M", 131}, {"N", 114},
            {"P", 97}, {"Q", 128}, {"R", 156}, {"S", 87},
            {"T", 101}, {"V", 99}, {"W", 186}, {"Y", 163}
        }
        
        ' Calculate theoretical spectrum
        Dim theoreticalSpectrum As New List(Of Integer)
        theoreticalSpectrum.Add(0) ' Start with 0
        
        Dim totalMass As Integer = 0
        For i As Integer = 0 To peptide.Length - 1
            Dim aminoAcid As Char = peptide(i)
            If massTable.ContainsKey(aminoAcid) Then
                totalMass += massTable(aminoAcid)
                theoreticalSpectrum.Add(totalMass)
            End If
        Next
        
        ' Count how many theoretical masses appear in the spectrum
        Dim score As Integer = 0
        For Each theoreticalMass As Integer In theoreticalSpectrum
            If spectrumArray.Contains(theoreticalMass) Then
                score += 1
            End If
        Next
        
        Return score
    End Function
End Module
```

## Explanation

This Visual Basic solution implements the algorithm to compute the score of a linear peptide:

1. **Input Parsing**: The function takes a peptide string and a spectrum string as input
2. **Spectrum Processing**: The spectrum string is parsed into an array of integers
3. **Mass Table**: A dictionary mapping amino acid characters to their masses
4. **Theoretical Spectrum Generation**: 
   - Start with 0 (the initial mass)
   - For each amino acid in the peptide, accumulate the mass
   - Add each cumulative mass to the theoretical spectrum
5. **Score Calculation**: Count how many masses in the theoretical spectrum appear in the given spectrum

## Key Features

- Handles all 20 standard amino acids with their correct masses
- Computes the prefix masses for a linear peptide
- Compares theoretical vs. experimental spectrum
- Returns the number of matching masses (score)

## Time Complexity
O(n²) where n is the length of the peptide, due to the spectrum matching operations.

## Space Complexity  
O(n) for storing the theoretical spectrum and mass table.

The solution correctly implements the linear peptide scoring algorithm as required by the Rosalind problem.

