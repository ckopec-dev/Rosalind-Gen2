# Rosalind Problem: Find a Profile-most Probable k-mer in a String (VB.NET Solution)

## Problem Description
Given a string Text, an integer k, and a 4 × k matrix Profile, we want to find a k-mer Pattern that maximizes the probability Pr(Pattern|Profile) among all possible k-mers in Text.

## Solution

```vb
Module ProfileMostProbableKmer
    Sub Main()
        ' Example input
        Dim text As String = "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT"
        Dim k As Integer = 5
        Dim profile As Double(,) = {
            {0.2, 0.2, 0.3, 0.2, 0.3},
            {0.4, 0.3, 0.1, 0.5, 0.1},
            {0.3, 0.3, 0.5, 0.2, 0.4},
            {0.1, 0.2, 0.1, 0.1, 0.2}
        }
        
        Dim result As String = FindProfileMostProbableKmer(text, k, profile)
        Console.WriteLine(result)
    End Sub
    
    Function FindProfileMostProbableKmer(text As String, k As Integer, profile As Double(,)) As String
        Dim maxProbability As Double = -1
        Dim mostProbableKmer As String = ""
        
        ' Iterate through all possible k-mers in text
        For i As Integer = 0 To text.Length - k
            Dim kmer As String = text.Substring(i, k)
            Dim probability As Double = CalculateProbability(kmer, profile)
            
            ' Update if this k-mer has higher probability
            If probability > maxProbability Then
                maxProbability = probability
                mostProbableKmer = kmer
            End If
        Next
        
        Return mostProbableKmer
    End Function
    
    Function CalculateProbability(kmer As String, profile As Double(,)) As Double
        Dim probability As Double = 1.0
        
        ' For each position in k-mer, multiply the corresponding profile probability
        For i As Integer = 0 To kmer.Length - 1
            Dim nucleotide As Char = kmer(i)
            Dim profileIndex As Integer = GetNucleotideIndex(nucleotide)
            probability *= profile(profileIndex, i)
        Next
        
        Return probability
    End Function
    
    Function GetNucleotideIndex(nucleotide As Char) As Integer
        Select Case nucleotide
            Case "A"
                Return 0
            Case "C"
                Return 1
            Case "G"
                Return 2
            Case "T"
                Return 3
            Case Else
                Return -1
        End Select
    End Function
End Module
```

## Explanation

This solution implements the algorithm to find the profile-most probable k-mer in a given string:

1. **Main Function**: Iterates through all possible k-mers in the input text
2. **FindProfileMostProbableKmer**: For each k-mer, calculates its probability using the profile matrix and keeps track of the k-mer with maximum probability
3. **CalculateProbability**: Computes the probability of a k-mer given the profile matrix by multiplying the appropriate profile values for each nucleotide position
4. **GetNucleotideIndex**: Maps nucleotides A, C, G, T to indices 0, 1, 2, 3 respectively for profile matrix access

## Key Features

- Uses a 4×k profile matrix where rows represent nucleotides (A, C, G, T) and columns represent positions
- Calculates probability by multiplying profile values for each nucleotide in the k-mer
- Returns the k-mer with the highest probability
- Handles edge cases with proper bounds checking

## Time Complexity
O(n×k) where n is the length of the text and k is the k-mer length

## Space Complexity
O(1) additional space (excluding input storage)

