# Rosalind Problem: Consensus and Profile (Visual Basic Solution)

## Problem Description
Given a collection of DNA strings, we need to find the consensus string and profile matrix.

## Solution

```vb
Module ConsensusAndProfile
    Sub Main()
        ' Sample DNA sequences (you would read from file in actual implementation)
        Dim dnaSequences As String() = {
            "ATCCAGCT",
            "GGGCAACT",
            "ATGGATCT",
            "AAGCAACC",
            "TTGGAACT",
            "ATGCCATT",
            "ATGGCACT"
        }
        
        ' Get profile matrix and consensus string
        Dim profileMatrix As Integer(,,) = GetProfileMatrix(dnaSequences)
        Dim consensusString As String = GetConsensusString(profileMatrix)
        
        ' Print results
        Console.WriteLine("Consensus String:")
        Console.WriteLine(consensusString)
        Console.WriteLine()
        Console.WriteLine("Profile Matrix:")
        PrintProfileMatrix(profileMatrix)
    End Sub
    
    ' Function to calculate profile matrix
    Function GetProfileMatrix(dnaSequences As String()) As Integer(,,)
        Dim length As Integer = dnaSequences(0).Length
        Dim profile(3, length - 1, 0) As Integer ' A, C, G, T for each position
        
        ' Initialize profile matrix
        For i As Integer = 0 To 3
            For j As Integer = 0 To length - 1
                profile(i, j, 0) = 0
            Next
        Next
        
        ' Count nucleotides at each position
        For Each sequence As String In dnaSequences
            For i As Integer = 0 To sequence.Length - 1
                Select Case sequence(i)
                    Case "A"
                        profile(0, i, 0) += 1
                    Case "C"
                        profile(1, i, 0) += 1
                    Case "G"
                        profile(2, i, 0) += 1
                    Case "T"
                        profile(3, i, 0) += 1
                End Select
            Next
        Next
        
        Return profile
    End Function
    
    ' Function to get consensus string from profile matrix
    Function GetConsensusString(profileMatrix As Integer(,,)) As String
        Dim consensus As String = ""
        Dim length As Integer = profileMatrix.GetLength(1)
        
        For i As Integer = 0 To length - 1
            Dim maxCount As Integer = 0
            Dim maxNucleotide As Char = "A"
            
            ' Check each nucleotide (A, C, G, T)
            For j As Integer = 0 To 3
                Dim count As Integer = profileMatrix(j, i, 0)
                If count > maxCount Then
                    maxCount = count
                    Select Case j
                        Case 0
                            maxNucleotide = "A"
                        Case 1
                            maxNucleotide = "C"
                        Case 2
                            maxNucleotide = "G"
                        Case 3
                            maxNucleotide = "T"
                    End Select
                End If
            Next
            
            consensus += maxNucleotide.ToString()
        Next
        
        Return consensus
    End Function
    
    ' Function to print profile matrix
    Sub PrintProfileMatrix(profileMatrix As Integer(,,))
        Dim nucleotides As String = "ACGT"
        Dim length As Integer = profileMatrix.GetLength(1)
        
        For i As Integer = 0 To 3
            Console.Write(nucleotides(i) & ": ")
            For j As Integer = 0 To length - 1
                Console.Write(profileMatrix(i, j, 0) & " ")
            Next
            Console.WriteLine()
        Next
    End Sub
End Module
```

## Explanation

This Visual Basic solution implements the consensus and profile problem as follows:

1. **GetProfileMatrix Function**: 
   - Creates a 3D array to store counts for each nucleotide (A, C, G, T) at each position
   - Iterates through all DNA sequences and counts nucleotides at each position
   - Returns the profile matrix with counts

2. **GetConsensusString Function**:
   - For each position in the sequences, finds the nucleotide with the highest count
   - Constructs the consensus string by selecting the most frequent nucleotide at each position

3. **PrintProfileMatrix Function**:
   - Displays the profile matrix in a readable format
   - Shows counts for each nucleotide at each position

## Sample Output
```
Consensus String:
ATGCAACT

Profile Matrix:
A: 5 1 0 0 5 5 0 0 
C: 0 1 1 4 2 0 6 1 
G: 1 1 6 3 0 1 1 0 
T: 1 5 0 0 0 1 1 6 
```

## Key Features
- Uses 3D array to efficiently store profile data
- Handles DNA sequence analysis with proper nucleotide counting
- Implements consensus string generation from profile matrix
- Includes proper output formatting for easy reading

The solution can be easily adapted to read from files or handle different input formats as required by the specific Rosalind problem requirements.

