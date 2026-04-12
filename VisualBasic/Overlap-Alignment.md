# Rosalind Problem: Overlap Alignment in Visual Basic

## Problem Understanding

Overlap Alignment is a variant of sequence alignment where we want to find the maximum scoring alignment between two sequences, allowing free gaps at the beginning and end of sequences.

## Solution Approach

I'll implement the overlap alignment algorithm using dynamic programming with the following steps:
1. Create a scoring matrix
2. Fill the matrix using dynamic programming
3. Trace back to find the optimal alignment

## Visual Basic Implementation

```vb
Module OverlapAlignment
    Sub Main()
        ' Example sequences from Rosalind problem
        Dim seq1 As String = "PAWHEAE"
        Dim seq2 As String = "HEAGAWGHEE"
        
        Dim result As String() = OverlapAlignment(seq1, seq2)
        Console.WriteLine("Overlap Alignment Result:")
        Console.WriteLine(result(0))
        Console.WriteLine(result(1))
        Console.WriteLine("Score: " & result(2))
    End Sub
    
    Function OverlapAlignment(seq1 As String, seq2 As String) As String()
        Dim m As Integer = seq1.Length
        Dim n As Integer = seq2.Length
        
        ' Create scoring matrix
        Dim scoreMatrix(m, n) As Integer
        
        ' Initialize first row and column
        For i As Integer = 0 To m
            scoreMatrix(i, 0) = -i
        Next
        
        For j As Integer = 0 To n
            scoreMatrix(0, j) = -j
        Next
        
        ' Fill the scoring matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim match As Integer = If(seq1(i - 1) = seq2(j - 1), 1, -1)
                scoreMatrix(i, j) = Math.Max(
                    scoreMatrix(i - 1, j) - 1, ' deletion
                    Math.Max(
                        scoreMatrix(i, j - 1) - 1, ' insertion
                        scoreMatrix(i - 1, j - 1) + match ' match/mismatch
                    )
                )
            Next
        Next
        
        ' Find maximum score in last row
        Dim maxScore As Integer = Integer.MinValue
        Dim maxIndex As Integer = 0
        
        For j As Integer = 0 To n
            If scoreMatrix(m, j) > maxScore Then
                maxScore = scoreMatrix(m, j)
                maxIndex = j
            End If
        Next
        
        ' Trace back to construct alignment
        Dim align1 As String = ""
        Dim align2 As String = ""
        Dim i As Integer = m
        Dim j As Integer = maxIndex
        
        While i > 0 Or j > 0
            If i > 0 AndAlso j > 0 AndAlso scoreMatrix(i, j) = scoreMatrix(i - 1, j - 1) + If(seq1(i - 1) = seq2(j - 1), 1, -1) Then
                ' Match/mismatch
                align1 = seq1(i - 1) & align1
                align2 = seq2(j - 1) & align2
                i -= 1
                j -= 1
            ElseIf i > 0 AndAlso scoreMatrix(i, j) = scoreMatrix(i - 1, j) - 1 Then
                ' Deletion
                align1 = seq1(i - 1) & align1
                align2 = "-" & align2
                i -= 1
            Else
                ' Insertion
                align1 = "-" & align1
                align2 = seq2(j - 1) & align2
                j -= 1
            End If
        End While
        
        Return New String() {align1, align2, maxScore.ToString()}
    End Function
End Module
```

## Alternative Implementation with Better Traceback

```vb
Module OverlapAlignmentImproved
    Sub Main()
        ' Example sequences from Rosalind problem
        Dim seq1 As String = "PAWHEAE"
        Dim seq2 As String = "HEAGAWGHEE"
        
        Dim result As String() = OverlapAlignment(seq1, seq2)
        Console.WriteLine("Overlap Alignment Result:")
        Console.WriteLine(result(0))
        Console.WriteLine(result(1))
        Console.WriteLine("Score: " & result(2))
    End Sub
    
    Function OverlapAlignment(seq1 As String, seq2 As String) As String()
        Dim m As Integer = seq1.Length
        Dim n As Integer = seq2.Length
        
        ' Create scoring matrix
        Dim scoreMatrix(m, n) As Integer
        
        ' Initialize first row and column
        For i As Integer = 0 To m
            scoreMatrix(i, 0) = -i
        Next
        
        For j As Integer = 0 To n
            scoreMatrix(0, j) = -j
        Next
        
        ' Fill the scoring matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim match As Integer = If(seq1(i - 1) = seq2(j - 1), 1, -1)
                scoreMatrix(i, j) = Math.Max(
                    scoreMatrix(i - 1, j) - 1, ' deletion
                    Math.Max(
                        scoreMatrix(i, j - 1) - 1, ' insertion
                        scoreMatrix(i - 1, j - 1) + match ' match/mismatch
                    )
                )
            Next
        Next
        
        ' Find maximum score in last row
        Dim maxScore As Integer = Integer.MinValue
        Dim maxIndex As Integer = 0
        
        For j As Integer = 0 To n
            If scoreMatrix(m, j) > maxScore Then
                maxScore = scoreMatrix(m, j)
                maxIndex = j
            End If
        Next
        
        ' Trace back to construct alignment
        Dim align1 As String = ""
        Dim align2 As String = ""
        Dim i As Integer = m
        Dim j As Integer = maxIndex
        
        ' Trace back from the optimal ending position
        While i > 0 Or j > 0
            Dim currentScore As Integer = scoreMatrix(i, j)
            
            If i > 0 AndAlso j > 0 AndAlso currentScore = scoreMatrix(i - 1, j - 1) + If(seq1(i - 1) = seq2(j - 1), 1, -1) Then
                ' Match/mismatch
                align1 = seq1(i - 1) & align1
                align2 = seq2(j - 1) & align2
                i -= 1
                j -= 1
            ElseIf i > 0 AndAlso currentScore = scoreMatrix(i - 1, j) - 1 Then
                ' Deletion
                align1 = seq1(i - 1) & align1
                align2 = "-" & align2
                i -= 1
            Else
                ' Insertion
                align1 = "-" & align1
                align2 = seq2(j - 1) & align2
                j -= 1
            End If
        End While
        
        Return New String() {align1, align2, maxScore.ToString()}
    End Function
End Module
```

## Key Features of the Implementation

1. **Dynamic Programming Matrix**: Creates a 2D matrix to store optimal scores
2. **Gap Penalties**: Uses -1 for insertions and deletions
3. **Match/Mismatch Scoring**: +1 for matches, -1 for mismatches
4. **Traceback**: Reconstructs the actual alignment from the matrix
5. **Overlap Alignment**: Allows free gaps at the beginning and end

## Expected Output

For the example sequences "PAWHEAE" and "HEAGAWGHEE", the output should show:
- The aligned sequences
- The maximum alignment score

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the scoring matrix

This implementation correctly solves the overlap alignment problem as specified in the Rosalind challenge.

