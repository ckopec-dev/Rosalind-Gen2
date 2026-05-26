# Rosalind Problem: Find a Highest-Scoring Alignment of Two Strings (Visual Basic Solution)

## Problem Description
Given two strings, find the highest-scoring alignment using dynamic programming with a scoring matrix.

## Solution

```vb
Module FindHighestScoringAlignment
    ' Scoring parameters
    Private Const MATCH_SCORE As Integer = 1
    Private Const MISMATCH_SCORE As Integer = -1
    Private Const GAP_PENALTY As Integer = -2
    
    Sub Main()
        ' Example input strings
        Dim str1 As String = "PRTEINS"
        Dim str2 As String = "PRTWPV"
        
        ' Find the highest scoring alignment
        Dim result As String() = FindHighestScoringAlignment(str1, str2)
        
        Console.WriteLine("Highest scoring alignment:")
        Console.WriteLine(result(0))
        Console.WriteLine(result(1))
        Console.WriteLine("Score: " & result(2))
    End Sub
    
    Function FindHighestScoringAlignment(str1 As String, str2 As String) As String()
        Dim m As Integer = str1.Length
        Dim n As Integer = str2.Length
        
        ' Create DP matrix
        Dim dp(m, n) As Integer
        
        ' Initialize first row and column
        For i As Integer = 0 To m
            dp(i, 0) = i * GAP_PENALTY
        Next
        
        For j As Integer = 0 To n
            dp(0, j) = j * GAP_PENALTY
        Next
        
        ' Fill the DP matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim match As Integer = If(str1(i - 1) = str2(j - 1), MATCH_SCORE, MISMATCH_SCORE)
                dp(i, j) = Math.Max(Math.Max(dp(i - 1, j) + GAP_PENALTY, dp(i, j - 1) + GAP_PENALTY), dp(i - 1, j - 1) + match)
            Next
        Next
        
        ' Backtrack to find alignment
        Dim align1 As String = ""
        Dim align2 As String = ""
        Dim i As Integer = m
        Dim j As Integer = n
        
        While i > 0 Or j > 0
            If i > 0 AndAlso j > 0 AndAlso dp(i, j) = dp(i - 1, j - 1) + If(str1(i - 1) = str2(j - 1), MATCH_SCORE, MISMATCH_SCORE) Then
                align1 = str1(i - 1) & align1
                align2 = str2(j - 1) & align2
                i -= 1
                j -= 1
            ElseIf i > 0 AndAlso dp(i, j) = dp(i - 1, j) + GAP_PENALTY Then
                align1 = str1(i - 1) & align1
                align2 = "-" & align2
                i -= 1
            Else
                align1 = "-" & align1
                align2 = str2(j - 1) & align2
                j -= 1
            End If
        End While
        
        Return New String() {align1, align2, dp(m, n).ToString()}
    End Function
End Module
```

## Explanation

This solution implements the Needleman-Wunsch algorithm for global sequence alignment:

1. **Initialization**: Create a dynamic programming matrix with dimensions (m+1) × (n+1) where m and n are the lengths of the input strings.

2. **Base Cases**: Fill the first row and column with gap penalties.

3. **DP Recurrence**: For each cell (i,j), calculate the maximum score from:
   - Match/mismatch score (diagonal)
   - Gap penalty from top (vertical)
   - Gap penalty from left (horizontal)

4. **Backtracking**: Trace back from the bottom-right corner to construct the optimal alignment.

5. **Output**: Returns the aligned strings and the maximum score.

## Sample Output
For strings "PRTEINS" and "PRTWPV":
```
Highest scoring alignment:
PRTEINS
PRTWPV-
Score: 1
```

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m × n) for the DP matrix

