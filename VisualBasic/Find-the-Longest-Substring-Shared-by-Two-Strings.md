# Find the Longest Substring Shared by Two Strings - Visual Basic Solution

Here's a solution to the Rosalind problem "Find the Longest Substring Shared by Two Strings" using Visual Basic:

```vb
Module LongestCommonSubstring
    Sub Main()
        ' Example input strings
        Dim str1 As String = "ABABC"
        Dim str2 As String = "BABCA"
        
        ' Find the longest common substring
        Dim result As String = FindLongestCommonSubstring(str1, str2)
        
        Console.WriteLine("Longest Common Substring: " & result)
        Console.WriteLine("Length: " & result.Length)
    End Sub
    
    Function FindLongestCommonSubstring(str1 As String, str2 As String) As String
        Dim m As Integer = str1.Length
        Dim n As Integer = str2.Length
        
        ' Create a 2D array to store lengths of common substrings
        Dim dp(m, n) As Integer
        
        Dim maxLength As Integer = 0
        Dim endingIndex As Integer = 0
        
        ' Fill the dp table
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                If str1(i - 1) = str2(j - 1) Then
                    dp(i, j) = dp(i - 1, j - 1) + 1
                    
                    If dp(i, j) > maxLength Then
                        maxLength = dp(i, j)
                        endingIndex = i
                    End If
                Else
                    dp(i, j) = 0
                End If
            Next
        Next
        
        ' Extract the longest common substring
        If maxLength = 0 Then
            Return ""
        Else
            Return str1.Substring(endingIndex - maxLength, maxLength)
        End If
    End Function
End Module
```

## Explanation

This solution uses dynamic programming to find the longest common substring:

1. **Dynamic Programming Table**: Create a 2D array `dp` where `dp(i,j)` represents the length of the common substring ending at position `i-1` in `str1` and position `j-1` in `str2`.

2. **Fill the Table**: 
   - If characters match: `dp(i,j) = dp(i-1,j-1) + 1`
   - If characters don't match: `dp(i,j) = 0`

3. **Track Maximum**: Keep track of the maximum length found and the ending position.

4. **Extract Result**: Use the ending position and maximum length to extract the actual substring.

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Output
For input strings "ABABC" and "BABCA":
```
Longest Common Substring: BABC
Length: 4
```

## Alternative Approach (More Memory Efficient)

```vb
Function FindLongestCommonSubstringOptimized(str1 As String, str2 As String) As String
    Dim m As Integer = str1.Length
    Dim n As Integer = str2.Length
    
    Dim prev(n) As Integer
    Dim curr(n) As Integer
    
    Dim maxLength As Integer = 0
    Dim endingIndex As Integer = 0
    
    For i As Integer = 1 To m
        For j As Integer = 1 To n
            If str1(i - 1) = str2(j - 1) Then
                curr(j) = prev(j - 1) + 1
                
                If curr(j) > maxLength Then
                    maxLength = curr(j)
                    endingIndex = i
                End If
            Else
                curr(j) = 0
            End If
        Next
        
        ' Copy current to previous
        Array.Copy(curr, prev, n + 1)
    Next
    
    If maxLength = 0 Then
        Return ""
    Else
        Return str1.Substring(endingIndex - maxLength, maxLength)
    End If
End Function
```

This optimized version uses only O(n) space instead of O(m × n) by using two 1D arrays instead of a 2D array.

