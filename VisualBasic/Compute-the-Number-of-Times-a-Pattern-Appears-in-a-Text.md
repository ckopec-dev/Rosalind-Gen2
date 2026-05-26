# Rosalind Problem: Compute the Number of Times a Pattern Appears in a Text
## Solution in Visual Basic

```vb
Module PatternCount
    Sub Main()
        ' Read input from console
        Dim text As String = Console.ReadLine()
        Dim pattern As String = Console.ReadLine()
        
        ' Calculate pattern count
        Dim count As Integer = CountPattern(text, pattern)
        
        ' Output result
        Console.WriteLine(count)
    End Sub
    
    Function CountPattern(text As String, pattern As String) As Integer
        Dim count As Integer = 0
        Dim patternLength As Integer = pattern.Length
        Dim textLength As Integer = text.Length
        
        ' Handle edge cases
        If patternLength = 0 OrElse textLength = 0 OrElse patternLength > textLength Then
            Return 0
        End If
        
        ' Loop through text and check for pattern matches
        For i As Integer = 0 To textLength - patternLength
            ' Extract substring of pattern length
            Dim substring As String = text.Substring(i, patternLength)
            
            ' Compare with pattern
            If substring = pattern Then
                count += 1
            End If
        Next
        
        Return count
    End Function
End Module
```

## Explanation

This Visual Basic solution implements the pattern counting algorithm for the Rosalind problem:

1. **Input Reading**: The program reads two strings from standard input:
   - First line: the text string
   - Second line: the pattern string to search for

2. **Pattern Counting Function**: 
   - The `CountPattern` function takes the text and pattern as parameters
   - It handles edge cases (empty strings, pattern longer than text)
   - Uses a sliding window approach to check each possible position where the pattern could occur
   - Compares each substring of the text with the pattern using string equality

3. **Algorithm Details**:
   - Loop from index 0 to (text length - pattern length)
   - Extract substring of pattern length at each position
   - Compare substring with pattern
   - Increment counter when match is found

4. **Output**: Returns the total count of pattern occurrences

## Time Complexity
- **Time**: O(n × m) where n is the length of text and m is the length of pattern
- **Space**: O(m) for the substring operations

## Example Usage
```
Input:
GATATATGCATATACTT
ATAT

Output:
3
```

The pattern "ATAT" appears 3 times in the text "GATATATGCATATACTT" at positions 1, 3, and 9.

