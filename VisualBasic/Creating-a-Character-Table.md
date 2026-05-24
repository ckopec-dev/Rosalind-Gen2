# Rosalind Problem: Creating a Character Table (Visual Basic Solution)

## Problem Understanding

The task is to create a character table from a set of DNA sequences. A character table represents which characters (nucleotides) appear at each position in the sequences, typically using binary representation where 1 indicates presence and 0 indicates absence of a character.

## Solution in Visual Basic

```vb
Module CharacterTable
    Sub Main()
        ' Sample input sequences
        Dim sequences As String() = {
            "ATGCGATCG",
            "ATGCGATCG",
            "ATGCGATCG",
            "ATGCGATCG"
        }
        
        ' Generate character table
        Dim characterTable As String() = GenerateCharacterTable(sequences)
        
        ' Print results
        For Each row As String In characterTable
            Console.WriteLine(row)
        Next
    End Sub
    
    Function GenerateCharacterTable(sequences As String()) As String()
        If sequences Is Nothing OrElse sequences.Length = 0 Then
            Return New String() {}
        End If
        
        Dim numSequences As Integer = sequences.Length
        Dim sequenceLength As Integer = sequences(0).Length
        Dim characterTable As New List(Of String)
        
        ' Process each position (column) in the sequences
        For col As Integer = 0 To sequenceLength - 1
            Dim row As New StringBuilder()
            
            ' For each sequence, check if character at position col is present
            For seqIndex As Integer = 0 To numSequences - 1
                ' Simple binary representation:
                ' 1 if character exists at this position in this sequence
                ' 0 if character is different
                If sequences(seqIndex)(col) = sequences(0)(col) Then
                    row.Append("1")
                Else
                    row.Append("0")
                End If
            Next
            
            characterTable.Add(row.ToString())
        Next
        
        Return characterTable.ToArray()
    End Function
    
    ' Alternative implementation for more complex character table
    Function GenerateAdvancedCharacterTable(sequences As String()) As String()
        If sequences Is Nothing OrElse sequences.Length = 0 Then
            Return New String() {}
        End If
        
        Dim numSequences As Integer = sequences.Length
        Dim sequenceLength As Integer = sequences(0).Length
        Dim characterTable As New List(Of String)
        
        ' Create a mapping of unique characters to indices
        Dim charMap As New Dictionary(Of Char, Integer)
        Dim charIndex As Integer = 0
        
        ' Collect all unique characters from all positions
        For i As Integer = 0 To sequenceLength - 1
            For j As Integer = 0 To numSequences - 1
                Dim ch As Char = sequences(j)(i)
                If Not charMap.ContainsKey(ch) Then
                    charMap(ch) = charIndex
                    charIndex += 1
                End If
            Next
        Next
        
        ' Generate table for each position
        For col As Integer = 0 To sequenceLength - 1
            Dim row As New StringBuilder()
            
            ' For each unique character, check if it appears at this position
            For Each kvp As KeyValuePair(Of Char, Integer) In charMap
                Dim ch As Char = kvp.Key
                Dim found As Boolean = False
                
                For seqIndex As Integer = 0 To numSequences - 1
                    If sequences(seqIndex)(col) = ch Then
                        found = True
                        Exit For
                    End If
                Next
                
                If found Then
                    row.Append("1")
                Else
                    row.Append("0")
                End If
            Next
            
            characterTable.Add(row.ToString())
        Next
        
        Return characterTable.ToArray()
    End Function
End Module
```

## Alternative Approach for Rosalind Specific Input

```vb
Module RosalindCharacterTable
    Sub Main()
        ' Read input from file or standard input
        Dim inputLines As String() = {
            "ATCG",
            "ATCG",
            "ATCG",
            "ATCG"
        }
        
        Dim result As String() = CreateCharacterTable(inputLines)
        
        For Each line As String In result
            Console.WriteLine(line)
        Next
    End Sub
    
    Function CreateCharacterTable(sequences As String()) As String()
        Dim numSequences As Integer = sequences.Length
        Dim seqLength As Integer = sequences(0).Length
        Dim table As New List(Of String)
        
        ' For each position in the sequences
        For pos As Integer = 0 To seqLength - 1
            Dim row As New StringBuilder()
            
            ' For each unique character in the alphabet
            Dim uniqueChars As New HashSet(Of Char)
            
            ' Collect all characters at this position
            For i As Integer = 0 To numSequences - 1
                uniqueChars.Add(sequences(i)(pos))
            Next
            
            ' Create binary representation for each character
            Dim sortedChars As String() = uniqueChars.ToArray().OrderBy(Function(c) c).ToArray()
            
            For Each ch As Char In sortedChars
                Dim found As Boolean = False
                For i As Integer = 0 To numSequences - 1
                    If sequences(i)(pos) = ch Then
                        found = True
                        Exit For
                    End If
                Next
                If found Then
                    row.Append("1")
                Else
                    row.Append("0")
                End If
            Next
            
            table.Add(row.ToString())
        Next
        
        Return table.ToArray()
    End Function
End Module
```

## Key Points

1. **Input Processing**: The solution handles DNA sequences as strings
2. **Character Table Generation**: Creates binary representation for each position
3. **Output Format**: Each row represents a position, with 1s and 0s indicating character presence
4. **Flexibility**: Can be adapted for different character sets and requirements

## Time Complexity
- O(n × m) where n is the number of sequences and m is the sequence length

## Space Complexity  
- O(n × m) for storing the character table

This solution provides a foundation that can be adapted for the specific Rosalind problem requirements, which typically involve reading sequences from a file and generating the appropriate binary character table format.

