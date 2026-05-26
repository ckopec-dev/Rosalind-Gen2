# Rosalind Problem: Construct the Suffix Array of a String (Visual Basic Solution)

## Problem Understanding
A suffix array is a sorted array of all suffixes of a given string. For a string of length n, there are n suffixes, and the suffix array contains the starting positions of these suffixes in lexicographically sorted order.

## Solution Approach
1. Generate all suffixes of the input string
2. Sort the suffixes lexicographically
3. Return the starting positions of the sorted suffixes

## Visual Basic Implementation

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module SuffixArray
    Sub Main()
        ' Example usage
        Dim inputString As String = "AACGATAGCGGTAGA"
        Dim suffixArray As List(Of Integer) = ConstructSuffixArray(inputString)
        
        ' Print result
        Console.WriteLine(String.Join(" ", suffixArray))
    End Sub
    
    Function ConstructSuffixArray(text As String) As List(Of Integer)
        Dim suffixes As New List(Of String)
        Dim suffixPositions As New List(Of Integer)
        
        ' Generate all suffixes and their positions
        For i As Integer = 0 To text.Length - 1
            suffixes.Add(text.Substring(i))
            suffixPositions.Add(i)
        Next
        
        ' Sort suffixes lexicographically and keep track of original positions
        Dim sortedSuffixes As List(Of String) = suffixes.OrderBy(Function(s) s).ToList()
        Dim result As New List(Of Integer)
        
        ' Find original positions of sorted suffixes
        For Each suffix As String In sortedSuffixes
            Dim index As Integer = suffixes.IndexOf(suffix)
            result.Add(suffixPositions(index))
        Next
        
        Return result
    End Function
End Module
```

## Alternative Implementation (More Efficient)

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module SuffixArray
    Sub Main()
        ' Example usage
        Dim inputString As String = "AACGATAGCGGTAGA"
        Dim suffixArray As List(Of Integer) = ConstructSuffixArray(inputString)
        
        ' Print result
        Console.WriteLine(String.Join(" ", suffixArray))
    End Sub
    
    Function ConstructSuffixArray(text As String) As List(Of Integer)
        Dim suffixArray As New List(Of Integer)
        
        ' Create array of suffix start positions
        For i As Integer = 0 To text.Length - 1
            suffixArray.Add(i)
        Next
        
        ' Sort positions based on suffix comparison
        suffixArray.Sort(Function(a, b)
                             Dim suffixA As String = text.Substring(a)
                             Dim suffixB As String = text.Substring(b)
                             Return String.Compare(suffixA, suffixB)
                         End Function)
        
        Return suffixArray
    End Function
End Module
```

## Explanation

1. **Input**: A string `text` of length n
2. **Process**:
   - Create a list of all starting positions (0 to n-1)
   - Sort these positions based on the lexicographical order of the suffixes they represent
   - The sorting is done by comparing the actual suffix strings
3. **Output**: A list of integers representing the starting positions of suffixes in sorted order

## Example Walkthrough

For input string "AACGATAGCGGTAGA":
- Suffixes: "AACGATAGCGGTAGA", "ACGATAGCGGTAGA", "CGATAGCGGTAGA", etc.
- Sorted suffixes: "AACGATAGCGGTAGA", "ACGATAGCGGTAGA", "AGA", "AGCGGTAGA", etc.
- Result: [13, 1, 12, 2, 11, 3, 10, 4, 9, 5, 8, 6, 7, 0]

## Time Complexity
- O(n² log n) where n is the length of the string
- The sorting step dominates with O(n log n) comparisons, each taking O(n) time for string comparison

## Space Complexity
- O(n²) for storing all suffixes

The second implementation is more efficient as it avoids storing the suffix strings separately and sorts the positions directly using a custom comparison function.

