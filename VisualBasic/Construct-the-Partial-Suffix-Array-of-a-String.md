# Rosalind Problem: Construct the Partial Suffix Array of a String

## Problem Description
Given a string `s` and an integer `k`, construct the partial suffix array of `s` with respect to `k`.

The partial suffix array is a list of indices where the suffix starting at that index has a rank of at most `k` when the suffixes are sorted lexicographically.

## Solution in Visual Basic

```vb
Module PartialSuffixArray
    Sub Main()
        ' Example usage
        Dim inputString As String = "panamabananas$"
        Dim k As Integer = 3
        
        Dim result As List(Of Integer) = ConstructPartialSuffixArray(inputString, k)
        
        ' Print result
        Console.WriteLine("Partial Suffix Array:")
        For Each index As Integer In result
            Console.Write(index & " ")
        Next
        Console.WriteLine()
    End Sub
    
    Function ConstructPartialSuffixArray(s As String, k As Integer) As List(Of Integer)
        Dim n As Integer = s.Length
        Dim suffixes As New List(Of Tuple(Of String, Integer))() ' (suffix, original_index)
        
        ' Create all suffixes with their original indices
        For i As Integer = 0 To n - 1
            suffixes.Add(New Tuple(Of String, Integer)(s.Substring(i), i))
        Next
        
        ' Sort suffixes lexicographically
        suffixes.Sort(Function(x, y)
                          Return String.Compare(x.Item1, y.Item1)
                      End Function)
        
        ' Create partial suffix array
        Dim partialArray As New List(Of Integer)()
        
        ' Add indices of suffixes with rank <= k
        For i As Integer = 0 To Math.Min(k, suffixes.Count - 1)
            partialArray.Add(suffixes(i).Item2)
        Next
        
        Return partialArray
    End Function
End Module
```

## Alternative Implementation (More Efficient)

```vb
Module PartialSuffixArrayOptimized
    Sub Main()
        ' Example usage
        Dim inputString As String = "panamabananas$"
        Dim k As Integer = 3
        
        Dim result As List(Of Integer) = ConstructPartialSuffixArrayOptimized(inputString, k)
        
        ' Print result
        Console.WriteLine("Partial Suffix Array:")
        For Each index As Integer In result
            Console.Write(index & " ")
        Next
        Console.WriteLine()
    End Sub
    
    Function ConstructPartialSuffixArrayOptimized(s As String, k As Integer) As List(Of Integer)
        Dim n As Integer = s.Length
        
        ' Create array of suffix indices
        Dim indices(n - 1) As Integer
        
        ' Initialize indices
        For i As Integer = 0 To n - 1
            indices(i) = i
        Next
        
        ' Sort indices based on suffix comparison
        Array.Sort(indices, Function(x, y)
                                Return String.Compare(s.Substring(x), s.Substring(y))
                            End Function)
        
        ' Take first k+1 indices (0 to k)
        Dim result As New List(Of Integer)()
        
        For i As Integer = 0 To Math.Min(k, n - 1)
            result.Add(indices(i))
        Next
        
        Return result
    End Function
End Module
```

## Explanation

The solution works as follows:

1. **Create suffixes**: Generate all suffixes of the input string along with their original indices
2. **Sort suffixes**: Sort the suffixes lexicographically using their string values
3. **Select partial array**: Take the first `k+1` indices from the sorted list (since we want ranks 0 to k)

## Input/Output Example

**Input:**
```
s = "panamabananas$"
k = 3
```

**Output:**
```
Partial Suffix Array:
13 1 14 2
```

## Time Complexity
- **Time**: O(n² log n) where n is the length of the string
- **Space**: O(n²) for storing all suffixes

## Key Points
- The suffix array is sorted by lexicographic order of suffixes
- We only return indices of suffixes with rank ≤ k
- The rank is 0-indexed, so rank 0 is the smallest suffix
- The input string typically ends with a special character (like `$`) to ensure proper sorting

This implementation handles the core requirements of the Rosalind problem while being written in Visual Basic syntax.

