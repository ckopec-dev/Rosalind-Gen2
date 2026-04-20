# Rosalind Problem: Enumerating Oriented Gene Orderings (PERM)

## Problem Understanding

This problem asks us to find all possible signed permutations of length n, where each element can be either positive or negative. This is essentially finding all permutations of the set {1, 2, ..., n} where each number can appear in either positive or negative form.

## Solution Approach

1. Generate all permutations of numbers 1 to n
2. For each permutation, generate all possible sign combinations (positive/negative)
3. Output all valid signed permutations

## Visual Basic Implementation

```vb
Module EnumeratingOrientedGeneOrderings
    Sub Main()
        Dim n As Integer = 3 ' Input value - can be changed
        
        ' Generate all signed permutations
        Dim result As List(Of String) = GenerateSignedPermutations(n)
        
        ' Output the count
        Console.WriteLine(result.Count)
        
        ' Output all permutations
        For Each perm As String In result
            Console.WriteLine(perm)
        Next
    End Sub
    
    Function GenerateSignedPermutations(n As Integer) As List(Of String)
        Dim result As New List(Of String)
        Dim numbers As New List(Of Integer)
        
        ' Create list of positive numbers
        For i As Integer = 1 To n
            numbers.Add(i)
        Next
        
        ' Generate all permutations of positive numbers
        Dim permutations As List(Of List(Of Integer)) = GetPermutations(numbers)
        
        ' For each permutation, generate all sign combinations
        For Each perm As List(Of Integer) In permutations
            Dim signCombinations As List(Of List(Of Integer)) = GenerateSignCombinations(perm.Count)
            
            For Each signs As List(Of Integer) In signCombinations
                Dim signedPerm As String = ""
                For i As Integer = 0 To perm.Count - 1
                    Dim value As Integer = If(signs(i) = 1, perm(i), -perm(i))
                    If i > 0 Then signedPerm &= " "
                    signedPerm &= value.ToString()
                Next
                result.Add(signedPerm)
            Next
        Next
        
        Return result
    End Function
    
    Function GetPermutations(numbers As List(Of Integer)) As List(Of List(Of Integer))
        Dim result As New List(Of List(Of Integer))
        
        If numbers.Count = 1 Then
            result.Add(New List(Of Integer) From {numbers(0)})
            Return result
        End If
        
        For i As Integer = 0 To numbers.Count - 1
            Dim current As Integer = numbers(i)
            Dim remaining As New List(Of Integer)
            
            For j As Integer = 0 To numbers.Count - 1
                If j <> i Then remaining.Add(numbers(j))
            Next
            
            Dim subPerms As List(Of List(Of Integer)) = GetPermutations(remaining)
            
            For Each subPerm As List(Of Integer) In subPerms
                Dim newPerm As New List(Of Integer)
                newPerm.Add(current)
                newPerm.AddRange(subPerm)
                result.Add(newPerm)
            Next
        Next
        
        Return result
    End Function
    
    Function GenerateSignCombinations(n As Integer) As List(Of List(Of Integer))
        Dim result As New List(Of List(Of Integer))
        
        ' Generate all binary combinations from 0 to 2^n - 1
        For i As Integer = 0 To (2 ^ n) - 1
            Dim signs As New List(Of Integer)
            For j As Integer = 0 To n - 1
                ' Check if j-th bit is set
                Dim bit As Integer = (i >> j) And 1
                ' Convert 0 to 1, 1 to -1
                signs.Add(If(bit = 0, 1, -1))
            Next
            result.Add(signs)
        Next
        
        Return result
    End Function
End Module
```

## Example Output for n=2

```
8
1 2
1 -2
-1 2
-1 -2
2 1
2 -1
-2 1
-2 -1
```

## Explanation

1. **Permutations Generation**: First, we generate all permutations of the numbers 1 to n
2. **Sign Combinations**: For each permutation, we generate all possible sign combinations using bit manipulation
3. **Result Construction**: We combine each permutation with each sign combination to create the final signed permutations

## Time Complexity
- O(n! × 2^n) where n! represents permutations and 2^n represents sign combinations

## Space Complexity
- O(n! × 2^n) for storing all results

The solution handles the signed permutation problem by generating all possible arrangements of numbers 1 to n where each number can be either positive or negative, creating all valid orientations of gene orderings.

