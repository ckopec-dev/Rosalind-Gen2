# Rosalind Problem: Compute the Probability of an Outcome Given a Hidden Path (VB.NET Solution)

## Problem Understanding

This problem asks us to compute the probability of observing a specific sequence of outcomes given a hidden path in a Hidden Markov Model (HMM).

## Solution Approach

We need to:
1. Parse the input data (hidden path and emission matrix)
2. Calculate the probability by multiplying emission probabilities for each state and observation pair

## VB.NET Implementation

```vb
Imports System
Imports System.IO
Imports System.Collections.Generic
Imports System.Linq

Module Program
    Sub Main()
        ' Read input from file
        Dim input As String() = File.ReadAllLines("rosalind_ba10c.txt")
        
        ' Parse the hidden path
        Dim path As String = input(0)
        
        ' Parse the alphabet (states)
        Dim alphabet As String() = input(2).Split(" "c)
        
        ' Parse the emission matrix
        Dim emissionMatrix As New Dictionary(Of String, Dictionary(Of String, Double))
        Dim matrixStart As Integer = 4
        
        For i As Integer = 0 To alphabet.Length - 1
            Dim line As String = input(matrixStart + i + 1)
            Dim values As String() = line.Split(" "c)
            
            If Not emissionMatrix.ContainsKey(alphabet(i)) Then
                emissionMatrix(alphabet(i)) = New Dictionary(Of String, Double)
            End If
            
            For j As Integer = 0 To values.Length - 1
                If j > 0 Then ' Skip the first element which is the state name
                    emissionMatrix(alphabet(i))(alphabet(j)) = Convert.ToDouble(values(j))
                End If
            Next
        Next
        
        ' Calculate probability
        Dim probability As Double = 1.0
        
        For i As Integer = 0 To path.Length - 1
            Dim state As Char = path(i)
            Dim observation As Char = input(4).ToCharArray()(i) ' First line after header is the observation sequence
            
            ' Convert to string for dictionary lookup
            Dim stateStr As String = state.ToString()
            Dim observationStr As String = observation.ToString()
            
            probability *= emissionMatrix(stateStr)(observationStr)
        Next
        
        Console.WriteLine(probability.ToString("F10"))
    End Sub
End Module
```

## Alternative Cleaner Implementation

```vb
Imports System
Imports System.IO
Imports System.Collections.Generic

Module ComputeProbability
    Sub Main()
        ' Read all input lines
        Dim lines As String() = File.ReadAllLines("rosalind_ba10c.txt")
        
        ' Hidden path
        Dim path As String = lines(0)
        
        ' Observations
        Dim observations As String = lines(4)
        
        ' State alphabet
        Dim states As String() = lines(2).Split(" "c)
        
        ' Parse emission probabilities
        Dim emissionMatrix As New Dictionary(Of String, Dictionary(Of String, Double))
        Dim matrixStart As Integer = 6
        
        For i As Integer = 0 To states.Length - 1
            Dim row As String() = lines(matrixStart + i + 1).Split(" "c)
            emissionMatrix(states(i)) = New Dictionary(Of String, Double)
            
            For j As Integer = 1 To row.Length - 1
                emissionMatrix(states(i))(states(j - 1)) = Convert.ToDouble(row(j))
            Next
        Next
        
        ' Calculate probability
        Dim probability As Double = 1.0
        
        For i As Integer = 0 To path.Length - 1
            Dim state As Char = path(i)
            Dim observation As Char = observations(i)
            
            probability *= emissionMatrix(state.ToString())(observation.ToString())
        Next
        
        Console.WriteLine(probability.ToString("F10"))
    End Sub
End Module
```

## Input Format Expected

The input file should contain:
```
path
--------
alphabet
--------
emission_matrix_header
emission_matrix_lines
observations
```

## Example Input
```
BBABBBABBB
--------
A B
--------
A 0.176 0.824
B 0.568 0.432
--------
BBAABBAAAB
```

## Key Points

1. **Input Parsing**: Carefully parse the hidden path and emission matrix
2. **Probability Calculation**: Multiply emission probabilities for each state-observation pair
3. **Dictionary Usage**: Use nested dictionaries for efficient matrix lookup
4. **Precision**: Output with appropriate decimal precision (10 decimal places)

This solution efficiently computes the probability of observing a specific sequence given a hidden path in a Hidden Markov Model.

