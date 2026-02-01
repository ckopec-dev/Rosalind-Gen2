# Rosalind Problem: Wright-Fisher's Expected Behavior

## Problem Description
In the Wright-Fisher model of genetic drift, we consider a population of N diploid individuals, where each individual has two alleles for each gene. We want to calculate the expected number of copies of a particular allele in the next generation.

## Solution in Visual Basic

```vb
Module WrightFisherExpectedBehavior
    Sub Main()
        ' Read input data
        Dim N As Integer = 15  ' Population size (diploid individuals)
        Dim m As Integer = 8   ' Number of copies of the allele in current generation
        Dim g As Integer = 9   ' Number of generations
        
        ' Calculate expected number of copies after g generations
        Dim expectedCopies As Double = ExpectedCopiesAfterGenerations(N, m, g)
        
        ' Output result
        Console.WriteLine("Expected number of copies after " & g & " generations: " & expectedCopies.ToString("F6"))
    End Sub
    
    Function ExpectedCopiesAfterGenerations(N As Integer, m As Integer, g As Integer) As Double
        ' In Wright-Fisher model, the expected number of copies remains constant
        ' over generations, assuming no selection, mutation, or migration
        ' Expected value = 2 * N * (m / (2 * N)) = m
        ' But if we're looking at the expected frequency and then converting back
        ' to copies, we need to consider the probability of fixation
        
        ' For the expected number of copies after g generations:
        ' If m = 0 or m = 2*N, then it's fixed (0 or 2*N copies)
        ' Otherwise, we use the formula for expected value in Wright-Fisher model
        
        If m = 0 Then
            Return 0.0
        ElseIf m = 2 * N Then
            Return 2.0 * N
        Else
            ' Expected copies in next generation = 2*N * (m/(2*N)) = m
            ' This is because the expected value of the proportion remains constant
            Return m
        End If
    End Function
    
    ' Alternative implementation for more complex scenarios
    Function ExpectedFrequencyAfterGenerations(N As Integer, p As Double, g As Integer) As Double
        ' Calculate expected frequency after g generations
        ' This is a simplified version - in reality, this would involve
        ' more complex calculations with binomial distributions
        
        ' For Wright-Fisher model, the expected frequency remains the same
        ' if there's no selection, mutation, or migration
        Return p
    End Function
    
    ' More accurate implementation for the specific problem
    Function CalculateExpectedBehavior(N As Integer, m As Integer, g As Integer) As Double
        ' The expected number of copies of an allele after g generations
        ' in a Wright-Fisher model with no selection is simply m
        ' This is because the expected value of the allele frequency is preserved
        
        ' However, if we want to consider the variance and probability of fixation:
        ' The variance decreases over time: Var(t) = p(1-p)/2N * (1 - (1-2p)^t)
        
        Return m
    End Function
End Module
```

## Explanation

This solution addresses the Wright-Fisher model for genetic drift. In this model:

1. **Population Model**: We have N diploid individuals (2N total alleles)
2. **Allele Dynamics**: The expected number of copies of a particular allele remains constant over generations in the absence of selection, mutation, or migration
3. **Key Insight**: In the Wright-Fisher model, the expected value of allele frequency is preserved, so if we start with m copies out of 2N total alleles, we expect m copies in the next generation

## Key Points

- The Wright-Fisher model assumes random mating and no selection
- Expected number of copies = initial number of copies (m)
- The model accounts for genetic drift, which causes allele frequencies to fluctuate randomly
- For large populations, these fluctuations are smaller
- The solution handles edge cases where alleles are fixed (0 or 2N copies)

## Usage

To use this code:
1. Set the population size (N)
2. Set the initial number of copies (m)
3. Set the number of generations (g)
4. The expected copies after g generations will be returned

The output represents the expected number of copies of the allele after the specified number of generations under the Wright-Fisher model.

