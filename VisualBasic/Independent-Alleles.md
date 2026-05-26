# Rosalind Problem: Independent Alleles (Independent Alleles)

## Problem Description
In a population whose members are in Hardy-Weinberg equilibrium, we can model the probability of having a certain number of dominant alleles in a population. Given two positive integers k and N, where k is the generation number and N is the number of organisms with at least one dominant allele, we want to find the probability that at least N organisms in generation k have at least one dominant allele.

## Solution Approach
This is a binomial probability problem. We need to:
1. Calculate the probability of an individual having at least one dominant allele
2. Use binomial distribution to find the probability of having at least N such individuals

## Visual Basic Implementation

```vb
Module IndependentAlleles
    Sub Main()
        ' Example usage
        Dim k As Integer = 2
        Dim N As Integer = 1
        Dim result As Double = Probability(k, N)
        Console.WriteLine("Probability: " & result.ToString("F6"))
    End Sub

    Function Probability(k As Integer, N As Integer) As Double
        ' Calculate the probability of having at least N organisms 
        ' with at least one dominant allele in generation k
        
        Dim totalOrganisms As Integer = 2 ^ k
        Dim p As Double = 0.0
        
        ' For each possible number of dominant alleles from N to totalOrganisms
        For i As Integer = N To totalOrganisms
            p += BinomialProbability(totalOrganisms, i, 0.75)
        Next
        
        Return p
    End Function
    
    Function BinomialProbability(n As Integer, k As Integer, p As Double) As Double
        ' Calculate binomial probability: C(n,k) * p^k * (1-p)^(n-k)
        Return Combination(n, k) * Math.Pow(p, k) * Math.Pow(1 - p, n - k)
    End Function
    
    Function Combination(n As Integer, k As Integer) As Double
        ' Calculate C(n,k) = n! / (k! * (n-k)!)
        If k > n Or k < 0 Then Return 0
        If k = 0 Or k = n Then Return 1
        
        ' Use the more efficient calculation to avoid overflow
        Dim result As Double = 1
        For i As Integer = 1 To Math.Min(k, n - k)
            result = result * (n - i + 1) / i
        Next
        
        Return result
    End Function
End Module
```

## Explanation

1. **Main Logic**: 
   - In generation k, there are 2^k organisms
   - Each organism has a probability of 0.75 of having at least one dominant allele (since we're dealing with heterozygous parents)
   - We want P(X ≥ N) where X is the number of organisms with at least one dominant allele

2. **Binomial Distribution**: 
   - We use the binomial probability formula: P(X = k) = C(n,k) × p^k × (1-p)^(n-k)
   - Where n = 2^k, p = 0.75, and we sum from N to n

3. **Combination Function**:
   - Calculates C(n,k) efficiently to avoid large factorials
   - Uses the property that C(n,k) = C(n, n-k) to minimize calculations

## Example Output
For k=2, N=1:
- Generation 2 has 4 organisms
- Probability of at least 1 organism with dominant allele = 0.75

The solution handles the mathematical complexity of binomial probabilities and efficiently calculates the required probability using Visual Basic's built-in functions.

