# Rosalind Problem: The Wright-Fisher Model of Genetic Drift (C# Solution)

## Problem Understanding

The Wright-Fisher model describes how genetic drift affects allele frequencies in a population over time. Given a population with `N` diploid individuals and an initial frequency `m` of the dominant allele, we need to calculate the probability that the allele will eventually be lost from the population.

## Solution Approach

The key insight is to use the fact that the probability of eventual fixation of an allele with frequency `p` in a population of size `N` is `2p`. For the probability of eventual loss, we need to calculate `1 - 2p` where `p = m/(2N)`.

However, since we're dealing with genetic drift, we can also model this as a Markov chain where we calculate the probability of absorption at state 0 (loss) versus state 2N (fixation).

## C# Implementation

```csharp
using System;
using System.Linq;

public class WrightFisherModel
{
    public static double CalculateAlleleLossProbability(int N, int m)
    {
        // N is the number of diploid individuals
        // m is the number of copies of the dominant allele
        // Total alleles = 2 * N
        
        double totalAlleles = 2 * N;
        double initialFrequency = m / totalAlleles;
        
        // For the Wright-Fisher model, the probability of eventual loss
        // of an allele with initial frequency p is 1 - 2p
        // This is a well-known result from population genetics
        
        double probabilityLoss = 1.0 - 2.0 * initialFrequency;
        
        return probabilityLoss;
    }
    
    // Alternative implementation using recursive approach for small cases
    public static double CalculateAlleleLossProbabilityRecursive(int N, int m)
    {
        // This is a more complex approach that simulates the process
        // For small values of N and m, we can calculate directly
        
        if (m == 0) return 1.0;  // Already lost
        if (m == 2 * N) return 0.0;  // Already fixed
        
        // Probability of losing the allele
        // This is a complex calculation that typically requires
        // solving the diffusion equation or using Markov chains
        
        // For the purposes of this problem, we use the analytical solution
        return 1.0 - 2.0 * (double)m / (2.0 * N);
    }
    
    public static void Main(string[] args)
    {
        // Example usage
        int N = 4;  // 4 diploid individuals
        int m = 3;  // 3 copies of dominant allele
        
        double result = CalculateAlleleLossProbability(N, m);
        Console.WriteLine($"Probability of allele loss: {result:F6}");
        
        // Test with different values
        Console.WriteLine("Testing with different values:");
        Console.WriteLine($"N=10, m=5: {CalculateAlleleLossProbability(10, 5):F6}");
        Console.WriteLine($"N=5, m=1: {CalculateAlleleLossProbability(5, 1):F6}");
        Console.WriteLine($"N=3, m=0: {CalculateAlleleLossProbability(3, 0):F6}");
    }
}

// Alternative more detailed implementation
public class AdvancedWrightFisherModel
{
    // More accurate calculation using the exact formula
    public static double CalculateExactLossProbability(int N, int m)
    {
        // For the Wright-Fisher model, the probability that an allele
        // with initial frequency p = m/(2N) is lost is:
        // P_loss = 1 - 2p = 1 - 2*(m/(2N)) = 1 - m/N
        
        if (m == 0) return 1.0;
        if (m == 2 * N) return 0.0;
        
        return 1.0 - (double)m / N;
    }
    
    // Function to read input from file or console (for Rosalind format)
    public static double SolveRosalindProblem(string input)
    {
        string[] parts = input.Split(' ');
        int N = int.Parse(parts[0]);
        int m = int.Parse(parts[1]);
        
        return CalculateExactLossProbability(N, m);
    }
    
    public static void Main(string[] args)
    {
        // Example from Rosalind problem
        string input = "4 3";
        double result = SolveRosalindProblem(input);
        Console.WriteLine($"Result: {result:F6}");
        
        // Additional test cases
        Console.WriteLine($"Test N=10, m=5: {CalculateExactLossProbability(10, 5):F6}");
        Console.WriteLine($"Test N=5, m=1: {CalculateExactLossProbability(5, 1):F6}");
    }
}
```

## Explanation

The solution uses the fundamental result from population genetics for the Wright-Fisher model:

1. **Key Formula**: For a population of `N` diploid individuals with `m` copies of a dominant allele, the probability that this allele will eventually be lost from the population is:
   ```
   P_loss = 1 - (m/N)
   ```

2. **Derivation**: 
   - Total alleles = 2N
   - Initial frequency p = m/(2N)
   - Probability of fixation = 2p = m/N
   - Probability of loss = 1 - probability of fixation = 1 - m/N

3. **Edge Cases**:
   - If m = 0: allele already lost, probability = 1
   - If m = 2N: allele already fixed, probability = 0

## Sample Input/Output

**Input**: `4 3`
**Output**: `0.250000`

**Input**: `10 5` 
**Output**: `0.500000`

This solution correctly implements the mathematical foundation of the Wright-Fisher model for genetic drift and provides the probability of allele loss in a population over time.

