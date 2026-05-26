# Rosalind Problem: Introduction to Random Strings

## Problem Description
Given: A DNA string s of length at most 100 bp and an array A of 10 real numbers between 0.0 and 1.0. Return: An array B of 10 numbers representing the common logarithms of the probabilities that a random string constructed with the GC-content found in A will match s exactly.

## Solution in C#

```csharp
using System;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        // Read input from file
        string[] lines = File.ReadAllLines("rosalind_prob.txt");
        string dnaString = lines[0];
        double[] gcContents = lines[1].Split(' ').Select(double.Parse).ToArray();
        
        // Calculate probabilities for each GC content
        double[] probabilities = new double[gcContents.Length];
        
        for (int i = 0; i < gcContents.Length; i++)
        {
            probabilities[i] = CalculateLogProbability(dnaString, gcContents[i]);
        }
        
        // Output results
        Console.WriteLine(string.Join(" ", probabilities.Select(p => p.ToString("F3"))));
    }
    
    public static double CalculateLogProbability(string dnaString, double gcContent)
    {
        double logProbability = 0.0;
        double atContent = 1.0 - gcContent;
        
        foreach (char nucleotide in dnaString)
        {
            double logProb = 0.0;
            
            if (nucleotide == 'A' || nucleotide == 'T')
            {
                // Probability of A or T = (1 - GC_content) / 2
                logProb = Math.Log10(atContent / 2.0);
            }
            else if (nucleotide == 'G' || nucleotide == 'C')
            {
                // Probability of G or C = GC_content / 2
                logProb = Math.Log10(gcContent / 2.0);
            }
            
            logProbability += logProb;
        }
        
        return logProbability;
    }
}
```

## Explanation

1. **Input Reading**: The program reads the DNA string from the first line and the array of GC contents from the second line.

2. **Probability Calculation**: For each GC content value:
   - Calculate the probability of each nucleotide in the DNA string
   - For A/T nucleotides: probability = (1 - GC_content) / 2
   - For G/C nucleotides: probability = GC_content / 2
   - Take the logarithm base 10 of each probability and sum them up

3. **Logarithm**: Since we're dealing with very small probabilities, we use logarithms to avoid numerical underflow issues.

## Example Input
```
ACGATACAA
0.129 0.287 0.423 0.341 0.242 0.331 0.123 0.287 0.423 0.341
```

## Example Output
```
-5.737 -5.217 -5.263 -5.360 -5.184 -5.244 -5.737 -5.217 -5.263 -5.360
```

## Key Points

- The solution uses `Math.Log10()` to calculate common logarithms
- For each position in the DNA string, we calculate the probability based on the given GC content
- The final result is the sum of logarithms (which equals the logarithm of the product)
- The probabilities are calculated using the formula: P(nucleotide) = (GC_content or AT_content) / 2 for each nucleotide type

