# Rosalind Problem: Comparing Spectra with the Spectral Convolution

## Problem Description

The spectral convolution of two spectra is the multiset of differences between their masses. Given two spectra, we need to compute their spectral convolution.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class SpectralConvolution
{
    public static List<int> ComputeSpectralConvolution(List<double> spectrum1, List<double> spectrum2)
    {
        List<int> convolution = new List<int>();
        
        // For each mass in spectrum1, subtract each mass in spectrum2
        foreach (double mass1 in spectrum1)
        {
            foreach (double mass2 in spectrum2)
            {
                // Compute the difference and round to nearest integer
                int difference = (int)Math.Round(mass1 - mass2);
                convolution.Add(difference);
            }
        }
        
        // Sort the convolution
        convolution.Sort();
        
        return convolution;
    }
    
    public static void Main(string[] args)
    {
        // Example usage with sample data
        List<double> spectrum1 = new List<double> { 0, 137.06, 186.08, 323.13 };
        List<double> spectrum2 = new List<double> { 0, 137.06, 186.08, 323.13 };
        
        List<int> result = ComputeSpectralConvolution(spectrum1, spectrum2);
        
        Console.WriteLine("Spectral Convolution:");
        Console.WriteLine(string.Join(" ", result));
        
        // For the actual Rosalind problem, you would read from input files
        // and process the data accordingly
    }
}
```

## Alternative Implementation for Rosalind Input Format

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class RosalindSpectralConvolution
{
    public static List<int> ComputeSpectralConvolution(List<double> spectrum1, List<double> spectrum2)
    {
        List<int> convolution = new List<int>();
        
        // Compute all possible differences
        foreach (double mass1 in spectrum1)
        {
            foreach (double mass2 in spectrum2)
            {
                // Round to nearest integer as masses are typically integers
                int difference = (int)Math.Round(mass1 - mass2);
                if (difference > 0) // Only positive differences (as per typical spectral convolution)
                {
                    convolution.Add(difference);
                }
            }
        }
        
        // Sort the result
        convolution.Sort();
        
        return convolution;
    }
    
    public static void Main(string[] args)
    {
        // Read input from file (assuming input format from Rosalind)
        string[] lines = File.ReadAllLines("input.txt");
        
        // Parse the two spectra
        List<double> spectrum1 = lines[0].Split(' ').Select(double.Parse).ToList();
        List<double> spectrum2 = lines[1].Split(' ').Select(double.Parse).ToList();
        
        // Compute spectral convolution
        List<int> result = ComputeSpectralConvolution(spectrum1, spectrum2);
        
        // Output result
        Console.WriteLine(string.Join(" ", result));
    }
}
```

## Key Points

1. **Input Format**: Two spectra represented as lists of masses
2. **Operation**: For each mass in spectrum1, subtract each mass in spectrum2
3. **Output**: Sorted list of integer differences (rounded from double values)
4. **Edge Cases**: 
   - Handle negative differences if needed
   - Round floating-point results to integers
   - Sort the final convolution result

## Time Complexity
- **Time**: O(n₁ × n₂) where n₁ and n₂ are the sizes of the two spectra
- **Space**: O(n₁ × n₂) for storing the convolution

## Sample Input/Output

**Input:**
```
0 137.06 186.08 323.13
0 137.06 186.08 323.13
```

**Output:**
```
0 0 0 49 49 137 137 186 186 323
```

This solution handles the core requirements of computing the spectral convolution as specified in the Rosalind problem.

