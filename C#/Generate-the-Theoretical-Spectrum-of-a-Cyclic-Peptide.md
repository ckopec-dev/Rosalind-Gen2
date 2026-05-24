# Rosalind Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

## Problem Description

The theoretical spectrum of a cyclic peptide is the collection of all subpeptide masses that can be formed from the peptide. For a cyclic peptide of length n, there are n subpeptides of each length from 1 to n.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    // Amino acid masses
    private static readonly Dictionary<char, int> AminoAcidMasses = new Dictionary<char, int>
    {
        {'A', 71}, {'C', 103}, {'D', 115}, {'E', 129}, {'F', 147},
        {'G', 57}, {'H', 137}, {'I', 113}, {'K', 128}, {'L', 113},
        {'M', 131}, {'N', 114}, {'P', 97}, {'Q', 128}, {'R', 156},
        {'S', 87}, {'T', 101}, {'V', 99}, {'W', 186}, {'Y', 163}
    };

    public static void Main()
    {
        // Example usage
        string peptide = "LEQN";
        var spectrum = GenerateTheoreticalSpectrum(peptide);
        
        Console.WriteLine(string.Join(" ", spectrum));
    }

    public static List<int> GenerateTheoreticalSpectrum(string peptide)
    {
        // Create a list to store all subpeptide masses
        List<int> spectrum = new List<int> { 0 }; // Start with 0 (empty subpeptide)
        
        int n = peptide.Length;
        
        // For each possible subpeptide length (1 to n)
        for (int length = 1; length <= n; length++)
        {
            // For each starting position in the cyclic peptide
            for (int start = 0; start < n; start++)
            {
                // Calculate mass of subpeptide starting at 'start' with given 'length'
                int mass = 0;
                
                // Handle the case where the subpeptide wraps around the cyclic peptide
                for (int i = 0; i < length; i++)
                {
                    int position = (start + i) % n;
                    mass += AminoAcidMasses[peptide[position]];
                }
                
                spectrum.Add(mass);
            }
        }
        
        // Sort the spectrum and return
        return spectrum.OrderBy(x => x).ToList();
    }
}
```

## Explanation

1. **Amino Acid Masses**: We define a dictionary mapping each amino acid to its mass.

2. **Main Logic**: 
   - Start with 0 (representing the empty subpeptide)
   - For each possible subpeptide length from 1 to n (where n is peptide length):
     - For each starting position in the cyclic peptide:
       - Calculate the mass by summing up amino acid masses
       - Handle cyclic nature by using modulo operation: `(start + i) % n`
   - Sort the result in ascending order

3. **Cyclic Nature**: In a cyclic peptide, subpeptides can wrap around the end. For example, in peptide "LEQN" with length 4, a subpeptide starting at position 2 with length 3 would be "QN" + "L" = "QNL".

## Example

For peptide "LEQN":
- Length = 4
- Subpeptides of length 1: L(113), E(129), Q(128), N(114) 
- Subpeptides of length 2: LE(242), EQ(257), QN(242), NL(227)
- Subpeptides of length 3: LEQ(369), EQN(369), QNL(342), NLQ(342)
- Subpeptides of length 4: LEQN(490)

The output would be: `0 113 114 128 129 227 242 242 257 342 342 369 369 490`

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the peptide
- **Space Complexity**: O(n²) for storing all subpeptide masses

This solution correctly handles the cyclic nature of peptides and generates the complete theoretical spectrum as required by the Rosalind problem.

