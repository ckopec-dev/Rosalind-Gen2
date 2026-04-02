# Rosalind Problem: Creating a Distance Matrix (C# Solution)

## Problem Understanding

The task is to create a distance matrix from a collection of DNA sequences. The distance between two sequences is typically calculated as the proportion of nucleotide positions at which the sequences differ (p-distance).

## Solution Approach

1. Parse input sequences from FASTA format
2. Calculate pairwise p-distances between all sequences
3. Output the distance matrix

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class DistanceMatrix
{
    public static void Main(string[] args)
    {
        // Read input from file or stdin
        string input = File.ReadAllText("rosalind_pdst.txt");
        var sequences = ParseFASTA(input);
        
        // Calculate distance matrix
        var distanceMatrix = CalculateDistanceMatrix(sequences);
        
        // Output results
        foreach (var row in distanceMatrix)
        {
            Console.WriteLine(string.Join(" ", row.Select(x => x.ToString("F6"))));
        }
    }
    
    // Parse FASTA format input
    public static Dictionary<string, string> ParseFASTA(string input)
    {
        var sequences = new Dictionary<string, string>();
        var lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        string currentId = null;
        var currentSequence = new List<string>();
        
        foreach (string line in lines)
        {
            if (line.StartsWith(">"))
            {
                if (currentId != null)
                {
                    sequences[currentId] = string.Join("", currentSequence);
                }
                currentId = line.Substring(1).Trim();
                currentSequence.Clear();
            }
            else
            {
                currentSequence.Add(line.Trim());
            }
        }
        
        if (currentId != null)
        {
            sequences[currentId] = string.Join("", currentSequence);
        }
        
        return sequences;
    }
    
    // Calculate pairwise p-distances between sequences
    public static double[,] CalculateDistanceMatrix(Dictionary<string, string> sequences)
    {
        int n = sequences.Count;
        double[,] distances = new double[n, n];
        
        var sequenceList = sequences.Values.ToList();
        
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < n; j++)
            {
                if (i == j)
                {
                    distances[i, j] = 0.0;
                }
                else
                {
                    distances[i, j] = CalculatePDistance(sequenceList[i], sequenceList[j]);
                }
            }
        }
        
        return distances;
    }
    
    // Calculate p-distance between two sequences
    public static double CalculatePDistance(string seq1, string seq2)
    {
        if (seq1.Length != seq2.Length)
        {
            throw new ArgumentException("Sequences must be of equal length");
        }
        
        int differences = 0;
        int totalPositions = seq1.Length;
        
        for (int i = 0; i < totalPositions; i++)
        {
            if (seq1[i] != seq2[i])
            {
                differences++;
            }
        }
        
        return (double)differences / totalPositions;
    }
}
```

## Example Usage

Given input FASTA format:
```
>Rosalind_0498
AAATAAACTTCTTGAATATTGGTAA
>Rosalind_1395
AAATTTACTTCTTGAATATTGGTAA
>Rosalind_2392
AAATTTACTTCTTGAATATTGGTAA
>Rosalind_2323
AAATTTACTTCTTGAATATTGGTAA
```

The output would be a 4×4 distance matrix where each element [i,j] represents the p-distance between sequence i and sequence j.

## Key Points

- **P-distance**: The proportion of positions where the two sequences differ
- **Input parsing**: Handles FASTA format with headers starting with '>'
- **Matrix output**: Uses 6 decimal places as required by Rosalind
- **Error handling**: Checks for equal sequence lengths
- **Time complexity**: O(n²m) where n is number of sequences and m is sequence length

## Compilation and Execution

```bash
# Compile
csc DistanceMatrix.cs

# Run
DistanceMatrix.exe
```

This solution correctly handles the Rosalind problem requirements for creating a distance matrix from DNA sequences.

