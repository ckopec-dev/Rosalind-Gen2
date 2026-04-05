# Rosalind Problem: Implement_GibbsSampler

## Problem Description
Implement the Gibbs sampler for motif finding. The Gibbs sampler is a probabilistic algorithm that finds motifs in a set of DNA sequences.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class GibbsSampler
{
    public static List<string> GibbsSampler(List<string> dna, int k, int t, int n)
    {
        // Initialize random number generator
        Random rand = new Random();
        
        // Initialize best motifs with random motifs from each sequence
        List<string> bestMotifs = new List<string>();
        foreach (string sequence in dna)
        {
            int start = rand.Next(sequence.Length - k + 1);
            bestMotifs.Add(sequence.Substring(start, k));
        }
        
        // Store current motifs
        List<string> motifs = new List<string>(bestMotifs);
        
        // Run Gibbs sampling for n iterations
        for (int i = 0; i < n; i++)
        {
            // Choose random sequence index
            int randomIndex = rand.Next(t);
            
            // Create profile from all motifs except the one at randomIndex
            List<string> profileMotifs = new List<string>(motifs);
            profileMotifs.RemoveAt(randomIndex);
            
            // Build profile matrix
            double[,] profile = BuildProfileMatrix(profileMotifs, k);
            
            // Generate new motif for the random sequence
            string newMotif = GenerateMotif(dna[randomIndex], k, profile);
            
            // Update motifs
            motifs[randomIndex] = newMotif;
            
            // Update best motifs if current motifs are better
            if (CalculateScore(motifs) < CalculateScore(bestMotifs))
            {
                bestMotifs = new List<string>(motifs);
            }
        }
        
        return bestMotifs;
    }
    
    private static double[,] BuildProfileMatrix(List<string> motifs, int k)
    {
        int t = motifs.Count;
        double[,] profile = new double[4, k]; // 4 nucleotides: A, C, G, T
        
        // Initialize with pseudocounts (1 for each position)
        for (int i = 0; i < 4; i++)
        {
            for (int j = 0; j < k; j++)
            {
                profile[i, j] = 1.0;
            }
        }
        
        // Count nucleotides at each position
        for (int i = 0; i < t; i++)
        {
            for (int j = 0; j < k; j++)
            {
                char nucleotide = motifs[i][j];
                int index = NucleotideToIndex(nucleotide);
                profile[index, j] += 1.0;
            }
        }
        
        // Convert counts to probabilities
        for (int j = 0; j < k; j++)
        {
            double total = 0;
            for (int i = 0; i < 4; i++)
            {
                total += profile[i, j];
            }
            for (int i = 0; i < 4; i++)
            {
                profile[i, j] /= total;
            }
        }
        
        return profile;
    }
    
    private static string GenerateMotif(string sequence, int k, double[,] profile)
    {
        Random rand = new Random();
        int n = sequence.Length;
        double maxProbability = 0;
        int bestPosition = 0;
        
        // Calculate probability for each k-mer
        for (int i = 0; i <= n - k; i++)
        {
            string kmer = sequence.Substring(i, k);
            double probability = 1.0;
            
            for (int j = 0; j < k; j++)
            {
                char nucleotide = kmer[j];
                int index = NucleotideToIndex(nucleotide);
                probability *= profile[index, j];
            }
            
            if (probability > maxProbability)
            {
                maxProbability = probability;
                bestPosition = i;
            }
        }
        
        return sequence.Substring(bestPosition, k);
    }
    
    private static int CalculateScore(List<string> motifs)
    {
        int score = 0;
        int k = motifs[0].Length;
        int t = motifs.Count;
        
        for (int j = 0; j < k; j++)
        {
            Dictionary<char, int> counts = new Dictionary<char, int>();
            counts['A'] = 0;
            counts['C'] = 0;
            counts['G'] = 0;
            counts['T'] = 0;
            
            // Count nucleotides at position j
            for (int i = 0; i < t; i++)
            {
                char nucleotide = motifs[i][j];
                counts[nucleotide]++;
            }
            
            // Add penalty for non-consensus nucleotide
            int maxCount = counts.Values.Max();
            score += (t - maxCount);
        }
        
        return score;
    }
    
    private static int NucleotideToIndex(char nucleotide)
    {
        switch (nucleotide)
        {
            case 'A': return 0;
            case 'C': return 1;
            case 'G': return 2;
            case 'T': return 3;
            default: return -1;
        }
    }
    
    // Helper method to print motifs
    public static void PrintMotifs(List<string> motifs)
    {
        foreach (string motif in motifs)
        {
            Console.WriteLine(motif);
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example input
        List<string> dna = new List<string>
        {
            "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA",
            "GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG",
            "TAGTACCGAGACCGAAAGAAGTATACAGGCGT",
            "TCATCGGTAGTGACAGCAGCCAGGTCCCGA",
            "GCTGAGACCGAGACCGAAAGAAGTATACAGGCGT"
        };
        
        int k = 8;
        int t = 5;
        int n = 100;
        
        List<string> result = GibbsSampler.GibbsSampler(dna, k, t, n);
        GibbsSampler.PrintMotifs(result);
    }
}
```

## Explanation

This implementation of the Gibbs sampler for motif finding includes:

1. **Main GibbsSampler function**: 
   - Initializes random motifs from each DNA sequence
   - Runs the Gibbs sampling algorithm for n iterations
   - Updates the best motifs when a better solution is found

2. **BuildProfileMatrix function**:
   - Creates a profile matrix from the motifs
   - Uses pseudocounts to avoid zero probabilities
   - Converts counts to probabilities

3. **GenerateMotif function**:
   - Uses the profile matrix to probabilistically select a new motif
   - Calculates the probability of each k-mer and selects the most probable one

4. **CalculateScore function**:
   - Computes the score of a set of motifs
   - Measures how far the motifs are from being consensus sequences

5. **NucleotideToIndex helper**:
   - Maps nucleotides to array indices (A=0, C=1, G=2, T=3)

The algorithm uses a probabilistic approach to find motifs, iteratively improving the solution by randomly selecting one sequence, rebuilding the profile excluding that sequence, and then sampling a new motif from the selected sequence using the profile probabilities.

