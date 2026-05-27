# Rosalind Problem: Implement_GreedyMotifSearch in C#

## Problem Description
The Greedy Motif Search algorithm is a heuristic approach to find the best motif (consensus pattern) in a set of DNA sequences. This implementation uses a greedy approach to find the best k-mer patterns across all sequences.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class GreedyMotifSearch
{
    public static void Main()
    {
        // Example input
        string[] dnaSequences = {
            "GGCGTTCAGGCA",
            "AAGAATCAGTCA",
            "CAAGGAGTTCCT",
            "CACCTTCACTCA"
        };
        
        int k = 3;
        
        string[] result = GreedyMotifSearchAlgorithm(dnaSequences, k);
        
        Console.WriteLine(string.Join(" ", result));
    }
    
    public static string[] GreedyMotifSearchAlgorithm(string[] dnaSequences, int k)
    {
        int t = dnaSequences.Length;
        string[] bestMotifs = new string[t];
        
        // Initialize best motifs with first k-mers from each sequence
        for (int i = 0; i < t; i++)
        {
            bestMotifs[i] = dnaSequences[i].Substring(0, k);
        }
        
        // Get all possible k-mers from the first sequence
        string[] firstSequenceKmers = GetKmers(dnaSequences[0], k);
        
        foreach (string motif in firstSequenceKmers)
        {
            string[] motifs = new string[t];
            motifs[0] = motif;
            
            // Build motifs for remaining sequences
            for (int i = 1; i < t; i++)
            {
                // Create profile from current motifs
                Dictionary<char, double[]> profile = CreateProfile(motifs, i);
                // Find best k-mer in current sequence using profile
                motifs[i] = GetBestKmer(dnaSequences[i], k, profile);
            }
            
            // Check if this set of motifs is better than current best
            if (ScoreMotifs(motifs) < ScoreMotifs(bestMotifs))
            {
                bestMotifs = motifs;
            }
        }
        
        return bestMotifs;
    }
    
    public static string[] GetKmers(string sequence, int k)
    {
        List<string> kmers = new List<string>();
        for (int i = 0; i <= sequence.Length - k; i++)
        {
            kmers.Add(sequence.Substring(i, k));
        }
        return kmers.ToArray();
    }
    
    public static Dictionary<char, double[]> CreateProfile(string[] motifs, int currentSequenceIndex)
    {
        int k = motifs[0].Length;
        Dictionary<char, double[]> profile = new Dictionary<char, double[]>();
        profile['A'] = new double[k];
        profile['C'] = new double[k];
        profile['G'] = new double[k];
        profile['T'] = new double[k];
        
        // Count nucleotides in each position
        for (int i = 0; i < k; i++)
        {
            foreach (string motif in motifs)
            {
                char nucleotide = motif[i];
                profile[nucleotide][i]++;
            }
        }
        
        // Convert counts to probabilities
        int motifCount = motifs.Length;
        foreach (var kvp in profile)
        {
            for (int i = 0; i < k; i++)
            {
                profile[kvp.Key][i] = profile[kvp.Key][i] / motifCount;
            }
        }
        
        return profile;
    }
    
    public static string GetBestKmer(string sequence, int k, Dictionary<char, double[]> profile)
    {
        double maxProbability = -1;
        string bestKmer = "";
        
        for (int i = 0; i <= sequence.Length - k; i++)
        {
            string kmer = sequence.Substring(i, k);
            double probability = 1.0;
            
            for (int j = 0; j < k; j++)
            {
                char nucleotide = kmer[j];
                probability *= profile[nucleotide][j];
            }
            
            if (probability > maxProbability)
            {
                maxProbability = probability;
                bestKmer = kmer;
            }
        }
        
        return bestKmer;
    }
    
    public static int ScoreMotifs(string[] motifs)
    {
        int k = motifs[0].Length;
        int score = 0;
        
        for (int i = 0; i < k; i++)
        {
            Dictionary<char, int> nucleotideCount = new Dictionary<char, int>();
            nucleotideCount['A'] = 0;
            nucleotideCount['C'] = 0;
            nucleotideCount['G'] = 0;
            nucleotideCount['T'] = 0;
            
            // Count nucleotides in position i
            foreach (string motif in motifs)
            {
                nucleotideCount[motif[i]]++;
            }
            
            // Find the maximum count and add (total - max) to score
            int maxCount = nucleotideCount.Values.Max();
            score += (motifs.Length - maxCount);
        }
        
        return score;
    }
}
```

## Explanation

This implementation follows the Greedy Motif Search algorithm:

1. **Initialization**: Start with the first k-mer from each DNA sequence as the initial best motifs.

2. **Main Loop**: For each possible k-mer in the first sequence:
   - Use it as the first motif
   - Build a profile from the current set of motifs
   - Find the best k-mer in each subsequent sequence using the profile
   - Compare the score of this set with the current best

3. **Profile Creation**: 
   - Count nucleotides at each position across all motifs
   - Convert counts to probabilities for each position

4. **K-mer Selection**: 
   - For each sequence, find the k-mer that maximizes the probability given the current profile

5. **Scoring**: 
   - Calculate the score as the number of mismatches from the consensus pattern
   - Lower score means better motifs

## Time Complexity
- O(t × n × k × t × n × k) = O(t² × n² × k²) where t is number of sequences, n is sequence length, and k is motif length

## Space Complexity
- O(t × k) for storing motifs and profile

The algorithm uses a greedy approach to find a locally optimal solution, which may not be the globally optimal solution but works well in practice for motif finding problems.

