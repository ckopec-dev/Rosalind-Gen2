# Rosalind Problem: Find a Highest-Scoring Multiple Sequence Alignment

## Problem Description
Given three DNA strings, find a multiple sequence alignment with the highest score.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class MultipleSequenceAlignment
{
    // Scoring parameters
    private const int MATCH_SCORE = 1;
    private const int MISMATCH_SCORE = -1;
    private const int GAP_PENALTY = -2;
    
    public static void Main()
    {
        // Example input (replace with actual input)
        string seq1 = "ACGT";
        string seq2 = "ACGT";
        string seq3 = "ACGT";
        
        var result = FindHighestScoringAlignment(seq1, seq2, seq3);
        Console.WriteLine($"Highest score: {result.score}");
        Console.WriteLine($"Alignment 1: {result.alignment1}");
        Console.WriteLine($"Alignment 2: {result.alignment2}");
        Console.WriteLine($"Alignment 3: {result.alignment3}");
    }
    
    public static (int score, string alignment1, string alignment2, string alignment3) 
    FindHighestScoringAlignment(string seq1, string seq2, string seq3)
    {
        int m = seq1.Length;
        int n = seq2.Length;
        int p = seq3.Length;
        
        // Create 3D DP table
        int[,,] dp = new int[m + 1, n + 1, p + 1];
        
        // Initialize base cases
        for (int i = 0; i <= m; i++)
            for (int j = 0; j <= n; j++)
                for (int k = 0; k <= p; k++)
                {
                    if (i == 0 && j == 0 && k == 0)
                        dp[i, j, k] = 0;
                    else if (i == 0 && j == 0)
                        dp[i, j, k] = k * GAP_PENALTY;
                    else if (i == 0 && k == 0)
                        dp[i, j, k] = j * GAP_PENALTY;
                    else if (j == 0 && k == 0)
                        dp[i, j, k] = i * GAP_PENALTY;
                    else if (i == 0)
                        dp[i, j, k] = dp[i, j - 1, k - 1] + GAP_PENALTY;
                    else if (j == 0)
                        dp[i, j, k] = dp[i - 1, j, k - 1] + GAP_PENALTY;
                    else if (k == 0)
                        dp[i, j, k] = dp[i - 1, j - 1, k] + GAP_PENALTY;
                    else
                        dp[i, j, k] = int.MinValue;
                }
        
        // Fill the DP table
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                for (int k = 1; k <= p; k++)
                {
                    int score = GetScore(seq1[i - 1], seq2[j - 1], seq3[k - 1]);
                    
                    dp[i, j, k] = Math.Max(dp[i - 1, j, k] + GAP_PENALTY, 
                                          Math.Max(dp[i, j - 1, k] + GAP_PENALTY,
                                                  Math.Max(dp[i, j, k - 1] + GAP_PENALTY,
                                                          dp[i - 1, j - 1, k - 1] + score)));
                }
            }
        }
        
        // Backtrack to find the alignment
        string align1 = "", align2 = "", align3 = "";
        int x = m, y = n, z = p;
        
        while (x > 0 || y > 0 || z > 0)
        {
            if (x > 0 && y > 0 && z > 0)
            {
                int score = GetScore(seq1[x - 1], seq2[y - 1], seq3[z - 1]);
                int current = dp[x, y, z];
                int diagonal = dp[x - 1, y - 1, z - 1] + score;
                int up = dp[x - 1, y, z] + GAP_PENALTY;
                int left = dp[x, y - 1, z] + GAP_PENALTY;
                int front = dp[x, y, z - 1] + GAP_PENALTY;
                
                if (current == diagonal)
                {
                    align1 = seq1[x - 1] + align1;
                    align2 = seq2[y - 1] + align2;
                    align3 = seq3[z - 1] + align3;
                    x--;
                    y--;
                    z--;
                }
                else if (current == up)
                {
                    align1 = seq1[x - 1] + align1;
                    align2 = "-" + align2;
                    align3 = "-" + align3;
                    x--;
                }
                else if (current == left)
                {
                    align1 = "-" + align1;
                    align2 = seq2[y - 1] + align2;
                    align3 = "-" + align3;
                    y--;
                }
                else if (current == front)
                {
                    align1 = "-" + align1;
                    align2 = "-" + align2;
                    align3 = seq3[z - 1] + align3;
                    z--;
                }
            }
            else if (x > 0)
            {
                align1 = seq1[x - 1] + align1;
                align2 = "-" + align2;
                align3 = "-" + align3;
                x--;
            }
            else if (y > 0)
            {
                align1 = "-" + align1;
                align2 = seq2[y - 1] + align2;
                align3 = "-" + align3;
                y--;
            }
            else if (z > 0)
            {
                align1 = "-" + align1;
                align2 = "-" + align2;
                align3 = seq3[z - 1] + align3;
                z--;
            }
        }
        
        return (dp[m, n, p], align1, align2, align3);
    }
    
    private static int GetScore(char c1, char c2, char c3)
    {
        if (c1 == c2 && c2 == c3)
            return MATCH_SCORE;
        else
            return MISMATCH_SCORE;
    }
}
```

## Explanation

This solution implements a dynamic programming approach for finding the highest-scoring multiple sequence alignment of three DNA sequences:

1. **DP Table**: Uses a 3D array `dp[i,j,k]` where each cell represents the maximum score for aligning the first `i`, `j`, and `k` characters of the three sequences.

2. **Initialization**: Sets up base cases for when one or more sequences are empty.

3. **Recurrence Relation**: For each cell, the algorithm considers all possible ways to extend the alignment:
   - Gap in first sequence
   - Gap in second sequence  
   - Gap in third sequence
   - Match/mismatch of all three characters

4. **Scoring**: 
   - Match: +1
   - Mismatch: -1
   - Gap penalty: -2

5. **Backtracking**: Reconstructs the actual alignment by following the path that led to the optimal score.

## Time and Space Complexity

- **Time Complexity**: O(m×n×p) where m, n, p are the lengths of the three sequences
- **Space Complexity**: O(m×n×p) for the 3D DP table

## Usage

Replace the example sequences in the `Main` method with actual input sequences from Rosalind, or modify the code to read from standard input or a file as required by the specific problem requirements.

