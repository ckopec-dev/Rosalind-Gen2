# Rosalind Problem: Global Alignment with Scoring Matrix and Affine Gap Penalty

## Problem Description
Given two strings and a scoring matrix, find the global alignment with affine gap penalty.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class GlobalAlignmentWithAffineGap
{
    public static void Main(string[] args)
    {
        // Read input from file
        string[] lines = File.ReadAllLines("input.txt");
        string seq1 = lines[0];
        string seq2 = lines[1];
        
        // Read scoring matrix
        int[,] scoringMatrix = ReadScoringMatrix(lines.Skip(2).ToArray());
        
        // Solve the problem
        var result = GlobalAlignmentWithAffineGapPenalty(seq1, seq2, scoringMatrix);
        
        Console.WriteLine(result.score);
        Console.WriteLine(result.alignment1);
        Console.WriteLine(result.alignment2);
    }
    
    public static (int score, string alignment1, string alignment2) GlobalAlignmentWithAffineGapPenalty(
        string seq1, string seq2, int[,] scoringMatrix)
    {
        int m = seq1.Length;
        int n = seq2.Length;
        
        // Gap opening and extension penalties
        int gapOpen = -11;
        int gapExtend = -1;
        
        // Create 3D DP tables for affine gap penalty
        // F[i,j,0] = match/mismatch score
        // F[i,j,1] = gap in sequence 1 (vertical gap)
        // F[i,j,2] = gap in sequence 2 (horizontal gap)
        int[,,] F = new int[m + 1, n + 1, 3];
        
        // Initialize first row and column
        for (int i = 1; i <= m; i++)
        {
            F[i, 0, 0] = int.MinValue;
            F[i, 0, 1] = gapOpen + (i - 1) * gapExtend;
            F[i, 0, 2] = int.MinValue;
        }
        
        for (int j = 1; j <= n; j++)
        {
            F[0, j, 0] = int.MinValue;
            F[0, j, 1] = int.MinValue;
            F[0, j, 2] = gapOpen + (j - 1) * gapExtend;
        }
        
        // Fill the DP tables
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                // Match/mismatch score
                int matchScore = F[i - 1, j - 1, 0] + scoringMatrix[seq1[i - 1], seq2[j - 1]];
                int gap1Score = F[i - 1, j - 1, 1] + scoringMatrix[seq1[i - 1], seq2[j - 1]];
                int gap2Score = F[i - 1, j - 1, 2] + scoringMatrix[seq1[i - 1], seq2[j - 1]];
                
                F[i, j, 0] = Math.Max(Math.Max(matchScore, gap1Score), gap2Score);
                
                // Gap in sequence 1 (vertical gap)
                int gap1Open = F[i - 1, j, 0] + gapOpen;
                int gap1Extend = F[i - 1, j, 1] + gapExtend;
                F[i, j, 1] = Math.Max(gap1Open, gap1Extend);
                
                // Gap in sequence 2 (horizontal gap)
                int gap2Open = F[i, j - 1, 0] + gapOpen;
                int gap2Extend = F[i, j - 1, 2] + gapExtend;
                F[i, j, 2] = Math.Max(gap2Open, gap2Extend);
            }
        }
        
        // Backtrack to find alignment
        int i = m, j = n;
        string alignment1 = "";
        string alignment2 = "";
        
        // Get the final score (maximum of all three states)
        int finalScore = Math.Max(Math.Max(F[m, n, 0], F[m, n, 1]), F[m, n, 2]);
        
        // Determine which state we ended in
        int currentState = 0;
        if (F[m, n, 1] == finalScore) currentState = 1;
        if (F[m, n, 2] == finalScore) currentState = 2;
        
        while (i > 0 || j > 0)
        {
            switch (currentState)
            {
                case 0: // Match/mismatch
                    if (i > 0 && j > 0)
                    {
                        alignment1 = seq1[i - 1] + alignment1;
                        alignment2 = seq2[j - 1] + alignment2;
                        i--;
                        j--;
                    }
                    else if (i > 0)
                    {
                        alignment1 = seq1[i - 1] + alignment1;
                        alignment2 = "-" + alignment2;
                        i--;
                    }
                    else if (j > 0)
                    {
                        alignment1 = "-" + alignment1;
                        alignment2 = seq2[j - 1] + alignment2;
                        j--;
                    }
                    break;
                    
                case 1: // Gap in sequence 1
                    if (i > 0)
                    {
                        alignment1 = seq1[i - 1] + alignment1;
                        alignment2 = "-" + alignment2;
                        i--;
                    }
                    else
                    {
                        alignment1 = "-" + alignment1;
                        alignment2 = seq2[j - 1] + alignment2;
                        j--;
                    }
                    break;
                    
                case 2: // Gap in sequence 2
                    if (j > 0)
                    {
                        alignment1 = "-" + alignment1;
                        alignment2 = seq2[j - 1] + alignment2;
                        j--;
                    }
                    else
                    {
                        alignment1 = seq1[i - 1] + alignment1;
                        alignment2 = "-" + alignment2;
                        i--;
                    }
                    break;
            }
            
            // Determine next state based on backtracking
            if (currentState == 0)
            {
                // From state 0, we can come from state 0, 1, or 2
                int maxPrev = Math.Max(Math.Max(F[i, j, 0], F[i, j, 1]), F[i, j, 2]);
                if (F[i, j, 0] == maxPrev) currentState = 0;
                else if (F[i, j, 1] == maxPrev) currentState = 1;
                else currentState = 2;
            }
            else if (currentState == 1)
            {
                // From state 1, we can come from state 0 or 1
                if (F[i, j, 0] >= F[i, j, 1]) currentState = 0;
                else currentState = 1;
            }
            else
            {
                // From state 2, we can come from state 0 or 2
                if (F[i, j, 0] >= F[i, j, 2]) currentState = 0;
                else currentState = 2;
            }
        }
        
        return (finalScore, alignment1, alignment2);
    }
    
    private static int[,] ReadScoringMatrix(string[] lines)
    {
        // Parse the scoring matrix from input lines
        int[,] matrix = new int[256, 256]; // ASCII size
        
        // Read header to get amino acid order
        string[] header = lines[0].Trim().Split();
        Dictionary<char, int> aminoAcids = new Dictionary<char, int>();
        for (int i = 0; i < header.Length; i++)
        {
            aminoAcids[header[i][0]] = i;
        }
        
        // Read matrix values
        for (int i = 1; i < lines.Length; i++)
        {
            string[] values = lines[i].Trim().Split();
            char rowChar = values[0][0];
            for (int j = 0; j < values.Length - 1; j++)
            {
                matrix[rowChar, header[j][0]] = int.Parse(values[j + 1]);
            }
        }
        
        return matrix;
    }
}
```

## Explanation

This solution implements the global alignment algorithm with affine gap penalty, which is more biologically realistic than linear gap penalties.

### Key Components:

1. **3D Dynamic Programming Table**: 
   - `F[i,j,0]`: Score when both sequences match/mismatch at position (i,j)
   - `F[i,j,1]`: Score when there's a gap in sequence 1 (vertical)
   - `F[i,j,2]`: Score when there's a gap in sequence 2 (horizontal)

2. **Gap Penalties**:
   - `gapOpen`: Penalty for opening a new gap (set to -11)
   - `gapExtend`: Penalty for extending an existing gap (set to -1)

3. **Initialization**:
   - First row and column initialized with appropriate gap penalties
   - Proper handling of gap opening vs. gap extension costs

4. **Recurrence Relations**:
   - Match/mismatch: Take maximum from previous states
   - Gap in sequence 1: Either open new gap or extend existing gap
   - Gap in sequence 2: Either open new gap or extend existing gap

5. **Backtracking**:
   - Trace back through the DP table to reconstruct the alignment
   - Properly handle transitions between gap states

### Time and Space Complexity:
- **Time**: O(m×n) where m and n are sequence lengths
- **Space**: O(m×n) for the 3D DP table

This approach correctly handles the affine gap penalty model which is more realistic for biological sequence alignment.

