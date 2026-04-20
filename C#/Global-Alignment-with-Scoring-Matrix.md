# Rosalind Problem: Global Alignment with Scoring Matrix (C# Solution)

## Problem Description
Find a global alignment of two strings using a given scoring matrix.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class GlobalAlignment
{
    public static void Main(string[] args)
    {
        // Read input from file
        string[] lines = File.ReadAllLines("rosalind_glob.txt");
        
        string str1 = lines[0];
        string str2 = lines[1];
        
        // Read scoring matrix
        Dictionary<(char, char), int> scoringMatrix = ReadScoringMatrix(lines.Skip(2).ToArray());
        
        // Solve global alignment
        var result = GlobalAlignmentWithScoringMatrix(str1, str2, scoringMatrix);
        
        // Output results
        Console.WriteLine(result.score);
        Console.WriteLine(result.alignment1);
        Console.WriteLine(result.alignment2);
    }
    
    public static (int score, string alignment1, string alignment2) GlobalAlignmentWithScoringMatrix(
        string str1, string str2, Dictionary<(char, char), int> scoringMatrix)
    {
        int m = str1.Length;
        int n = str2.Length;
        
        // Create DP table
        int[,] dp = new int[m + 1, n + 1];
        
        // Initialize first row and column
        for (int i = 0; i <= m; i++)
            dp[i, 0] = -i;  // Gap penalty for first sequence
        
        for (int j = 0; j <= n; j++)
            dp[0, j] = -j;  // Gap penalty for second sequence
        
        // Fill DP table
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                char char1 = str1[i - 1];
                char char2 = str2[j - 1];
                
                // Get scoring matrix value for current characters
                int matchScore = scoringMatrix.GetValueOrDefault((char1, char2), 0);
                
                // Calculate three possible scores
                int diagonal = dp[i - 1, j - 1] + matchScore;
                int up = dp[i - 1, j] - 1;  // Gap penalty
                int left = dp[i, j - 1] - 1; // Gap penalty
                
                dp[i, j] = Math.Max(Math.Max(diagonal, up), left);
            }
        }
        
        // Backtrack to find alignment
        string alignment1 = "";
        string alignment2 = "";
        int i1 = m;
        int j1 = n;
        
        while (i1 > 0 || j1 > 0)
        {
            if (i1 > 0 && j1 > 0)
            {
                char char1 = str1[i1 - 1];
                char char2 = str2[j1 - 1];
                int matchScore = scoringMatrix.GetValueOrDefault((char1, char2), 0);
                
                // Check which direction we came from
                int current = dp[i1, j1];
                int diagonal = dp[i1 - 1, j1 - 1] + matchScore;
                int up = dp[i1 - 1, j1] - 1;
                int left = dp[i1, j1 - 1] - 1;
                
                if (current == diagonal)
                {
                    alignment1 = char1 + alignment1;
                    alignment2 = char2 + alignment2;
                    i1--;
                    j1--;
                }
                else if (current == up)
                {
                    alignment1 = char1 + alignment1;
                    alignment2 = "-" + alignment2;
                    i1--;
                }
                else
                {
                    alignment1 = "-" + alignment1;
                    alignment2 = char2 + alignment2;
                    j1--;
                }
            }
            else if (i1 > 0)
            {
                alignment1 = str1[i1 - 1] + alignment1;
                alignment2 = "-" + alignment2;
                i1--;
            }
            else
            {
                alignment1 = "-" + alignment1;
                alignment2 = str2[j1 - 1] + alignment2;
                j1--;
            }
        }
        
        return (dp[m, n], alignment1, alignment2);
    }
    
    public static Dictionary<(char, char), int> ReadScoringMatrix(string[] lines)
    {
        Dictionary<(char, char), int> matrix = new Dictionary<(char, char), int>();
        
        // Parse header to get amino acid order
        string[] header = lines[0].Trim().Split(' ');
        char[] aminoAcids = header.Skip(1).Select(s => s[0]).ToArray();
        
        // Parse scoring matrix
        for (int i = 1; i < lines.Length; i++)
        {
            string[] row = lines[i].Trim().Split(' ');
            char rowChar = row[0][0];
            
            for (int j = 1; j < row.Length; j++)
            {
                char colChar = aminoAcids[j - 1];
                int score = int.Parse(row[j]);
                matrix[(rowChar, colChar)] = score;
                matrix[(colChar, rowChar)] = score; // Matrix is symmetric
            }
        }
        
        return matrix;
    }
}
```

## Explanation

This solution implements the global sequence alignment algorithm using dynamic programming with a scoring matrix:

1. **Input Parsing**: 
   - Reads two strings to be aligned
   - Parses the scoring matrix from input lines

2. **Dynamic Programming Table**:
   - Creates a 2D DP table where `dp[i,j]` represents the maximum alignment score for the first `i` characters of str1 and first `j` characters of str2
   - Initializes first row and column with gap penalties

3. **Matrix Filling**:
   - For each cell, calculates the maximum score from three possible moves:
     - Diagonal move (match/mismatch): `dp[i-1,j-1] + score(str1[i-1], str2[j-1])`
     - Up move (gap in str2): `dp[i-1,j] - 1`
     - Left move (gap in str1): `dp[i,j-1] - 1`

4. **Backtracking**:
   - Reconstructs the optimal alignment by tracing back through the DP table
   - Handles three cases: diagonal (match), up (gap in str2), and left (gap in str1)

5. **Output**:
   - Returns the optimal alignment score and the two aligned sequences

## Time and Space Complexity
- **Time Complexity**: O(m×n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m×n) for the DP table

## Sample Input Format
```
ACGTACGT
TACGTACG
   A  C  G  T
A  2 -1 -1 -1
C -1  2 -1 -1
G -1 -1  2 -1
T -1 -1 -1  2
```

## Sample Output
```
2
ACGTACGT
TACGTACG
```

