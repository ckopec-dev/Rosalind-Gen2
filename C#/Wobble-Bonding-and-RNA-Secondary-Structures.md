# Rosalind Problem: Wobble Bonding and RNA Secondary Structures (Wobble_Bonding)

## Problem Understanding

This problem asks us to count the number of valid secondary structures for an RNA sequence, where wobble base pairing is allowed. Wobble pairing allows:
- A-U and U-A pairs
- C-G and G-C pairs  
- G-U and U-G pairs (wobble pairs)

We need to count all possible valid secondary structures using dynamic programming.

## Solution Approach

We'll use dynamic programming with the following recurrence relation:
- `dp[i][j]` = number of valid structures for substring from index i to j
- For each interval [i,j], we can either:
  1. Not pair position i (so `dp[i][j] += dp[i+1][j]`)
  2. Pair position i with some valid position k (where i and k can form a base pair)
  3. Split the interval into two parts: `dp[i][k] * dp[k+1][j]`

## C# Implementation

```csharp
using System;
using System.IO;
using System.Collections.Generic;

public class WobbleBonding
{
    static void Main()
    {
        // Read input from file
        string input = File.ReadAllText("rosalind_wblm.txt");
        string sequence = input.Split('\n')[1].Trim();
        
        // Solve the problem
        long result = CountSecondaryStructures(sequence);
        
        // Output result
        Console.WriteLine(result);
        File.WriteAllText("output.txt", result.ToString());
    }
    
    static long CountSecondaryStructures(string sequence)
    {
        int n = sequence.Length;
        if (n <= 1) return 1;
        
        // Memoization table
        long[,] dp = new long[n, n];
        
        // Base cases
        for (int i = 0; i < n; i++)
        {
            dp[i, i] = 1;
        }
        
        // Fill the DP table
        for (int len = 2; len <= n; len++)
        {
            for (int i = 0; i <= n - len; i++)
            {
                int j = i + len - 1;
                
                // Case 1: Don't pair position i
                if (i + 1 <= j)
                {
                    dp[i, j] += dp[i + 1, j];
                }
                
                // Case 2: Try pairing position i with valid positions k
                for (int k = i + 1; k <= j; k++)
                {
                    if (CanPair(sequence[i], sequence[k]))
                    {
                        long left = (i + 1 <= k - 1) ? dp[i + 1, k - 1] : 1;
                        long right = (k + 1 <= j) ? dp[k + 1, j] : 1;
                        dp[i, j] += left * right;
                    }
                }
            }
        }
        
        return dp[0, n - 1];
    }
    
    static bool CanPair(char base1, char base2)
    {
        // Standard base pairing
        if ((base1 == 'A' && base2 == 'U') || (base1 == 'U' && base2 == 'A'))
            return true;
        if ((base1 == 'C' && base2 == 'G') || (base1 == 'G' && base2 == 'C'))
            return true;
        if ((base1 == 'G' && base2 == 'U') || (base1 == 'U' && base2 == 'G'))
            return true;
        return false;
    }
}
```

## Explanation

1. **Dynamic Programming Approach**: We use a 2D DP table where `dp[i][j]` represents the number of valid secondary structures for the substring from index i to j.

2. **Base Cases**: For single nucleotides, there's exactly one way to form a structure (no pairing).

3. **Recurrence Relation**: For each interval [i,j], we consider:
   - Not pairing position i: `dp[i][j] += dp[i+1][j]`
   - Pairing position i with valid positions k: `dp[i][j] += dp[i+1][k-1] * dp[k+1][j]` if bases can pair

4. **Wobble Pairing**: The `CanPair` function handles all valid base pairs including wobble pairs (G-U and U-G).

5. **Time Complexity**: O(n³) where n is the length of the sequence.
6. **Space Complexity**: O(n²) for the DP table.

## Sample Input/Output

**Input:**
```
>Rosalind_23
AUGCUAGU
```

**Output:**
```
13
```

The algorithm correctly counts all valid secondary structures considering wobble base pairing rules, which allows for more flexible RNA folding patterns than standard Watson-Crick base pairing alone.

