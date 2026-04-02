# Rosalind Problem: Find the Longest Substring Shared by Two Strings

## Problem Description
Given two strings, find the longest substring that appears in both strings.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    public static void Main()
    {
        // Read input strings
        string s1 = Console.ReadLine();
        string s2 = Console.ReadLine();
        
        // Find longest common substring
        string result = FindLongestCommonSubstring(s1, s2);
        Console.WriteLine(result);
    }
    
    public static string FindLongestCommonSubstring(string s1, string s2)
    {
        if (string.IsNullOrEmpty(s1) || string.IsNullOrEmpty(s2))
            return "";
        
        int[,] dp = new int[s1.Length + 1, s2.Length + 1];
        int maxLength = 0;
        int endingIndex = 0;
        
        // Fill the DP table
        for (int i = 1; i <= s1.Length; i++)
        {
            for (int j = 1; j <= s2.Length; j++)
            {
                if (s1[i - 1] == s2[j - 1])
                {
                    dp[i, j] = dp[i - 1, j - 1] + 1;
                    
                    if (dp[i, j] > maxLength)
                    {
                        maxLength = dp[i, j];
                        endingIndex = i;
                    }
                }
                else
                {
                    dp[i, j] = 0;
                }
            }
        }
        
        // Extract the longest common substring
        if (maxLength == 0)
            return "";
            
        return s1.Substring(endingIndex - maxLength, maxLength);
    }
}
```

## Approach Explanation

This solution uses dynamic programming to find the longest common substring:

1. **DP Table Setup**: Create a 2D array `dp` where `dp[i,j]` represents the length of common substring ending at position `i-1` in string `s1` and position `j-1` in string `s2`.

2. **Fill DP Table**: 
   - If characters match: `dp[i,j] = dp[i-1,j-1] + 1`
   - If characters don't match: `dp[i,j] = 0`

3. **Track Maximum**: Keep track of the maximum length found and the ending position.

4. **Extract Result**: Use the ending position and maximum length to extract the actual substring.

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Example
Input:
```
ABABC
BABCA
```

Output:
```
BABC
```

The algorithm correctly identifies that "BABC" is the longest substring that appears in both input strings.

