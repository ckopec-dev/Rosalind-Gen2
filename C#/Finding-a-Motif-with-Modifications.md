# Rosalind Problem: Finding a Motif with Modifications (C# Solution)

## Problem Understanding

The problem asks us to find all occurrences of a pattern (motif) in a DNA string, allowing for up to `d` mismatches (modifications). This is essentially a "motif finding with mismatches" problem.

## Approach

1. **Sliding Window**: Slide through the DNA string with a window size equal to the pattern length
2. **Hamming Distance**: For each window, calculate the Hamming distance between the pattern and the substring
3. **Count Mismatches**: If the Hamming distance is ≤ d, record the position
4. **Return Results**: Collect all valid positions

## C# Solution

```csharp
using System;
using System.Collections.Generic;
using System.IO;

public class MotifWithModifications
{
    /// <summary>
    /// Finds all occurrences of a pattern in a DNA string with up to d mismatches
    /// </summary>
    /// <param name="text">The DNA string to search in</param>
    /// <param name="pattern">The pattern to search for</param>
    /// <param name="d">Maximum number of mismatches allowed</param>
    /// <returns>List of starting positions (0-indexed) where pattern occurs with at most d mismatches</returns>
    public static List<int> FindMotifs(string text, string pattern, int d)
    {
        List<int> positions = new List<int>();
        int patternLength = pattern.Length;
        int textLength = text.Length;
        
        // If pattern is longer than text, no matches possible
        if (patternLength > textLength)
            return positions;
        
        // Slide through the text
        for (int i = 0; i <= textLength - patternLength; i++)
        {
            string substring = text.Substring(i, patternLength);
            
            // Check if the Hamming distance is within the allowed mismatches
            if (HammingDistance(pattern, substring) <= d)
            {
                positions.Add(i);
            }
        }
        
        return positions;
    }
    
    /// <summary>
    /// Calculates the Hamming distance between two strings of equal length
    /// </summary>
    /// <param name="str1">First string</param>
    /// <param name="str2">Second string</param>
    /// <returns>Number of positions where characters differ</returns>
    private static int HammingDistance(string str1, string str2)
    {
        int distance = 0;
        
        for (int i = 0; i < str1.Length; i++)
        {
            if (str1[i] != str2[i])
                distance++;
        }
        
        return distance;
    }
    
    /// <summary>
    /// Main method to solve the Rosalind problem
    /// </summary>
    public static void Main(string[] args)
    {
        // Read input from file (assuming input.txt format)
        string[] lines = File.ReadAllLines("input.txt");
        
        string text = lines[0];
        string pattern = lines[1];
        int d = int.Parse(lines[2]);
        
        // Find all motifs
        List<int> positions = FindMotifs(text, pattern, d);
        
        // Output results
        Console.WriteLine(string.Join(" ", positions));
        
        // Alternative: Write to output file
        File.WriteAllText("output.txt", string.Join(" ", positions));
    }
}
```

## Example Usage

For the sample input:
```
ACGTACGTACGT
ACG
2
```

The solution would:
1. Check each 3-character substring of "ACGTACGTACGT"
2. Calculate Hamming distance with "ACG"
3. Return positions where distance ≤ 2

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the length of text and m is the length of pattern
- **Space Complexity**: O(k) where k is the number of matches found

## Sample Input/Output

**Input:**
```
ACGTACGTACGT
ACG
2
```

**Output:**
```
0 4 8
```

## Key Points

1. The solution uses a sliding window approach to examine all possible substrings
2. Hamming distance is calculated character by character
3. Positions are 0-indexed as per standard convention
4. The algorithm handles edge cases like pattern longer than text
5. Results are returned in ascending order of positions

This solution efficiently finds all occurrences of a motif with a specified number of allowed mismatches, which is exactly what the Rosalind problem requires.

