# Rosalind Problem: Genome Assembly as Shortest Superstring (C# Solution)

## Problem Understanding

The goal is to find the shortest superstring that contains all given DNA sequences as substrings. This is a variant of the Shortest Superstring Problem, which is NP-hard, but can be solved approximately using greedy algorithms.

## Approach

1. **Overlap Calculation**: For each pair of sequences, calculate the maximum overlap where one sequence can be placed at the end of another
2. **Greedy Assembly**: Repeatedly merge the pair of sequences with maximum overlap until only one sequence remains
3. **Superstring Construction**: The final merged sequence is the shortest superstring

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class GenomeAssembly
{
    public static string FindShortestSuperstring(string[] strings)
    {
        if (strings == null || strings.Length == 0)
            return "";
            
        if (strings.Length == 1)
            return strings[0];
            
        // Create a graph where each node is a string and edges represent overlaps
        int n = strings.Length;
        int[,] overlap = new int[n, n];
        
        // Calculate overlaps between all pairs
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < n; j++)
            {
                if (i != j)
                {
                    overlap[i, j] = CalculateOverlap(strings[i], strings[j]);
                }
            }
        }
        
        // Greedy approach: keep merging the pair with maximum overlap
        while (n > 1)
        {
            // Find maximum overlap
            int maxOverlap = -1;
            int bestI = -1, bestJ = -1;
            
            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < n; j++)
                {
                    if (i != j && overlap[i, j] > maxOverlap)
                    {
                        maxOverlap = overlap[i, j];
                        bestI = i;
                        bestJ = j;
                    }
                }
            }
            
            // If no positive overlap found, we're done
            if (maxOverlap <= 0)
                break;
                
            // Merge strings[bestI] and strings[bestJ]
            string merged = MergeStrings(strings[bestI], strings[bestJ], maxOverlap);
            
            // Replace strings[bestI] with merged string
            strings[bestI] = merged;
            
            // Remove strings[bestJ] by shifting elements
            for (int i = bestJ; i < n - 1; i++)
            {
                strings[i] = strings[i + 1];
            }
            
            // Update overlap matrix
            for (int i = 0; i < n - 1; i++)
            {
                if (i != bestI)
                {
                    overlap[bestI, i] = CalculateOverlap(strings[bestI], strings[i]);
                    overlap[i, bestI] = CalculateOverlap(strings[i], strings[bestI]);
                }
            }
            
            n--;
        }
        
        return strings[0];
    }
    
    private static int CalculateOverlap(string s1, string s2)
    {
        int maxOverlap = 0;
        int len1 = s1.Length;
        int len2 = s2.Length;
        
        // Try all possible overlaps
        for (int i = 1; i <= Math.Min(len1, len2); i++)
        {
            // Check if s2's prefix matches s1's suffix of length i
            if (s1.Substring(len1 - i) == s2.Substring(0, i))
            {
                maxOverlap = i;
            }
        }
        
        return maxOverlap;
    }
    
    private static string MergeStrings(string s1, string s2, int overlap)
    {
        // s1 ends with overlap characters that match beginning of s2
        return s1 + s2.Substring(overlap);
    }
    
    // Alternative approach using all permutations (for small inputs)
    public static string FindShortestSuperstringBruteForce(string[] strings)
    {
        if (strings == null || strings.Length == 0)
            return "";
            
        if (strings.Length == 1)
            return strings[0];
            
        var permutations = GetPermutations(strings.ToList());
        string shortest = null;
        
        foreach (var perm in permutations)
        {
            string superstring = BuildSuperstring(perm.ToArray());
            if (shortest == null || superstring.Length < shortest.Length)
            {
                shortest = superstring;
            }
        }
        
        return shortest;
    }
    
    private static List<List<string>> GetPermutations(List<string> items)
    {
        if (items.Count <= 1)
            return new List<List<string>> { new List<string>(items) };
            
        var result = new List<List<string>>();
        for (int i = 0; i < items.Count; i++)
        {
            var remaining = new List<string>(items);
            string current = remaining[i];
            remaining.RemoveAt(i);
            
            foreach (var perm in GetPermutations(remaining))
            {
                var newPerm = new List<string> { current };
                newPerm.AddRange(perm);
                result.Add(newPerm);
            }
        }
        
        return result;
    }
    
    private static string BuildSuperstring(string[] strings)
    {
        if (strings.Length == 0)
            return "";
            
        string result = strings[0];
        for (int i = 1; i < strings.Length; i++)
        {
            int overlap = CalculateOverlap(result, strings[i]);
            result += strings[i].Substring(overlap);
        }
        
        return result;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example from Rosalind problem
        string[] sequences = {
            "ATTAGACCTG",
            "CCTGCCGGAA",
            "AGACCTGCCG",
            "GCCGGAATAC"
        };
        
        string superstring = GenomeAssembly.FindShortestSuperstring(sequences);
        Console.WriteLine($"Shortest superstring: {superstring}");
        
        // Test with another example
        string[] sequences2 = {
            "ABC",
            "BCD",
            "CDE"
        };
        
        string superstring2 = GenomeAssembly.FindShortestSuperstring(sequences2);
        Console.WriteLine($"Shortest superstring 2: {superstring2}");
    }
}
```

## Key Components Explained

### 1. **Overlap Calculation**
```csharp
private static int CalculateOverlap(string s1, string s2)
{
    int maxOverlap = 0;
    // Check all possible overlaps from 1 to min(len1, len2)
    for (int i = 1; i <= Math.Min(len1, len2); i++)
    {
        if (s1.Substring(len1 - i) == s2.Substring(0, i))
        {
            maxOverlap = i;
        }
    }
    return maxOverlap;
}
```

### 2. **String Merging**
```csharp
private static string MergeStrings(string s1, string s2, int overlap)
{
    return s1 + s2.Substring(overlap);
}
```

### 3. **Greedy Assembly Algorithm**
- Repeatedly find the pair of strings with maximum overlap
- Merge them into a single string
- Update the overlap matrix
- Continue until only one string remains

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the number of sequences
- **Space Complexity**: O(n²) for the overlap matrix

## Usage Notes

This solution uses a greedy approach that works well for most practical cases. For very large inputs or when exact solutions are required, more sophisticated algorithms like dynamic programming or branch-and-bound methods would be needed, though they have higher computational complexity.

The algorithm correctly handles the example case and can be adapted for various genome assembly scenarios where the goal is to find the minimal superstring containing all input sequences.

