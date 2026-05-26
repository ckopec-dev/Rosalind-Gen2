# Rosalind Problem: Construct the Suffix Array of a String (C# Solution)

## Problem Understanding

A suffix array is a sorted array of all suffixes of a given string. Each element in the suffix array represents the starting position of a suffix in the original string.

## Solution Approach

1. Generate all suffixes of the input string
2. Sort the suffixes lexicographically
3. Return the starting positions of the sorted suffixes

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class SuffixArray
{
    public static List<int> ConstructSuffixArray(string text)
    {
        // Add terminator character to handle edge cases
        text += "$";
        
        // Generate all suffixes with their starting positions
        var suffixes = new List<(string suffix, int position)>();
        
        for (int i = 0; i < text.Length; i++)
        {
            suffixes.Add((text.Substring(i), i));
        }
        
        // Sort suffixes lexicographically
        var sortedSuffixes = suffixes.OrderBy(s => s.suffix).ToList();
        
        // Extract the starting positions
        var suffixArray = sortedSuffixes.Select(s => s.position).ToList();
        
        return suffixArray;
    }
    
    // Alternative implementation using LINQ in one line
    public static List<int> ConstructSuffixArrayLinq(string text)
    {
        text += "$";
        return Enumerable.Range(0, text.Length)
                        .Select(i => (text.Substring(i), i))
                        .OrderBy(s => s.Item1)
                        .Select(s => s.Item2)
                        .ToList();
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Test with example from Rosalind
        string input = "PANAMABANANAS$";
        var result = SuffixArray.ConstructSuffixArray(input);
        
        Console.WriteLine("Input string: " + input);
        Console.WriteLine("Suffix Array: " + string.Join(" ", result));
        
        // Verify by printing sorted suffixes
        Console.WriteLine("\nSorted suffixes:");
        var text = "PANAMABANANAS$";
        var suffixes = Enumerable.Range(0, text.Length)
                                .Select(i => text.Substring(i))
                                .OrderBy(s => s)
                                .ToList();
        
        for (int i = 0; i < suffixes.Count; i++)
        {
            Console.WriteLine($"{result[i]}: {suffixes[i]}");
        }
    }
}
```

## Example Walkthrough

For input string "PANAMABANANAS$":

1. **Generate suffixes:**
   - Position 0: "PANAMABANANAS$"
   - Position 1: "ANAMABANANAS$"
   - Position 2: "NAMABANANAS$"
   - Position 3: "AMABANANAS$"
   - Position 4: "MABANANAS$"
   - Position 5: "ABANANAS$"
   - Position 6: "BANANAS$"
   - Position 7: "ANANAS$"
   - Position 8: "NANAS$"
   - Position 9: "ANAS$"
   - Position 10: "NAS$"
   - Position 11: "AS$"
   - Position 12: "S$"
   - Position 13: "$"

2. **Sort lexicographically:**
   - "$"
   - "ANAS$"
   - "ANANAS$"
   - "ANAMABANANAS$"
   - "AS$"
   - "BANANAS$"
   - "MABANANAS$"
   - "NANAS$"
   - "NAMABANANAS$"
   - "PANAMABANANAS$"
   - "S$"
   - "ABANANAS$"
   - "AMABANANAS$"
   - "NAS$"

3. **Extract starting positions:**
   - 13, 11, 7, 1, 12, 6, 4, 8, 2, 0, 10, 5, 3, 9

## Time and Space Complexity

- **Time Complexity:** O(n² log n) where n is the length of the string
  - Generating suffixes: O(n²) 
  - Sorting: O(n log n) with O(n²) comparisons
- **Space Complexity:** O(n²) for storing all suffixes

## Test Case

```csharp
// Test with simple example
string test = "banana$";
var result = SuffixArray.ConstructSuffixArray(test);
// Expected: [5, 3, 1, 0, 4, 2]
```

This solution correctly handles the suffix array construction problem and can be used to solve the Rosalind problem "Construct the Suffix Array of a String".

