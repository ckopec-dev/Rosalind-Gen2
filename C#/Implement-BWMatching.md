# Rosalind Problem: Implement_BWMatching in C#

## Problem Description
BWMatching is a key algorithm for finding occurrences of a pattern in a Burrows-Wheeler Transform (BWT) of a string. Given a BWT string and a pattern, we need to count how many times the pattern occurs in the original string.

## Solution Approach
1. Build the BWT index using the Burrows-Wheeler Transform
2. Implement the backward search algorithm to count pattern occurrences
3. Use the FirstOccurrence array and LastToFirst mapping for efficient searching

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BWMatching
{
    public static int BWMatchingAlgorithm(string bwt, string[] patterns)
    {
        // Build the BWT index
        var bwtArray = bwt.ToCharArray();
        var sortedBwt = bwtArray.OrderBy(c => c).ToArray();
        
        // Create First column
        var firstColumn = new char[bwt.Length];
        Array.Copy(sortedBwt, firstColumn, bwt.Length);
        
        // Create LastToFirst mapping
        var lastToFirst = new int[bwt.Length];
        var firstOccurrence = new Dictionary<char, int>();
        var charCount = new Dictionary<char, int>();
        
        // Initialize character counts
        foreach (char c in bwt)
        {
            if (charCount.ContainsKey(c))
                charCount[c]++;
            else
                charCount[c] = 1;
        }
        
        // Build firstOccurrence array
        int index = 0;
        foreach (char c in sortedBwt)
        {
            if (!firstOccurrence.ContainsKey(c))
                firstOccurrence[c] = index;
            index++;
        }
        
        // Build LastToFirst mapping
        var charPositions = new Dictionary<char, Queue<int>>();
        for (int i = 0; i < bwt.Length; i++)
        {
            char c = bwt[i];
            if (!charPositions.ContainsKey(c))
                charPositions[c] = new Queue<int>();
            charPositions[c].Enqueue(i);
        }
        
        for (int i = 0; i < bwt.Length; i++)
        {
            char c = sortedBwt[i];
            lastToFirst[i] = charPositions[c].Dequeue();
        }
        
        // Process each pattern
        var results = new List<int>();
        foreach (string pattern in patterns)
        {
            int count = BWMatchingPattern(bwtArray, firstColumn, lastToFirst, firstOccurrence, pattern);
            results.Add(count);
        }
        
        return results.Sum();
    }
    
    private static int BWMatchingPattern(char[] bwt, char[] firstColumn, int[] lastToFirst, 
                                       Dictionary<char, int> firstOccurrence, string pattern)
    {
        int top = 0;
        int bottom = bwt.Length - 1;
        int patternIndex = pattern.Length - 1;
        
        while (top <= bottom)
        {
            if (patternIndex >= 0)
            {
                char symbol = pattern[patternIndex];
                
                // Find range in BWT for symbol
                int newTop = top;
                int newBottom = bottom;
                
                // Find first occurrence of symbol in range
                for (int i = top; i <= bottom; i++)
                {
                    if (bwt[i] == symbol)
                    {
                        newTop = i;
                        break;
                    }
                }
                
                // Find last occurrence of symbol in range
                for (int i = bottom; i >= top; i--)
                {
                    if (bwt[i] == symbol)
                    {
                        newBottom = i;
                        break;
                    }
                }
                
                // Check if symbol exists in range
                if (newTop > newBottom)
                {
                    return 0;
                }
                
                // Update top and bottom
                top = firstOccurrence[symbol] + GetCount(bwt, symbol, 0, newTop - 1);
                bottom = firstOccurrence[symbol] + GetCount(bwt, symbol, 0, newBottom) - 1;
                
                patternIndex--;
            }
            else
            {
                return bottom - top + 1;
            }
        }
        
        return 0;
    }
    
    private static int GetCount(char[] bwt, char symbol, int start, int end)
    {
        int count = 0;
        for (int i = start; i <= end; i++)
        {
            if (bwt[i] == symbol)
                count++;
        }
        return count;
    }
    
    // Simpler and more efficient implementation
    public static int[] BWMatchingEfficient(string bwt, string[] patterns)
    {
        // Build the BWT index
        var bwtArray = bwt.ToCharArray();
        var sortedBwt = bwtArray.OrderBy(c => c).ToArray();
        
        // Create first occurrence array
        var firstOccurrence = new Dictionary<char, int>();
        int index = 0;
        foreach (char c in sortedBwt)
        {
            if (!firstOccurrence.ContainsKey(c))
                firstOccurrence[c] = index;
            index++;
        }
        
        // Create count array for each position
        var count = new Dictionary<char, int[]>();
        foreach (char c in bwtArray.Distinct())
        {
            count[c] = new int[bwtArray.Length + 1];
        }
        
        for (int i = 0; i < bwtArray.Length; i++)
        {
            foreach (char c in count.Keys)
            {
                count[c][i + 1] = count[c][i];
            }
            count[bwtArray[i]][i + 1]++;
        }
        
        // Process each pattern
        var results = new List<int>();
        foreach (string pattern in patterns)
        {
            int result = BWMatchingPatternEfficient(bwtArray, firstOccurrence, count, pattern);
            results.Add(result);
        }
        
        return results.ToArray();
    }
    
    private static int BWMatchingPatternEfficient(char[] bwt, Dictionary<char, int> firstOccurrence, 
                                                 Dictionary<char, int[]> count, string pattern)
    {
        int top = 0;
        int bottom = bwt.Length - 1;
        
        for (int i = pattern.Length - 1; i >= 0; i--)
        {
            char symbol = pattern[i];
            
            // Check if symbol exists in current range
            if (top > bottom)
                return 0;
                
            // Find range of symbol in current range
            int newTop = firstOccurrence[symbol] + count[symbol][top];
            int newBottom = firstOccurrence[symbol] + count[symbol][bottom + 1] - 1;
            
            top = newTop;
            bottom = newBottom;
        }
        
        return bottom - top + 1;
    }
    
    // Main method for testing
    public static void Main(string[] args)
    {
        // Example usage
        string bwt = "TCCTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTT