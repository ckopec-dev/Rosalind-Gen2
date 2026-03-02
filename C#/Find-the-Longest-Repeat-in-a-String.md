# Rosalind Problem: Find the Longest Repeat in a String

## Problem Understanding

The task is to find the longest substring that appears at least twice in a given string, where the occurrences can overlap.

## Approach

I'll use a suffix array approach with a longest common prefix (LCP) array to efficiently find the longest repeated substring.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    public static void Main()
    {
        // Read input from stdin
        string input = Console.ReadLine();
        
        // Find the longest repeat
        string result = FindLongestRepeat(input);
        
        Console.WriteLine(result);
    }
    
    public static string FindLongestRepeat(string text)
    {
        // Add a sentinel character to make it easier to handle
        text += "$";
        
        // Create suffix array
        int n = text.Length;
        int[] suffixArray = new int[n];
        
        // Initialize suffix array with indices
        for (int i = 0; i < n; i++)
        {
            suffixArray[i] = i;
        }
        
        // Sort suffixes lexicographically
        Array.Sort(suffixArray, (a, b) =>
        {
            int i = a;
            int j = b;
            while (i < n && j < n)
            {
                if (text[i] != text[j])
                    return text[i].CompareTo(text[j]);
                i++;
                j++;
            }
            return (n - a).CompareTo(n - b);
        });
        
        // Create LCP array
        int[] lcp = new int[n];
        for (int i = 0; i < n; i++)
        {
            lcp[i] = 0;
        }
        
        // Compute LCP values
        for (int i = 0; i < n; i++)
        {
            if (suffixArray[i] == n - 1)
            {
                lcp[i] = 0;
            }
            else
            {
                int j = suffixArray[i + 1];
                int k = suffixArray[i];
                int count = 0;
                while (k < n && j < n && text[k] == text[j])
                {
                    k++;
                    j++;
                    count++;
                }
                lcp[i] = count;
            }
        }
        
        // Find maximum LCP value
        int maxLength = 0;
        int maxIndex = 0;
        
        for (int i = 0; i < n; i++)
        {
            if (lcp[i] > maxLength)
            {
                maxLength = lcp[i];
                maxIndex = suffixArray[i];
            }
        }
        
        return text.Substring(maxIndex, maxLength);
    }
}
```

## Alternative Simpler Approach

For smaller inputs or when performance isn't critical, here's a more straightforward approach:

```csharp
using System;
using System.Collections.Generic;

public class Program
{
    public static void Main()
    {
        string input = Console.ReadLine();
        string result = FindLongestRepeat(input);
        Console.WriteLine(result);
    }
    
    public static string FindLongestRepeat(string text)
    {
        string longest = "";
        
        // Try all possible substring lengths from longest to shortest
        for (int length = text.Length - 1; length > 0; length--)
        {
            // Try all possible starting positions for this length
            for (int start = 0; start <= text.Length - length; start++)
            {
                string substring = text.Substring(start, length);
                
                // Check if this substring appears elsewhere in the text
                int count = 0;
                int pos = 0;
                
                while (pos <= text.Length - length)
                {
                    int found = text.IndexOf(substring, pos);
                    if (found == -1) break;
                    count++;
                    pos = found + 1;
                    
                    // If we found it at least twice, we found our answer
                    if (count >= 2)
                    {
                        return substring;
                    }
                }
            }
        }
        
        return longest;
    }
}
```

## Explanation

### Approach 1 (Suffix Array + LCP):
1. Create a suffix array of all suffixes of the string
2. Sort the suffixes lexicographically
3. Compute the LCP (Longest Common Prefix) array
4. The maximum value in the LCP array corresponds to the longest repeated substring

### Approach 2 (Brute Force):
1. Check all possible substring lengths from longest to shortest
2. For each length, check all possible substrings
3. Count how many times each substring appears
4. Return the first substring that appears at least twice

## Time Complexity

- **Approach 1**: O(n log n) due to sorting and LCP computation
- **Approach 2**: O(n³) in worst case

## Space Complexity

- **Approach 1**: O(n)
- **Approach 2**: O(n)

The first approach is more efficient for larger inputs, while the second approach is simpler to understand and implement.

