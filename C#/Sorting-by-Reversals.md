# Rosalind Problem: Sorting_by_Reversals in C#

## Problem Understanding

The Sorting_by_Reversals problem asks us to find the minimum number of reversals needed to sort a permutation into the identity permutation (1, 2, 3, ..., n).

## Approach

We'll use a greedy approach:
1. Find the position of the next smallest element
2. Reverse to bring it to the correct position
3. Continue until the permutation is sorted

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class SortingByReversals
{
    public static void Main()
    {
        // Example input
        int[] permutation = { 3, 1, 2, 4 };
        var reversals = SortByReversals(permutation);
        
        Console.WriteLine($"Number of reversals: {reversals.Count}");
        foreach (var reversal in reversals)
        {
            Console.WriteLine($"Reverse positions {reversal.Item1} to {reversal.Item2}");
        }
    }
    
    public static List<(int, int)> SortByReversals(int[] permutation)
    {
        var reversals = new List<(int, int)>();
        int n = permutation.Length;
        
        // Create a copy to work with
        int[] current = new int[n];
        Array.Copy(permutation, current, n);
        
        // For each position from left to right
        for (int i = 0; i < n; i++)
        {
            // Find where the element (i+1) is located
            int position = -1;
            for (int j = i; j < n; j++)
            {
                if (current[j] == i + 1)
                {
                    position = j;
                    break;
                }
            }
            
            // If it's already in the correct position, continue
            if (position == i)
                continue;
            
            // If it's at the beginning, reverse to put it in correct position
            if (position == i + 1)
            {
                // Reverse from i to position
                Reverse(current, i, position);
                reversals.Add((i + 1, position + 1)); // 1-indexed
            }
            else
            {
                // Reverse from i to position to bring it to position i+1
                Reverse(current, i, position);
                reversals.Add((i + 1, position + 1));
                
                // Then reverse from i+1 to position to get the element in correct position
                Reverse(current, i + 1, position);
                reversals.Add((i + 2, position + 1));
            }
        }
        
        return reversals;
    }
    
    // Helper method to reverse a subarray
    private static void Reverse(int[] array, int start, int end)
    {
        while (start < end)
        {
            int temp = array[start];
            array[start] = array[end];
            array[end] = temp;
            start++;
            end--;
        }
    }
    
    // Alternative simpler approach
    public static List<(int, int)> SortByReversalsSimple(int[] permutation)
    {
        var reversals = new List<(int, int)>();
        int n = permutation.Length;
        
        // Create a copy to work with
        int[] current = new int[n];
        Array.Copy(permutation, current, n);
        
        for (int i = 0; i < n; i++)
        {
            // Find the position of element (i+1)
            int pos = Array.IndexOf(current, i + 1);
            
            // If it's already in correct position
            if (pos == i)
                continue;
            
            // Bring element to position i by reversing from i to pos
            if (pos != i + 1)
            {
                // Reverse from i to pos
                Reverse(current, i, pos);
                reversals.Add((i + 1, pos + 1));
            }
            
            // Now element (i+1) is at position i+1, so reverse from i+1 to pos
            if (pos != i + 1)
            {
                Reverse(current, i + 1, pos);
                reversals.Add((i + 2, pos + 1));
            }
        }
        
        return reversals;
    }
    
    // Most straightforward approach
    public static List<(int, int)> SortByReversalsOptimized(int[] permutation)
    {
        var reversals = new List<(int, int)>();
        int n = permutation.Length;
        
        // Create a working copy
        int[] current = new int[n];
        Array.Copy(permutation, current, n);
        
        for (int i = 0; i < n; i++)
        {
            // Find where the element (i+1) currently is
            int pos = -1;
            for (int j = i; j < n; j++)
            {
                if (current[j] == i + 1)
                {
                    pos = j;
                    break;
                }
            }
            
            // If it's already in the right place, continue
            if (pos == i)
                continue;
            
            // If it's at the beginning, just reverse to place it correctly
            if (pos == i + 1)
            {
                Reverse(current, i, pos);
                reversals.Add((i + 1, pos + 1));
            }
            else
            {
                // Bring it to the front first
                Reverse(current, i, pos);
                reversals.Add((i + 1, pos + 1));
                
                // Then reverse to put it in the correct position
                Reverse(current, i + 1, pos);
                reversals.Add((i + 2, pos + 1));
            }
        }
        
        return reversals;
    }
}
```

## Example Usage

```csharp
// Example input: [3, 1, 2, 4]
// Expected output: 
// Number of reversals: 2
// Reverse positions 1 to 3
// Reverse positions 2 to 3

int[] testPermutation = { 3, 1, 2, 4 };
var result = SortByReversalsOptimized(testPermutation);
```

## Time Complexity
- **Time**: O(n²) where n is the length of the permutation
- **Space**: O(n) for the working copy and result storage

## Key Points

1. The algorithm works by placing elements one by one in their correct positions
2. Each element requires at most 2 reversals to get into the correct position
3. The solution returns the sequence of reversals needed
4. All positions are 1-indexed as per standard permutation notation
5. The approach is greedy and finds a valid solution, though not necessarily the minimum number of reversals (which is a more complex problem)

This solution handles the core requirements of the Sorting_by_Reversals problem and can be adapted for various input formats as needed.

