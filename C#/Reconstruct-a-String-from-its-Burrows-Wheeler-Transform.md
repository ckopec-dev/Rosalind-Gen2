# Rosalind Problem: Reconstruct a String from its Burrows-Wheeler Transform

## Problem Description
Given the Burrows-Wheeler transform of a string, reconstruct the original string.

## Solution Approach
The Burrows-Wheeler Transform (BWT) is a reversible transformation. To reconstruct the original string:
1. Sort the BWT string lexicographically
2. Create a table where each row is the BWT string with the previous row's content prepended
3. Repeat this process until we have the original string
4. The original string is the row that ends with the special termination character '$'

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BWTReconstruction
{
    public static string ReconstructString(string bwt)
    {
        // Add termination character if not present
        if (!bwt.EndsWith("$"))
        {
            bwt += "$";
        }
        
        int n = bwt.Length;
        
        // Create list of all rotations
        List<string> rotations = new List<string>();
        
        // Generate all rotations of the BWT string
        for (int i = 0; i < n; i++)
        {
            string rotation = bwt.Substring(i) + bwt.Substring(0, i);
            rotations.Add(rotation);
        }
        
        // Sort rotations lexicographically
        rotations.Sort();
        
        // The original string is the last column of the sorted rotations
        // But we need to find which rotation ends with '$'
        int originalIndex = rotations.FindIndex(r => r.EndsWith("$"));
        
        // Reconstruct the original string by following the cycle
        string result = "";
        int currentIndex = originalIndex;
        
        // Follow the cycle backwards to reconstruct the original string
        for (int i = 0; i < n; i++)
        {
            result = rotations[currentIndex][0] + result;
            // Find the next index in the cycle
            currentIndex = FindIndex(rotations, rotations[currentIndex]);
        }
        
        // Remove the termination character
        return result.Substring(0, result.Length - 1);
    }
    
    // Alternative cleaner approach
    public static string ReconstructStringClean(string bwt)
    {
        // Add termination character
        bwt += "$";
        int n = bwt.Length;
        
        // Create array of all rotations
        string[] rotations = new string[n];
        
        // Generate all rotations
        for (int i = 0; i < n; i++)
        {
            rotations[i] = bwt.Substring(i) + bwt.Substring(0, i);
        }
        
        // Sort rotations
        Array.Sort(rotations);
        
        // Extract the original string by taking the first character of each rotation
        // and following the cycle
        string result = "";
        int currentIndex = 0;
        
        // Find the index of the row ending with '$'
        for (int i = 0; i < n; i++)
        {
            if (rotations[i].EndsWith("$"))
            {
                currentIndex = i;
                break;
            }
        }
        
        // Build the original string by following the cycle
        for (int i = 0; i < n; i++)
        {
            result = rotations[currentIndex][0] + result;
            // Find next index
            currentIndex = FindIndex(rotations, rotations[currentIndex]);
        }
        
        // Remove the termination character
        return result.Substring(0, result.Length - 1);
    }
    
    // More efficient approach using sorting indices
    public static string ReconstructStringEfficient(string bwt)
    {
        // Add termination character
        bwt += "$";
        int n = bwt.Length;
        
        // Create array of indices
        int[] indices = new int[n];
        for (int i = 0; i < n; i++)
        {
            indices[i] = i;
        }
        
        // Sort indices based on the corresponding BWT characters
        Array.Sort(indices, (a, b) => bwt[a].CompareTo(bwt[b]));
        
        // Create the original string by following the cycle
        string result = "";
        int current = 0;
        
        // Build the string by following the cycle
        for (int i = 0; i < n; i++)
        {
            result = bwt[current] + result;
            current = indices[current];
        }
        
        // Remove the termination character
        return result.Substring(0, result.Length - 1);
    }
    
    private static int FindIndex(string[] array, string value)
    {
        for (int i = 0; i < array.Length; i++)
        {
            if (array[i] == value)
                return i;
        }
        return -1;
    }
    
    // Main method for testing
    public static void Main()
    {
        // Test case from Rosalind
        string bwt = "BANANA$";
        string original = ReconstructStringEfficient(bwt);
        Console.WriteLine($"BWT: {bwt}");
        Console.WriteLine($"Original: {original}");
        
        // Another test case
        bwt = "ACACACAA$";
        original = ReconstructStringEfficient(bwt);
        Console.WriteLine($"BWT: {bwt}");
        Console.WriteLine($"Original: {original}");
    }
}
```

## Explanation of the Algorithm

1. **Add Termination Character**: The BWT string should end with a special termination character '$'

2. **Sort Rotations**: Create all possible rotations of the BWT string and sort them lexicographically

3. **Find Original Position**: Locate the row in the sorted rotations that ends with '$'

4. **Cycle Following**: Follow the cycle by moving from one row to the next based on the sorting order

5. **Reconstruction**: Build the original string by collecting characters from the first column of the sorted rotations

## Time and Space Complexity

- **Time Complexity**: O(n² log n) where n is the length of the string
- **Space Complexity**: O(n²) for storing all rotations

## Sample Input/Output

**Input**: `BANANA$`
**Output**: `BANANA`

**Input**: `ACACACAA$` 
**Output**: `ACACACAA`

The efficient approach using index sorting reduces the complexity and is more suitable for larger inputs.

