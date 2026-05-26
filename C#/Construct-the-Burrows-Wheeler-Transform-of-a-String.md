# Rosalind Problem: Construct the Burrows-Wheeler Transform of a String

## Problem Description
The Burrows-Wheeler Transform (BWT) is a reversible string transformation used in data compression and bioinformatics. Given a string, the BWT is constructed by:
1. Creating all rotations of the string
2. Sorting the rotations lexicographically
3. Taking the last column of the sorted rotations

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BurrowsWheelerTransform
{
    public static string ConstructBWT(string text)
    {
        // Add termination character (typically '$')
        text += "$";
        
        // Create all rotations of the string
        List<string> rotations = new List<string>();
        for (int i = 0; i < text.Length; i++)
        {
            string rotation = text.Substring(i) + text.Substring(0, i);
            rotations.Add(rotation);
        }
        
        // Sort rotations lexicographically
        rotations.Sort();
        
        // Take the last column (BWT)
        string bwt = "";
        foreach (string rotation in rotations)
        {
            bwt += rotation[rotation.Length - 1];
        }
        
        return bwt;
    }
    
    public static void Main()
    {
        // Example usage
        string input = "GCGTGCCTGGTCA$";
        string result = ConstructBWT(input);
        Console.WriteLine($"Input: {input}");
        Console.WriteLine($"BWT: {result}");
        
        // Test with sample data from Rosalind
        string sample = "ACGT$";
        string sampleResult = ConstructBWT(sample);
        Console.WriteLine($"Sample Input: {sample}");
        Console.WriteLine($"Sample BWT: {sampleResult}");
    }
}
```

## Alternative Implementation (More Efficient)

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BurrowsWheelerTransformOptimized
{
    public static string ConstructBWT(string text)
    {
        // Add termination character
        text += "$";
        
        // Create rotations with their original indices
        var rotations = new List<(string rotation, int index)>();
        for (int i = 0; i < text.Length; i++)
        {
            string rotation = text.Substring(i) + text.Substring(0, i);
            rotations.Add((rotation, i));
        }
        
        // Sort by rotation string
        var sortedRotations = rotations.OrderBy(r => r.rotation).ToList();
        
        // Extract last column
        string bwt = new string(sortedRotations.Select(r => r.rotation[r.rotation.Length - 1]).ToArray());
        
        return bwt;
    }
    
    public static void Main()
    {
        // Test cases
        string[] testCases = {
            "ACGT$",
            "GCGTGCCTGGTCA$",
            "BANANA$"
        };
        
        foreach (string testCase in testCases)
        {
            string result = ConstructBWT(testCase);
            Console.WriteLine($"Input: {testCase}");
            Console.WriteLine($"BWT: {result}");
            Console.WriteLine();
        }
    }
}
```

## Explanation

1. **Input Preparation**: Add a termination character (`$`) to the input string to handle the circular nature properly.

2. **Rotation Generation**: Create all possible rotations of the string by taking substrings from each position.

3. **Sorting**: Sort all rotations lexicographically (dictionary order).

4. **BWT Construction**: Extract the last character from each sorted rotation to form the BWT.

## Example Walkthrough

For input string "ACGT$":
1. Rotations: 
   - "ACGT$"
   - "CGT$A"
   - "GT$AC"
   - "T$ACG"
   - "$ACGT"
2. Sorted rotations:
   - "$ACGT"
   - "ACGT$"
   - "CGT$A"
   - "GT$AC"
   - "T$ACG"
3. Last column: "$ACGT" → BWT = "$ACGT"

## Time and Space Complexity

- **Time Complexity**: O(n² log n) where n is the length of the string (due to string operations and sorting)
- **Space Complexity**: O(n²) for storing all rotations

## Usage for Rosalind

To solve the actual Rosalind problem, you would:
1. Read the input string from file or console
2. Apply the `ConstructBWT` function
3. Output the result

The solution handles the standard Burrows-Wheeler Transform construction as required by the Rosalind problem specification.

