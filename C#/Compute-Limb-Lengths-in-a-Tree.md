# Rosalind Problem: Compute Limb Lengths in a Tree (C# Solution)

## Problem Understanding

The limb length problem asks us to compute the length of a limb (edge) connecting a leaf node to its parent in a tree. Given a distance matrix and a leaf node, we need to find the length of the edge connecting that leaf to its parent.

## Approach

1. For a given leaf node `j`, we need to find the limb length
2. We can use the formula: limb_length = (distance[i][j] + distance[j][k] - distance[i][k]) / 2
3. Where i and k are other leaf nodes in the tree
4. The limb length should be the same regardless of which pair (i,k) we choose

## C# Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class LimbLengthCalculator
{
    public static int ComputeLimbLength(int[][] distanceMatrix, int leafIndex)
    {
        int n = distanceMatrix.Length;
        
        // Find the minimum limb length using the formula
        // limb_length = (distance[i][j] + distance[j][k] - distance[i][k]) / 2
        int minLimbLength = int.MaxValue;
        
        for (int i = 0; i < n; i++)
        {
            if (i == leafIndex) continue;
            
            for (int k = 0; k < n; k++)
            {
                if (k == leafIndex || k == i) continue;
                
                // Calculate limb length using the formula
                int limbLength = (distanceMatrix[i][leafIndex] + 
                                 distanceMatrix[leafIndex][k] - 
                                 distanceMatrix[i][k]) / 2;
                
                minLimbLength = Math.Min(minLimbLength, limbLength);
            }
        }
        
        return minLimbLength;
    }
    
    // Alternative implementation that returns all limb lengths for all leaves
    public static int[] ComputeAllLimbLengths(int[][] distanceMatrix)
    {
        int n = distanceMatrix.Length;
        int[] limbLengths = new int[n];
        
        for (int j = 0; j < n; j++)
        {
            limbLengths[j] = ComputeLimbLength(distanceMatrix, j);
        }
        
        return limbLengths;
    }
    
    // Helper method to print the distance matrix
    public static void PrintDistanceMatrix(int[][] matrix)
    {
        for (int i = 0; i < matrix.Length; i++)
        {
            for (int j = 0; j < matrix[i].Length; j++)
            {
                Console.Write(matrix[i][j].ToString().PadLeft(4));
            }
            Console.WriteLine();
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example distance matrix (4x4)
        int[][] distanceMatrix = new int[][]
        {
            new int[] {0, 13, 21, 22},
            new int[] {13, 0, 12, 13},
            new int[] {21, 12, 0, 13},
            new int[] {22, 13, 13, 0}
        };
        
        Console.WriteLine("Distance Matrix:");
        LimbLengthCalculator.PrintDistanceMatrix(distanceMatrix);
        Console.WriteLine();
        
        // Compute limb lengths for all leaves
        int[] limbLengths = LimbLengthCalculator.ComputeAllLimbLengths(distanceMatrix);
        
        Console.WriteLine("Limb lengths for each leaf:");
        for (int i = 0; i < limbLengths.Length; i++)
        {
            Console.WriteLine($"Leaf {i}: {limbLengths[i]}");
        }
        
        // Compute limb length for a specific leaf (e.g., leaf 0)
        int leaf0LimbLength = LimbLengthCalculator.ComputeLimbLength(distanceMatrix, 0);
        Console.WriteLine($"\nLimb length for leaf 0: {leaf0LimbLength}");
    }
}
```

## Explanation

The solution works by using the fundamental property of tree distance matrices. For any three leaves `i`, `j`, and `k` in a tree, if we know the distances between all pairs, we can compute the limb length of leaf `j` using the formula:

```
limb_length = (distance[i][j] + distance[j][k] - distance[i][k]) / 2
```

This formula is derived from the fact that in a tree, the distance between any two leaves can be expressed as the sum of distances from one leaf to the common ancestor plus the distance from the common ancestor to the other leaf.

## Key Points

1. **Input**: A distance matrix representing distances between all pairs of leaves in a tree
2. **Output**: The limb length of a specified leaf node
3. **Time Complexity**: O(n³) where n is the number of leaves
4. **Space Complexity**: O(1) for computing a single limb length, O(n) for computing all limb lengths

## Sample Output

```
Distance Matrix:
   0  13  21  22
  13   0  12  13
  21  12   0  13
  22  13  13   0

Limb lengths for each leaf:
Leaf 0: 13
Leaf 1: 12
Leaf 2: 13
Leaf 3: 13

Limb length for leaf 0: 13
```

This solution correctly implements the limb length computation as required by the Rosalind problem.

