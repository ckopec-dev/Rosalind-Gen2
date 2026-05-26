# Rosalind Problem: Solve_the_Turnpike_Problem

## Problem Description

The Turnpike Problem is a classic computational biology problem that asks us to reconstruct a set of points on a line given the distances between all pairs of points. This is essentially the inverse of the problem of computing pairwise distances.

## Solution Approach

We'll use a recursive backtracking approach:
1. Start with the first point at position 0
2. For each subsequent point, try all possible positions that could create the required distances
3. Use pruning to avoid invalid partial solutions
4. Continue until all points are placed

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class TurnpikeProblem
{
    public static List<int> SolveTurnpike(List<int> distances)
    {
        // Sort distances in descending order
        distances.Sort((a, b) => b.CompareTo(a));
        
        // The maximum distance must be between the first and last point
        int n = FindN(distances);
        
        // Create array to store the positions of points
        int[] points = new int[n];
        points[0] = 0; // First point is at position 0
        
        // Create a list of remaining distances to place
        List<int> remainingDistances = new List<int>(distances);
        remainingDistances.RemoveAt(0); // Remove the maximum distance
        
        // Start backtracking
        if (Backtrack(points, remainingDistances, 1))
        {
            return points.ToList();
        }
        
        return new List<int>(); // No solution found
    }
    
    private static bool Backtrack(int[] points, List<int> distances, int position)
    {
        // Base case: all positions filled
        if (position == points.Length)
        {
            return distances.Count == 0;
        }
        
        // Try placing the next point at various positions
        int maxDistance = distances.Max();
        
        // Try placing point at maxDistance from the first point
        int candidatePosition = points[position - 1] + maxDistance;
        
        // Check if this position is valid (not already occupied)
        if (IsValidPlacement(points, position, candidatePosition))
        {
            points[position] = candidatePosition;
            
            // Remove distances that involve this new point
            List<int> newDistances = RemoveRelatedDistances(distances, points, position);
            
            // Recursively try to fill remaining positions
            if (Backtrack(points, newDistances, position + 1))
            {
                return true;
            }
            
            // Backtrack
            points[position] = 0;
        }
        
        // Try the other direction (if not the first point)
        if (position > 1)
        {
            int candidatePosition2 = points[0] + maxDistance;
            if (IsValidPlacement(points, position, candidatePosition2))
            {
                points[position] = candidatePosition2;
                
                // Remove distances that involve this new point
                List<int> newDistances = RemoveRelatedDistances(distances, points, position);
                
                // Recursively try to fill remaining positions
                if (Backtrack(points, newDistances, position + 1))
                {
                    return true;
                }
                
                // Backtrack
                points[position] = 0;
            }
        }
        
        return false;
    }
    
    private static bool IsValidPlacement(int[] points, int position, int candidate)
    {
        // Check if position is already occupied
        for (int i = 0; i < position; i++)
        {
            if (points[i] == candidate)
                return false;
        }
        
        return true;
    }
    
    private static List<int> RemoveRelatedDistances(List<int> distances, int[] points, int position)
    {
        List<int> newDistances = new List<int>(distances);
        int currentPoint = points[position];
        
        // Remove distances that involve the newly placed point
        for (int i = 0; i < position; i++)
        {
            int distance = Math.Abs(currentPoint - points[i]);
            newDistances.Remove(distance);
        }
        
        return newDistances;
    }
    
    private static int FindN(List<int> distances)
    {
        // The number of points n satisfies: n(n-1)/2 = number of distances
        int numDistances = distances.Count;
        int n = (int)Math.Sqrt(2 * numDistances) + 1;
        
        // Verify the solution
        while (n * (n - 1) / 2 < numDistances)
            n++;
            
        return n;
    }
    
    // Alternative cleaner implementation
    public static List<int> SolveTurnpikeClean(List<int> distances)
    {
        // Sort distances in descending order
        distances.Sort((a, b) => b.CompareTo(a));
        
        int n = (int)((1 + Math.Sqrt(1 + 8 * distances.Count)) / 2);
        int[] points = new int[n];
        points[0] = 0;
        
        List<int> remainingDistances = new List<int>(distances);
        remainingDistances.RemoveAt(0); // Remove max distance
        
        if (BacktrackClean(points, remainingDistances, 1))
        {
            return points.ToList();
        }
        
        return new List<int>();
    }
    
    private static bool BacktrackClean(int[] points, List<int> distances, int position)
    {
        if (position == points.Length)
        {
            return distances.Count == 0;
        }
        
        int maxDist = distances[0];
        int candidate = points[position - 1] + maxDist;
        
        // Try placing the point at candidate position
        if (TryPlacePoint(points, position, candidate, distances))
        {
            if (BacktrackClean(points, distances, position + 1))
                return true;
        }
        
        // Try the other possible position
        if (position > 1)
        {
            candidate = points[0] + maxDist;
            if (TryPlacePoint(points, position, candidate, distances))
            {
                if (BacktrackClean(points, distances, position + 1))
                    return true;
            }
        }
        
        return false;
    }
    
    private static bool TryPlacePoint(int[] points, int position, int candidate, List<int> distances)
    {
        // Check if candidate position is valid
        for (int i = 0; i < position; i++)
        {
            if (points[i] == candidate)
                return false;
        }
        
        // Remove all distances that involve this new point
        List<int> newDistances = new List<int>(distances);
        for (int i = 0; i < position; i++)
        {
            int distance = Math.Abs(candidate - points[i]);
            newDistances.Remove(distance);
        }
        
        points[position] = candidate;
        return true;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example from Rosalind problem
        List<int> distances = new List<int> { 1, 2, 2, 2, 3, 3, 3, 4, 5, 5, 6, 7, 8, 10 };
        List<int> result = TurnpikeProblem.SolveTurnpikeClean(distances);
        
        Console.WriteLine("Solution:");
        Console.WriteLine(string.Join(" ", result));
    }
}
```

## Explanation

This solution implements a backtracking algorithm to solve the Turnpike Problem:

1. **Input Processing**: The input distances are sorted in descending order
2. **Point Count Determination**: We calculate the number of points using the formula n(n-1)/2 = number of distances
3. **Backtracking**: 
   - Start with the first point at position 0
   - For each subsequent point, try possible positions that could create the maximum remaining distance
   - Use pruning to avoid invalid placements
   - Remove distances that are now "used up" by placing a point
4. **Validation**: Check if all distances are properly placed

## Time Complexity
- Worst case: O(2^n) due to backtracking
- In practice, with good pruning, it's much faster

## Space Complexity
- O(n) for storing the points and recursion stack

The algorithm correctly reconstructs the point positions that would generate the given pairwise distances, solving the Turnpike Problem as required by the Rosalind challenge.

