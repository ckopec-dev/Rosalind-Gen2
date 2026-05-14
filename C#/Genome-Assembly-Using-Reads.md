# Rosalind Problem: Genome Assembly Using Reads (C# Solution)

## Problem Understanding

The problem requires us to perform genome assembly using a set of DNA reads. We need to find the shortest possible string that contains all given reads as substrings.

## Approach

We'll use a greedy approach to find the shortest superstring:
1. Find the maximum overlap between any two reads
2. Merge the reads with maximum overlap
3. Repeat until we have one superstring

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class GenomeAssembly
{
    public static string AssembleGenome(List<string> reads)
    {
        if (reads == null || reads.Count == 0)
            return "";
        
        if (reads.Count == 1)
            return reads[0];
        
        // Keep merging reads until we have one superstring
        while (reads.Count > 1)
        {
            int maxOverlap = -1;
            int bestI = -1, bestJ = -1;
            string bestMerge = "";
            
            // Find the pair of reads with maximum overlap
            for (int i = 0; i < reads.Count; i++)
            {
                for (int j = 0; j < reads.Count; j++)
                {
                    if (i == j) continue;
                    
                    int overlap = GetOverlap(reads[i], reads[j]);
                    if (overlap > maxOverlap)
                    {
                        maxOverlap = overlap;
                        bestI = i;
                        bestJ = j;
                        bestMerge = MergeStrings(reads[i], reads[j], overlap);
                    }
                }
            }
            
            // If no overlap found, we can't merge further
            if (maxOverlap <= 0) break;
            
            // Merge the two reads and remove one
            string merged = bestMerge;
            reads.RemoveAt(bestJ);
            reads[bestI] = merged;
        }
        
        return reads[0];
    }
    
    private static int GetOverlap(string s1, string s2)
    {
        // Find maximum overlap where s1's suffix overlaps with s2's prefix
        int maxOverlap = 0;
        
        // Try all possible overlaps from 1 to min(s1.Length, s2.Length)
        for (int i = 1; i <= Math.Min(s1.Length, s2.Length); i++)
        {
            if (s1.Substring(s1.Length - i) == s2.Substring(0, i))
            {
                maxOverlap = i;
            }
        }
        
        return maxOverlap;
    }
    
    private static string MergeStrings(string s1, string s2, int overlap)
    {
        // Merge s1 and s2 with given overlap
        if (overlap == 0)
            return s1 + s2;
            
        return s1 + s2.Substring(overlap);
    }
    
    // Alternative approach using De Bruijn graph (more efficient for large datasets)
    public static string AssembleGenomeDeBruijn(List<string> reads, int k = 3)
    {
        if (reads == null || reads.Count == 0)
            return "";
        
        // Create De Bruijn graph
        var graph = new Dictionary<string, List<string>>();
        var inDegree = new Dictionary<string, int>();
        var outDegree = new Dictionary<string, int>();
        
        // Build the graph
        foreach (string read in reads)
        {
            string prefix = read.Substring(0, k - 1);
            string suffix = read.Substring(1, k - 1);
            
            if (!graph.ContainsKey(prefix))
                graph[prefix] = new List<string>();
                
            graph[prefix].Add(suffix);
            
            // Update degrees
            inDegree[suffix] = inDegree.GetValueOrDefault(suffix, 0) + 1;
            outDegree[prefix] = outDegree.GetValueOrDefault(prefix, 0) + 1;
        }
        
        // Find Eulerian path
        string startNode = FindStartNode(graph, inDegree, outDegree);
        List<string> path = new List<string>();
        EulerianPath(graph, startNode, path);
        
        // Reconstruct the string
        if (path.Count == 0) return "";
        
        string result = path[0];
        for (int i = 1; i < path.Count; i++)
        {
            result += path[i][k - 2];
        }
        
        return result;
    }
    
    private static string FindStartNode(Dictionary<string, List<string>> graph, 
                                       Dictionary<string, int> inDegree, 
                                       Dictionary<string, int> outDegree)
    {
        string start = graph.Keys.First();
        foreach (var kvp in graph)
        {
            string node = kvp.Key;
            int inDeg = inDegree.GetValueOrDefault(node, 0);
            int outDeg = outDegree.GetValueOrDefault(node, 0);
            
            if (outDeg > inDeg)
            {
                start = node;
                break;
            }
        }
        return start;
    }
    
    private static void EulerianPath(Dictionary<string, List<string>> graph, 
                                     string current, List<string> path)
    {
        while (graph.ContainsKey(current) && graph[current].Count > 0)
        {
            string next = graph[current][0];
            graph[current].RemoveAt(0);
            EulerianPath(graph, next, path);
        }
        path.Insert(0, current);
    }
}

// Main program to solve the problem
public class Program
{
    public static void Main()
    {
        // Example input - replace with actual data from Rosalind
        List<string> reads = new List<string>
        {
            "ATTAGACCTG",
            "CCTGCCGGAA",
            "TGCCTAACCG",
            "GCCGGAATAC"
        };
        
        // Solve using greedy approach
        string result = GenomeAssembly.AssembleGenome(reads);
        Console.WriteLine("Assembled genome: " + result);
        
        // Alternative: using De Bruijn approach
        string result2 = GenomeAssembly.AssembleGenomeDeBruijn(reads, 3);
        Console.WriteLine("Assembled genome (De Bruijn): " + result2);
    }
}
```

## Key Features of the Solution

1. **Greedy Approach**: Finds maximum overlap between reads and merges them
2. **Overlap Calculation**: Computes the maximum overlap between two strings
3. **String Merging**: Properly merges two strings with given overlap
4. **De Bruijn Graph Alternative**: More efficient approach for larger datasets
5. **Edge Case Handling**: Handles empty inputs and single read cases

## Time Complexity

- **Greedy Approach**: O(n³) where n is the number of reads
- **De Bruijn Approach**: O(E + V) where E is edges and V is vertices in the graph

## Usage

To solve the actual Rosalind problem:
1. Parse the input reads from the dataset
2. Call `GenomeAssembly.AssembleGenome(reads)` with the list of reads
3. Output the resulting superstring

The solution handles the core problem of finding the shortest superstring that contains all given reads as substrings.

