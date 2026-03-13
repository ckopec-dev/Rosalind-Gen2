# Rosalind Problem: Genome Assembly with Perfect Coverage and Repeats (C# Solution)

## Problem Understanding

This problem asks us to reconstruct a genome from a set of reads that have perfect coverage and may contain repeats. The key insight is that we need to find the shortest superstring that contains all given reads as substrings.

## Approach

1. **De Bruijn Graph Construction**: Build a De Bruijn graph where nodes are (k-1)-mers and edges represent overlaps
2. **Eulerian Path Finding**: Find an Eulerian path in the graph to reconstruct the genome
3. **Handle Repeats**: The perfect coverage ensures that we can reconstruct the original genome

## C# Solution

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
            
        // Find the k-mer size (assuming all reads are same length)
        int k = reads[0].Length;
        
        // Build De Bruijn graph
        var graph = BuildDeBruijnGraph(reads, k);
        
        // Find Eulerian path
        var eulerianPath = FindEulerianPath(graph);
        
        // Reconstruct genome from Eulerian path
        return ReconstructGenome(eulerianPath, k);
    }
    
    private static Dictionary<string, List<string>> BuildDeBruijnGraph(List<string> reads, int k)
    {
        var graph = new Dictionary<string, List<string>>();
        
        foreach (string read in reads)
        {
            // Create k-1 mer prefixes and suffixes
            string prefix = read.Substring(0, k - 1);
            string suffix = read.Substring(1, k - 1);
            
            if (!graph.ContainsKey(prefix))
                graph[prefix] = new List<string>();
                
            graph[prefix].Add(suffix);
        }
        
        return graph;
    }
    
    private static List<string> FindEulerianPath(Dictionary<string, List<string>> graph)
    {
        // Find starting node (node with out-degree - in-degree = 1)
        var inDegrees = new Dictionary<string, int>();
        var outDegrees = new Dictionary<string, int>();
        
        foreach (var kvp in graph)
        {
            string node = kvp.Key;
            var edges = kvp.Value;
            
            if (!outDegrees.ContainsKey(node))
                outDegrees[node] = 0;
            outDegrees[node] += edges.Count;
            
            foreach (string target in edges)
            {
                if (!inDegrees.ContainsKey(target))
                    inDegrees[target] = 0;
                inDegrees[target]++;
            }
        }
        
        string startNode = null;
        foreach (var node in graph.Keys)
        {
            int inDeg = inDegrees.ContainsKey(node) ? inDegrees[node] : 0;
            int outDeg = outDegrees.ContainsKey(node) ? outDegrees[node] : 0;
            
            if (outDeg - inDeg == 1)
            {
                startNode = node;
                break;
            }
        }
        
        if (startNode == null)
        {
            // If no start node found, use any node
            startNode = graph.Keys.First();
        }
        
        // Hierholzer's algorithm for Eulerian path
        var stack = new Stack<string>();
        var path = new List<string>();
        
        stack.Push(startNode);
        
        while (stack.Count > 0)
        {
            string current = stack.Peek();
            
            if (graph.ContainsKey(current) && graph[current].Count > 0)
            {
                string next = graph[current][0];
                graph[current].RemoveAt(0);
                stack.Push(next);
            }
            else
            {
                path.Add(stack.Pop());
            }
        }
        
        path.Reverse();
        return path;
    }
    
    private static string ReconstructGenome(List<string> path, int k)
    {
        if (path.Count == 0)
            return "";
            
        string genome = path[0];
        
        for (int i = 1; i < path.Count; i++)
        {
            // Take the last character of previous node and add it to current node
            genome += path[i][path[i].Length - 1];
        }
        
        return genome;
    }
    
    // Alternative approach using shortest superstring directly
    public static string AssembleGenomeSimple(List<string> reads)
    {
        if (reads == null || reads.Count == 0)
            return "";
            
        if (reads.Count == 1)
            return reads[0];
            
        // Find maximum overlap between each pair of strings
        int n = reads.Count;
        int[,] overlap = new int[n, n];
        
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < n; j++)
            {
                if (i != j)
                {
                    overlap[i, j] = FindOverlap(reads[i], reads[j]);
                }
            }
        }
        
        // Use greedy approach to find shortest superstring
        var remaining = new List<int>();
        for (int i = 0; i < n; i++)
            remaining.Add(i);
            
        while (remaining.Count > 1)
        {
            int maxOverlap = -1;
            int bestI = -1, bestJ = -1;
            
            for (int i = 0; i < remaining.Count; i++)
            {
                for (int j = 0; j < remaining.Count; j++)
                {
                    if (i != j && overlap[remaining[i], remaining[j]] > maxOverlap)
                    {
                        maxOverlap = overlap[remaining[i], remaining[j]];
                        bestI = i;
                        bestJ = j;
                    }
                }
            }
            
            if (maxOverlap >= 0)
            {
                int i = remaining[bestI];
                int j = remaining[bestJ];
                
                // Merge reads[i] and reads[j]
                string merged = MergeStrings(reads[i], reads[j], maxOverlap);
                
                // Remove the two strings and add the merged one
                remaining.RemoveAt(bestJ);
                remaining.RemoveAt(bestI);
                remaining.Add(remaining.Count);
                reads.Add(merged);
            }
            else
            {
                // No more overlaps, just take first one
                break;
            }
        }
        
        return reads[remaining[0]];
    }
    
    private static int FindOverlap(string s1, string s2)
    {
        int maxOverlap = 0;
        int minLen = Math.Min(s1.Length, s2.Length);
        
        // Try all possible overlaps
        for (int i = 1; i <= minLen; i++)
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
        return s1 + s2.Substring(overlap);
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example test case
        var reads = new List<string>
        {
            "ATTAGACCTG",
            "CCTGCCGGAA",
            "TACCTGCCGG",
            "GCCGGAAATT",
            "ACCTGCCGGA"
        };
        
        string result = GenomeAssembly.AssembleGenome(reads);
        Console.WriteLine("Assembled genome: " + result);
        
        // Simple approach
        string result2 = GenomeAssembly.AssembleGenomeSimple(reads);
        Console.WriteLine("Assembled genome (simple): " + result2);
    }
}
```

## Explanation

This solution provides two approaches:

1. **De Bruijn Graph Approach**: 
   - Builds a De Bruijn graph where nodes are (k-1)-mers
   - Finds an Eulerian path to reconstruct the genome
   - This is the standard approach for genome assembly

2. **Shortest Superstring Approach**:
   - Uses a greedy algorithm to merge overlapping strings
   - Finds maximum overlaps between strings
   - Merges strings to create the shortest possible superstring

## Key Points

- **Perfect Coverage**: The problem assumes perfect coverage, meaning every position in the original genome is covered by at least one read
- **Repeats**: The solution handles repeats by finding the correct Eulerian path
- **Time Complexity**: O(n*k) for the De Bruijn approach where n is the number of reads and k is the read length
- **Space Complexity**: O(n*k) for storing the graph

## Usage

The solution can be adapted for specific Rosalind input formats by reading the input file and parsing the reads appropriately. The main function `AssembleGenome` takes a list of reads and returns the assembled genome string.

