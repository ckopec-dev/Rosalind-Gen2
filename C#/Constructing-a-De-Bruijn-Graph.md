# Rosalind Problem: Constructing a De Bruijn Graph

## Problem Description
In this problem, we need to construct a De Bruijn graph from a collection of k-mers. A De Bruijn graph is a directed graph that represents overlaps between k-mers.

## Solution Approach
1. For each k-mer, extract the prefix (k-1 characters from the left) and suffix (k-1 characters from the right)
2. Create edges from each prefix to its corresponding suffix
3. Output all edges in the required format

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class DeBruijnGraph
{
    public static void Main(string[] args)
    {
        // Read input from file
        string input = File.ReadAllText("rosalind_ba3d.txt");
        string[] lines = input.Trim().Split('\n');
        
        // Parse k-mer length (first line)
        int k = int.Parse(lines[0]);
        
        // Parse k-mers (remaining lines)
        List<string> kmers = new List<string>();
        for (int i = 1; i < lines.Length; i++)
        {
            kmers.Add(lines[i].Trim());
        }
        
        // Construct De Bruijn graph
        var edges = ConstructDeBruijnGraph(kmers, k);
        
        // Output results
        foreach (var edge in edges)
        {
            Console.WriteLine($"{edge.Item1} -> {edge.Item2}");
        }
    }
    
    public static List<(string, string)> ConstructDeBruijnGraph(List<string> kmers, int k)
    {
        var edges = new List<(string, string)>();
        var adjacencyMap = new Dictionary<string, List<string>>();
        
        // For each k-mer, extract prefix and suffix
        foreach (string kmer in kmers)
        {
            string prefix = kmer.Substring(0, k - 1);
            string suffix = kmer.Substring(1, k - 1);
            
            // Add edge from prefix to suffix
            edges.Add((prefix, suffix));
            
            // Build adjacency list for verification
            if (!adjacencyMap.ContainsKey(prefix))
            {
                adjacencyMap[prefix] = new List<string>();
            }
            adjacencyMap[prefix].Add(suffix);
        }
        
        return edges;
    }
}
```

## Alternative Implementation (More Efficient)

```csharp
using System;
using System.Collections.Generic;
using System.IO;

public class DeBruijnGraphOptimized
{
    public static void Main(string[] args)
    {
        // Read input from file
        string input = File.ReadAllText("rosalind_ba3d.txt");
        string[] lines = input.Trim().Split('\n');
        
        // Parse k-mer length (first line)
        int k = int.Parse(lines[0]);
        
        // Parse k-mers (remaining lines)
        List<string> kmers = new List<string>();
        for (int i = 1; i < lines.Length; i++)
        {
            kmers.Add(lines[i].Trim());
        }
        
        // Construct De Bruijn graph
        var edges = ConstructDeBruijnGraph(kmers, k);
        
        // Output results
        foreach (var edge in edges)
        {
            Console.WriteLine($"{edge.Item1} -> {edge.Item2}");
        }
    }
    
    public static List<(string, string)> ConstructDeBruijnGraph(List<string> kmers, int k)
    {
        var edges = new List<(string, string)>();
        
        // For each k-mer, extract prefix and suffix and create edges
        foreach (string kmer in kmers)
        {
            string prefix = kmer.Substring(0, k - 1);
            string suffix = kmer.Substring(1, k - 1);
            
            edges.Add((prefix, suffix));
        }
        
        // Sort edges for consistent output
        edges.Sort((x, y) => 
        {
            int prefixComparison = string.Compare(x.Item1, y.Item1);
            if (prefixComparison != 0) return prefixComparison;
            return string.Compare(x.Item2, y.Item2);
        });
        
        return edges;
    }
}
```

## Sample Input
```
4
AAGA
AAGA
AAGA
AAGA
```

## Sample Output
```
AAG -> AGA
AAG -> AGA
AAG -> AGA
AAG -> AGA
```

## Key Points

1. **Input parsing**: Read k-mer length from first line, then all k-mers from subsequent lines
2. **Prefix extraction**: For a k-mer, the prefix is the first k-1 characters
3. **Suffix extraction**: For a k-mer, the suffix is the last k-1 characters
4. **Edge creation**: Each k-mer contributes an edge from its prefix to its suffix
5. **Output format**: Print edges in "prefix -> suffix" format

## Time Complexity
- **Time**: O(n × k) where n is the number of k-mers and k is the k-mer length
- **Space**: O(n × k) for storing the edges

This solution efficiently constructs the De Bruijn graph by processing each k-mer once and extracting the necessary prefix-suffix pairs to form the edges.

