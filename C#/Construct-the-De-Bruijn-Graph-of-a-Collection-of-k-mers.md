# Rosalind Problem: Construct the De Bruijn Graph of a Collection of k-mers

## Problem Statement
Given a collection of k-mers, construct the de Bruijn graph. The de Bruijn graph is a directed graph where each k-mer is represented as an edge, and vertices represent (k-1)-mers.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class DeBruijnGraph
{
    public static Dictionary<string, List<string>> ConstructDeBruijnGraph(List<string> kmers)
    {
        var graph = new Dictionary<string, List<string>>();
        
        // For each k-mer, extract prefix and suffix
        foreach (string kmer in kmers)
        {
            string prefix = kmer.Substring(0, kmer.Length - 1);
            string suffix = kmer.Substring(1);
            
            // Add edge from prefix to suffix
            if (!graph.ContainsKey(prefix))
            {
                graph[prefix] = new List<string>();
            }
            graph[prefix].Add(suffix);
        }
        
        return graph;
    }
    
    public static void PrintDeBruijnGraph(Dictionary<string, List<string>> graph)
    {
        // Sort the graph by keys for consistent output
        foreach (var kvp in graph.OrderBy(x => x.Key))
        {
            string prefix = kvp.Key;
            var suffixes = kvp.Value.OrderBy(x => x).ToList();
            
            Console.Write($"{prefix} -> ");
            Console.WriteLine(string.Join(",", suffixes));
        }
    }
    
    public static void Main(string[] args)
    {
        // Example input
        List<string> kmers = new List<string>
        {
            "GAGG",
            "CAGG",
            "GGGG",
            "GGGA",
            "CAGG",
            "AGGG",
            "GGAG"
        };
        
        var deBruijnGraph = ConstructDeBruijnGraph(kmers);
        PrintDeBruijnGraph(deBruijnGraph);
    }
}
```

## Explanation

### Algorithm Steps:
1. **Input Processing**: Take a list of k-mers as input
2. **Graph Construction**: For each k-mer:
   - Extract the prefix (first k-1 characters)
   - Extract the suffix (last k-1 characters)
   - Create an edge from prefix to suffix in the graph
3. **Output Format**: Print the adjacency list representation

### Key Concepts:
- **Prefix**: First (k-1) characters of a k-mer
- **Suffix**: Last (k-1) characters of a k-mer
- **Edge**: From prefix to suffix represents the k-mer
- **Graph**: Each vertex is a (k-1)-mer, each edge is a k-mer

### Time Complexity:
- **Time**: O(n × k) where n is the number of k-mers and k is the k-mer length
- **Space**: O(n × k) for storing the graph

### Example:
For input k-mers: `["GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG"]`

The output would be:
```
AGG -> GG
CAG -> AGG
GAG -> AGG
GGA -> GAG
GGG -> GGA,GGG
```

This represents the de Bruijn graph where each edge corresponds to a k-mer, and vertices represent the (k-1)-mers that form the prefix and suffix of each k-mer.

