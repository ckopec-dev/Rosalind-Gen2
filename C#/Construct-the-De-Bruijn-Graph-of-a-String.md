# Rosalind Problem: Construct the De Bruijn Graph of a String

## Problem Statement
Given a string Text of length at least k, construct the de Bruijn graph of Text.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class DeBruijnGraph
{
    public static List<string> ConstructDeBruijnGraph(string text, int k)
    {
        // Create a dictionary to store the adjacency list
        Dictionary<string, List<string>> adjacencyList = new Dictionary<string, List<string>>();
        
        // Get all k-mers from the text
        List<string> kmers = new List<string>();
        for (int i = 0; i <= text.Length - k; i++)
        {
            kmers.Add(text.Substring(i, k));
        }
        
        // For each k-mer, create the prefix and suffix
        foreach (string kmer in kmers)
        {
            string prefix = kmer.Substring(0, k - 1);
            string suffix = kmer.Substring(1, k - 1);
            
            // Add the edge from prefix to suffix
            if (!adjacencyList.ContainsKey(prefix))
            {
                adjacencyList[prefix] = new List<string>();
            }
            adjacencyList[prefix].Add(suffix);
        }
        
        // Convert to required format: each line is "Prefix -> Suffix1, Suffix2, ..."
        List<string> result = new List<string>();
        foreach (var kvp in adjacencyList.OrderBy(x => x.Key))
        {
            string prefix = kvp.Key;
            string suffixes = string.Join(", ", kvp.Value.OrderBy(x => x));
            result.Add($"{prefix} -> {suffixes}");
        }
        
        return result;
    }
    
    public static void Main(string[] args)
    {
        // Example usage
        string text = "AAGATTCTCTAC";
        int k = 4;
        
        List<string> graph = ConstructDeBruijnGraph(text, k);
        
        foreach (string edge in graph)
        {
            Console.WriteLine(edge);
        }
    }
}
```

## Explanation

The solution works as follows:

1. **Input Processing**: Given a string `text` and integer `k`, we first extract all k-mers from the text.

2. **Graph Construction**: For each k-mer, we:
   - Extract the prefix (first k-1 characters)
   - Extract the suffix (last k-1 characters)
   - Create a directed edge from prefix to suffix

3. **Adjacency List**: We use a dictionary where keys are prefixes and values are lists of suffixes that can be reached from that prefix.

4. **Output Format**: The result is formatted as "Prefix -> Suffix1, Suffix2, ..." with edges sorted lexicographically by prefix.

## Example

For input `text = "AAGATTCTCTAC"` and `k = 4`:
- K-mers: AAGA, AGAT, GATT, ATTC, TTCT, TCTC, CTCT, TCTA, CTAC
- Edges: 
  - AAG → AGA
  - AGA → GAT
  - GAT → ATT
  - ATT → TTC
  - TTC → TCT
  - TCT → CTCT
  - CTCT → TCTA
  - TCTA → CTAC

## Time Complexity
- O(n × k) where n is the length of the text and k is the k-mer length

## Space Complexity
- O(n × k) for storing the k-mers and adjacency list

The solution correctly handles the de Bruijn graph construction by creating the appropriate directed edges between overlapping k-mers.

