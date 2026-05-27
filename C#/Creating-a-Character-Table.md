# Rosalind Problem: Creating a Character Table (Creating_a_Character_Table)

## Problem Description
Given: A collection of at most 100 character strings of equal length (at most 20), each representing a taxon. The strings are in FASTA format.

Return: An adjacency list-formatted collection of edges corresponding to the nontrivial character splits of the strings.

## Solution Approach
1. Parse the FASTA formatted input to extract taxon names and their sequences
2. For each position in the sequences, determine if it creates a valid character split
3. A character split occurs when we can partition the taxa into two non-empty groups based on the character at that position
4. Generate the adjacency list representation of these splits

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class CharacterTable
{
    public static void Main(string[] args)
    {
        // Read input from file or console
        string input = File.ReadAllText("rosalind_cstr.txt");
        var result = Solve(input);
        
        foreach (var edge in result)
        {
            Console.WriteLine(edge);
        }
    }
    
    public static List<string> Solve(string input)
    {
        // Parse FASTA format
        var taxa = ParseFasta(input);
        var taxaNames = taxa.Keys.ToList();
        var sequences = taxa.Values.ToList();
        
        if (sequences.Count == 0) return new List<string>();
        
        int sequenceLength = sequences[0].Length;
        var edges = new List<string>();
        
        // For each position in the sequences
        for (int pos = 0; pos < sequenceLength; pos++)
        {
            // Create a mapping of character to taxa indices
            var charToTaxa = new Dictionary<char, List<int>>();
            
            for (int i = 0; i < sequences.Count; i++)
            {
                char c = sequences[i][pos];
                if (!charToTaxa.ContainsKey(c))
                    charToTaxa[c] = new List<int>();
                charToTaxa[c].Add(i);
            }
            
            // Check if this position creates a valid character split
            // A valid split must have at least 2 groups, and each group must have at least 1 element
            if (charToTaxa.Count >= 2)
            {
                foreach (var kvp in charToTaxa)
                {
                    if (kvp.Value.Count == 0) continue;
                    
                    // Check if this group and the rest form a valid split
                    var group1 = kvp.Value;
                    var group2 = new List<int>();
                    
                    foreach (var otherKvp in charToTaxa)
                    {
                        if (otherKvp.Key != kvp.Key)
                        {
                            group2.AddRange(otherKvp.Value);
                        }
                    }
                    
                    // Valid split if both groups are non-empty
                    if (group1.Count > 0 && group2.Count > 0)
                    {
                        // Convert to 1-based indexing for output
                        var taxa1 = group1.Select(i => (i + 1).ToString()).ToList();
                        var taxa2 = group2.Select(i => (i + 1).ToString()).ToList();
                        
                        // Create adjacency list representation
                        edges.Add(string.Join(" ", taxa1) + " " + string.Join(" ", taxa2));
                    }
                }
            }
        }
        
        return edges;
    }
    
    private static Dictionary<string, string> ParseFasta(string input)
    {
        var taxa = new Dictionary<string, string>();
        var lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        string currentTaxon = "";
        string currentSequence = "";
        
        foreach (string line in lines)
        {
            if (line.StartsWith(">"))
            {
                if (!string.IsNullOrEmpty(currentTaxon))
                {
                    taxa[currentTaxon] = currentSequence;
                }
                currentTaxon = line.Substring(1).Trim();
                currentSequence = "";
            }
            else
            {
                currentSequence += line.Trim();
            }
        }
        
        if (!string.IsNullOrEmpty(currentTaxon))
        {
            taxa[currentTaxon] = currentSequence;
        }
        
        return taxa;
    }
}
```

## Alternative Simpler Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class CharacterTableSolver
{
    public static void Main()
    {
        string input = File.ReadAllText("rosalind_cstr.txt");
        var result = CreateCharacterTable(input);
        
        foreach (var edge in result)
        {
            Console.WriteLine(edge);
        }
    }
    
    public static List<string> CreateCharacterTable(string input)
    {
        // Parse FASTA format
        var sequences = ParseFasta(input);
        var taxaCount = sequences.Count;
        var sequenceLength = sequences[0].Length;
        var edges = new List<string>();
        
        // For each position in the sequence
        for (int pos = 0; pos < sequenceLength; pos++)
        {
            // Group taxa by the character at this position
            var groups = new Dictionary<char, List<int>>();
            
            for (int i = 0; i < taxaCount; i++)
            {
                char c = sequences[i][pos];
                if (!groups.ContainsKey(c))
                    groups[c] = new List<int>();
                groups[c].Add(i);
            }
            
            // If we have at least 2 different characters, we can form a split
            if (groups.Count >= 2)
            {
                // For each character, create a split with all other taxa
                foreach (var group in groups)
                {
                    var thisGroup = group.Value;
                    var otherGroup = new List<int>();
                    
                    foreach (var other in groups)
                    {
                        if (other.Key != group.Key)
                        {
                            otherGroup.AddRange(other.Value);
                        }
                    }
                    
                    // Only include if both groups are non-empty
                    if (thisGroup.Count > 0 && otherGroup.Count > 0)
                    {
                        // Convert to 1-based indices and create edge
                        var thisGroup1Based = thisGroup.Select(x => (x + 1).ToString()).ToList();
                        var otherGroup1Based = otherGroup.Select(x => (x + 1).ToString()).ToList();
                        
                        edges.Add(string.Join(" ", thisGroup1Based) + " " + string.Join(" ", otherGroup1Based));
                    }
                }
            }
        }
        
        return edges;
    }
    
    private static List<string> ParseFasta(string input)
    {
        var sequences = new List<string>();
        var lines = input.Split('\n');
        var currentSequence = new List<string>();
        
        foreach (string line in lines)
        {
            if (line.StartsWith(">"))
            {
                if (currentSequence.Count > 0)
                {
                    sequences.Add(string.Join("", currentSequence));
                }
                currentSequence.Clear();
            }
            else
            {
                currentSequence.Add(line.Trim());
            }
        }
        
        if (currentSequence.Count > 0)
        {
            sequences.Add(string.Join("", currentSequence));
        }
        
        return sequences;
    }
}
```

## Key Points

1. **Input Parsing**: The solution handles FASTA format input where taxa are identified by `>` prefixes
2. **Character Splits**: For each position in the sequences, we check if the characters at that position can form valid splits
3. **Adjacency List**: Each valid split is represented as two groups of taxa indices separated by a space
4. **Indexing**: Results use 1-based indexing as required by the problem
5. **Validation**: Only splits where both groups are non-empty are included

## Time Complexity
- O(n × m × k) where n is the number of taxa, m is the sequence length, and k is the number of distinct characters at each position

## Space Complexity
- O(n × m) for storing the sequences and intermediate results

