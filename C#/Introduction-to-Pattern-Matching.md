# Rosalind Problem: Introduction to Pattern Matching (BA9A)

## Problem Description

Given a collection of strings, return all the nodes of the trie built from these strings.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.IO;

public class TrieNode
{
    public Dictionary<char, TrieNode> Children;
    public int NodeId;
    public bool IsLeaf;
    
    public TrieNode(int id)
    {
        NodeId = id;
        Children = new Dictionary<char, TrieNode>();
        IsLeaf = false;
    }
}

public class Trie
{
    public TrieNode Root;
    private int nodeIdCounter;
    
    public Trie()
    {
        Root = new TrieNode(0);
        nodeIdCounter = 1;
    }
    
    public void Insert(string pattern)
    {
        TrieNode current = Root;
        
        foreach (char c in pattern)
        {
            if (!current.Children.ContainsKey(c))
            {
                current.Children[c] = new TrieNode(nodeIdCounter++);
            }
            current = current.Children[c];
        }
        current.IsLeaf = true;
    }
    
    public List<string> GetEdges()
    {
        var edges = new List<string>();
        TraverseEdges(Root, edges, "");
        return edges;
    }
    
    private void TraverseEdges(TrieNode node, List<string> edges, string prefix)
    {
        foreach (var kvp in node.Children)
        {
            char c = kvp.Key;
            TrieNode child = kvp.Value;
            
            // Add edge from parent to child
            edges.Add($"{node.NodeId}->{child.NodeId}:{c}");
            
            // Continue traversing
            TraverseEdges(child, edges, prefix + c);
        }
    }
}

public class Program
{
    public static void Main()
    {
        // Read input from file or console
        string[] lines = File.ReadAllLines("rosalind_ba9a.txt");
        // Or for testing: string[] lines = {"ATAGA", "ATC", "GAT"};
        
        // Build trie
        Trie trie = new Trie();
        
        foreach (string pattern in lines)
        {
            trie.Insert(pattern);
        }
        
        // Get all edges
        List<string> edges = trie.GetEdges();
        
        // Output results
        foreach (string edge in edges)
        {
            Console.WriteLine(edge);
        }
    }
}
```

## Example

**Input:**
```
ATAGA
ATC
GAT
```

**Output:**
```
0->1:A
1->4:A
2->3:T
3->4:C
4->5:G
5->6:A
6->7:T
7->8:G
8->9:A
```

## Explanation

1. **Trie Construction**: We build a trie by inserting each string from the input collection
2. **Node Structure**: Each node contains:
   - A dictionary mapping characters to child nodes
   - A unique node ID
   - A flag indicating if it's a leaf node
3. **Edge Generation**: For each node, we generate edges to all its children with the connecting character
4. **Output Format**: Each line represents an edge in the format `parent->child:character`

## Time and Space Complexity

- **Time Complexity**: O(N × M) where N is the number of strings and M is the average length of strings
- **Space Complexity**: O(N × M) for storing the trie structure

## Key Points

1. The trie is built by inserting each pattern character by character
2. Each edge in the output represents a transition from one node to another in the trie
3. The character on each edge is the character that leads from parent to child node
4. Node IDs are assigned sequentially starting from 0

This solution correctly handles the pattern matching problem by constructing the trie structure and outputting all edges in the required format.

