# Rosalind Problem: Square_in_a_Graph

## Problem Description
Given: A positive integer k≤4 and k simple graphs (each with at most 100 vertices) in the edge list format.

Return: For each graph, output "Yes" if it contains a square (4-cycle) as a subgraph, and "No" otherwise.

## Solution Approach
1. Parse the input to extract graph information
2. For each graph, check if it contains a 4-cycle (square)
3. A 4-cycle exists if there are 4 vertices connected in a cycle pattern
4. We can use adjacency matrix representation for efficient checking

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class SquareInAGraph
{
    public static void Main(string[] args)
    {
        // Read input from file or console
        string input = File.ReadAllText("input.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        List<string> graphLines = new List<string>();
        List<List<string>> graphs = new List<List<string>>();
        
        foreach (string line in lines)
        {
            if (string.IsNullOrWhiteSpace(line))
                continue;
                
            if (line.StartsWith(">"))
            {
                if (graphLines.Count > 0)
                    graphs.Add(new List<string>(graphLines));
                graphLines.Clear();
            }
            else
            {
                graphLines.Add(line);
            }
        }
        
        if (graphLines.Count > 0)
            graphs.Add(graphLines);
        
        // Process each graph
        foreach (var graph in graphs)
        {
            bool hasSquare = HasSquare(graph);
            Console.WriteLine(hasSquare ? "Yes" : "No");
        }
    }
    
    private static bool HasSquare(List<string> graphLines)
    {
        // Parse graph
        int n = int.Parse(graphLines[0].Split()[0]); // number of vertices
        int m = int.Parse(graphLines[0].Split()[1]); // number of edges
        
        // Create adjacency matrix
        bool[,] adj = new bool[n, n];
        
        // Fill adjacency matrix
        for (int i = 1; i < graphLines.Count; i++)
        {
            string[] parts = graphLines[i].Split();
            int u = int.Parse(parts[0]) - 1; // Convert to 0-based indexing
            int v = int.Parse(parts[1]) - 1;
            adj[u, v] = true;
            adj[v, u] = true;
        }
        
        // Check for 4-cycle
        // A 4-cycle has vertices a-b-c-d-a
        for (int a = 0; a < n; a++)
        {
            for (int b = a + 1; b < n; b++)
            {
                if (!adj[a, b]) continue;
                
                for (int c = b + 1; c < n; c++)
                {
                    if (!adj[b, c]) continue;
                    
                    for (int d = c + 1; d < n; d++)
                    {
                        if (!adj[c, d]) continue;
                        
                        // Check if a-d exists (completing the cycle)
                        if (adj[a, d])
                        {
                            // Check if a-b-c-d-a forms a valid 4-cycle
                            // We need to ensure no other edges between these vertices
                            // But we're just checking existence of the cycle
                            return true;
                        }
                    }
                }
            }
        }
        
        return false;
    }
    
    // Alternative approach - more efficient
    private static bool HasSquareOptimized(List<string> graphLines)
    {
        int n = int.Parse(graphLines[0].Split()[0]);
        int m = int.Parse(graphLines[0].Split()[1]);
        
        // Create adjacency list
        HashSet<int>[] adj = new HashSet<int>[n];
        for (int i = 0; i < n; i++)
            adj[i] = new HashSet<int>();
        
        // Fill adjacency list
        for (int i = 1; i < graphLines.Count; i++)
        {
            string[] parts = graphLines[i].Split();
            int u = int.Parse(parts[0]) - 1;
            int v = int.Parse(parts[1]) - 1;
            adj[u].Add(v);
            adj[v].Add(u);
        }
        
        // Check for 4-cycle using the approach: find two edges that share a common vertex
        // and check if their other endpoints are connected
        for (int u = 0; u < n; u++)
        {
            foreach (int v in adj[u])
            {
                if (u >= v) continue; // Avoid duplicate checking
                
                foreach (int w in adj[v])
                {
                    if (w <= u) continue; // Avoid duplicate checking
                    
                    foreach (int x in adj[u])
                    {
                        if (x <= w) continue; // Avoid duplicate checking
                        
                        // Check if w-x is an edge
                        if (adj[w].Contains(x))
                        {
                            // Found a 4-cycle: u-v-w-x-u
                            return true;
                        }
                    }
                }
            }
        }
        
        return false;
    }
}
```

## Alternative Cleaner Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("rosalind_sq.txt");
        List<string> inputLines = new List<string>(lines);
        
        int i = 0;
        while (i < inputLines.Count)
        {
            if (string.IsNullOrEmpty(inputLines[i]))
            {
                i++;
                continue;
            }
            
            // Parse graph
            string[] firstLine = inputLines[i].Split();
            int n = int.Parse(firstLine[0]); // vertices
            int m = int.Parse(firstLine[1]); // edges
            
            // Create adjacency matrix
            bool[,] adj = new bool[n, n];
            
            // Read edges
            for (int j = 1; j <= m; j++)
            {
                string[] edge = inputLines[i + j].Split();
                int u = int.Parse(edge[0]) - 1;
                int v = int.Parse(edge[1]) - 1;
                adj[u, v] = true;
                adj[v, u] = true;
            }
            
            // Check for 4-cycle
            bool hasSquare = CheckForSquare(adj, n);
            Console.WriteLine(hasSquare ? "Yes" : "No");
            
            i += m + 1;
        }
    }
    
    static bool CheckForSquare(bool[,] adj, int n)
    {
        // Check all possible 4-cycles
        for (int a = 0; a < n; a++)
        {
            for (int b = a + 1; b < n; b++)
            {
                if (!adj[a, b]) continue;
                
                for (int c = b + 1; c < n; c++)
                {
                    if (!adj[b, c]) continue;
                    
                    for (int d = c + 1; d < n; d++)
                    {
                        if (!adj[c, d]) continue;
                        
                        // Check if a-d exists to complete the cycle
                        if (adj[a, d])
                        {
                            return true;
                        }
                    }
                }
            }
        }
        
        return false;
    }
}
```

## Explanation

The solution works by:

1. **Parsing Input**: Reading the graph data in edge list format
2. **Adjacency Matrix**: Creating a boolean matrix to represent connections between vertices
3. **4-Cycle Detection**: 
   - Iterate through all possible combinations of 4 vertices (a, b, c, d)
   - Check if edges exist: a-b, b-c, c-d, and a-d
   - If all these edges exist, we have found a square (4-cycle)
4. **Output**: Return "Yes" if a square exists, "No" otherwise

The time complexity is O(n⁴) in the worst case, where n is the number of vertices, which is acceptable for graphs with at most 100 vertices.

