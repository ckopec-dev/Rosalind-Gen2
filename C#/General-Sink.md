# Rosalind Problem General_Sink Solution in C#

## Problem Understanding

The General Sink problem asks us to find a sink in a directed graph - a vertex with no outgoing edges. In other words, we need to find a vertex that has an out-degree of 0.

## Solution Approach

1. Read the directed graph from input
2. Calculate the out-degree of each vertex
3. Find vertices with out-degree 0 (sinks)
4. Return the sink vertices

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class GeneralSink
{
    public static void Main(string[] args)
    {
        // Read input from file
        string input = File.ReadAllText("input.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        // Parse the graph
        Dictionary<int, List<int>> graph = new Dictionary<int, List<int>>();
        
        foreach (string line in lines)
        {
            string[] parts = line.Trim().Split(' ');
            if (parts.Length >= 2)
            {
                int from = int.Parse(parts[0]);
                int to = int.Parse(parts[1]);
                
                if (!graph.ContainsKey(from))
                    graph[from] = new List<int>();
                graph[from].Add(to);
            }
        }
        
        // Find all vertices that are sinks (out-degree = 0)
        HashSet<int> allVertices = new HashSet<int>();
        HashSet<int> verticesWithOutgoingEdges = new HashSet<int>();
        
        // Collect all vertices
        foreach (var kvp in graph)
        {
            allVertices.Add(kvp.Key);
            foreach (int neighbor in kvp.Value)
            {
                allVertices.Add(neighbor);
                verticesWithOutgoingEdges.Add(kvp.Key);
            }
        }
        
        // Find sinks (vertices that don't have outgoing edges)
        List<int> sinks = new List<int>();
        foreach (int vertex in allVertices)
        {
            if (!verticesWithOutgoingEdges.Contains(vertex))
            {
                sinks.Add(vertex);
            }
        }
        
        // Output results
        Console.WriteLine(string.Join(" ", sinks.OrderBy(x => x)));
    }
}
```

## Alternative Cleaner Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class GeneralSinkSolution
{
    public static void Main()
    {
        // Read input
        string[] lines = File.ReadAllLines("input.txt");
        
        // Build adjacency list
        Dictionary<int, List<int>> adjacencyList = new Dictionary<int, List<int>>();
        
        foreach (string line in lines)
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            
            string[] parts = line.Trim().Split(' ');
            int from = int.Parse(parts[0]);
            int to = int.Parse(parts[1]);
            
            if (!adjacencyList.ContainsKey(from))
                adjacencyList[from] = new List<int>();
            adjacencyList[from].Add(to);
        }
        
        // Find all vertices
        HashSet<int> allVertices = new HashSet<int>();
        HashSet<int> verticesWithOutgoing = new HashSet<int>();
        
        foreach (var kvp in adjacencyList)
        {
            allVertices.Add(kvp.Key);
            verticesWithOutgoing.Add(kvp.Key);
            allVertices.UnionWith(kvp.Value);
        }
        
        // Find sinks (vertices with out-degree 0)
        var sinks = allVertices.Except(verticesWithOutgoing).OrderBy(x => x);
        
        Console.WriteLine(string.Join(" ", sinks));
    }
}
```

## Sample Input/Output

**Input:**
```
1 2
2 3
3 4
4 5
```

**Output:**
```
5
```

## Explanation

In this example:
- Vertex 1 has outgoing edge to 2
- Vertex 2 has outgoing edge to 3  
- Vertex 3 has outgoing edge to 4
- Vertex 4 has outgoing edge to 5
- Vertex 5 has no outgoing edges

Therefore, vertex 5 is the sink.

## Time and Space Complexity

- **Time Complexity**: O(E + V) where E is the number of edges and V is the number of vertices
- **Space Complexity**: O(V + E) for storing the adjacency list and vertex sets

The solution efficiently identifies all vertices with out-degree 0, which are the sinks in the directed graph.

