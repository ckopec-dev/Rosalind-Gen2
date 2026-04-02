# Rosalind Problem: Find an Eulerian Path in a Graph

## Problem Statement
Find an Eulerian path in a directed graph.

An Eulerian path is a path that visits every edge exactly once. A directed graph has an Eulerian path if and only if:
1. All vertices have equal in-degree and out-degree, OR
2. All vertices except two have equal in-degree and out-degree, where one vertex has out-degree = in-degree + 1 (start vertex) and one vertex has in-degree = out-degree + 1 (end vertex)

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class EulerianPath
{
    public static void Main()
    {
        // Read input from stdin
        var lines = new List<string>();
        string line;
        while ((line = Console.ReadLine()) != null)
        {
            lines.Add(line);
        }
        
        // Parse the graph
        var graph = ParseGraph(lines);
        
        // Find Eulerian path
        var path = FindEulerianPath(graph);
        
        // Output the path
        Console.WriteLine(string.Join(" -> ", path));
    }
    
    private static Dictionary<int, List<int>> ParseGraph(List<string> lines)
    {
        var graph = new Dictionary<int, List<int>>();
        
        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            
            var parts = line.Split(" -> ");
            int from = int.Parse(parts[0]);
            
            if (!graph.ContainsKey(from))
                graph[from] = new List<int>();
                
            if (parts.Length > 1 && !string.IsNullOrWhiteSpace(parts[1]))
            {
                var toNodes = parts[1].Split(',').Select(x => int.Parse(x.Trim()));
                graph[from].AddRange(toNodes);
            }
        }
        
        return graph;
    }
    
    private static List<int> FindEulerianPath(Dictionary<int, List<int>> graph)
    {
        // Calculate in-degrees and out-degrees
        var inDegree = new Dictionary<int, int>();
        var outDegree = new Dictionary<int, int>();
        
        foreach (var kvp in graph)
        {
            int from = kvp.Key;
            outDegree[from] = kvp.Value.Count;
            
            if (!inDegree.ContainsKey(from))
                inDegree[from] = 0;
                
            foreach (int to in kvp.Value)
            {
                if (!inDegree.ContainsKey(to))
                    inDegree[to] = 0;
                inDegree[to]++;
                
                if (!outDegree.ContainsKey(to))
                    outDegree[to] = 0;
            }
        }
        
        // Find start and end vertices
        int start = -1;
        int end = -1;
        int unbalancedCount = 0;
        
        foreach (var vertex in inDegree.Keys.Union(outDegree.Keys))
        {
            int inD = inDegree.ContainsKey(vertex) ? inDegree[vertex] : 0;
            int outD = outDegree.ContainsKey(vertex) ? outDegree[vertex] : 0;
            
            if (inD == outD) continue;
            
            unbalancedCount++;
            if (outD == inD + 1)
                start = vertex;
            else if (inD == outD + 1)
                end = vertex;
        }
        
        // If no unbalanced vertices, start from any vertex with outgoing edges
        if (start == -1)
        {
            start = graph.Keys.FirstOrDefault(k => graph[k].Count > 0);
        }
        
        // Create a copy of the graph for traversal
        var graphCopy = new Dictionary<int, List<int>>();
        foreach (var kvp in graph)
        {
            graphCopy[kvp.Key] = new List<int>(kvp.Value);
        }
        
        // Find Eulerian path using Hierholzer's algorithm
        var path = new List<int>();
        var stack = new Stack<int>();
        stack.Push(start);
        
        while (stack.Count > 0)
        {
            int current = stack.Peek();
            
            if (graphCopy.ContainsKey(current) && graphCopy[current].Count > 0)
            {
                int next = graphCopy[current][0];
                graphCopy[current].RemoveAt(0);
                stack.Push(next);
            }
            else
            {
                path.Add(stack.Pop());
            }
        }
        
        // Reverse the path since we built it backwards
        path.Reverse();
        return path;
    }
}
```

## Approach Explanation

1. **Parse Input**: Read the graph from standard input, where each line represents a node and its outgoing edges.

2. **Calculate Degrees**: Determine in-degrees and out-degrees for all vertices.

3. **Find Start/End**: Identify the start and end vertices based on degree differences:
   - If all vertices have equal in and out degrees, any vertex can be start
   - If there are two unbalanced vertices, the one with out-degree = in-degree + 1 is start, and the one with in-degree = out-degree + 1 is end

4. **Hierholzer's Algorithm**: 
   - Use a stack to traverse the graph
   - When we can't proceed further, add the vertex to the path
   - Reverse the final path to get the correct order

## Time Complexity
- O(E + V) where E is the number of edges and V is the number of vertices

## Space Complexity  
- O(E + V) for storing the graph and auxiliary data structures

## Example Input
```
0 -> 3
1 -> 0
2 -> 1,6
3 -> 2
4 -> 2
5 -> 4
6 -> 5,8
7 -> 9
8 -> 7
9 -> 6
```

## Example Output
```
6 -> 5 -> 4 -> 2 -> 1 -> 0 -> 3 -> 2 -> 6 -> 8 -> 7 -> 9 -> 6
```

