# Rosalind Problem: Semi-Connected Graph

## Problem Description
A directed graph is semi-connected if for any two vertices u and v, there is either a path from u to v or a path from v to u. Given a directed graph, determine whether it is semi-connected.

## Solution Approach
1. Find strongly connected components using Kosaroviu's algorithm
2. Create condensation graph (contract SCCs into single nodes)
3. Check if condensation graph is a directed path (linear order)

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class SemiConnectedGraph
{
    public static void Main(string[] args)
    {
        // Read input from file
        string input = File.ReadAllText("rosalind_scc.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        // Parse graph
        int n = int.Parse(lines[0].Split()[0]);
        int m = int.Parse(lines[0].Split()[1]);
        
        List<List<int>> graph = new List<List<int>>(n + 1);
        List<List<int>> reverseGraph = new List<List<int>>(n + 1);
        
        for (int i = 0; i <= n; i++)
        {
            graph.Add(new List<int>());
            reverseGraph.Add(new List<int>());
        }
        
        for (int i = 1; i <= m; i++)
        {
            string[] edge = lines[i].Split();
            int u = int.Parse(edge[0]);
            int v = int.Parse(edge[1]);
            
            graph[u].Add(v);
            reverseGraph[v].Add(u);
        }
        
        // Find strongly connected components
        var sccs = FindStronglyConnectedComponents(n, graph, reverseGraph);
        
        // Create condensation graph
        var condensation = CreateCondensationGraph(n, sccs, graph);
        
        // Check if condensation is a directed path
        bool isSemiConnected = IsDirectedPath(condensation);
        
        Console.WriteLine(isSemiConnected ? "1" : "0");
    }
    
    // Find strongly connected components using Kosaraju's algorithm
    public static List<List<int>> FindStronglyConnectedComponents(int n, List<List<int>> graph, List<List<int>> reverseGraph)
    {
        List<int> finishOrder = new List<int>();
        bool[] visited = new bool[n + 1];
        
        // First DFS on reverse graph to get finish order
        for (int i = 1; i <= n; i++)
        {
            if (!visited[i])
            {
                DfsFinishOrder(reverseGraph, i, visited, finishOrder);
            }
        }
        
        // Second DFS on original graph in reverse finish order
        visited = new bool[n + 1];
        List<List<int>> sccs = new List<List<int>>();
        
        for (int i = finishOrder.Count - 1; i >= 0; i--)
        {
            int node = finishOrder[i];
            if (!visited[node])
            {
                List<int> scc = new List<int>();
                DfsScc(graph, node, visited, scc);
                sccs.Add(scc);
            }
        }
        
        return sccs;
    }
    
    // DFS to get finish order
    private static void DfsFinishOrder(List<List<int>> graph, int node, bool[] visited, List<int> finishOrder)
    {
        visited[node] = true;
        
        foreach (int neighbor in graph[node])
        {
            if (!visited[neighbor])
            {
                DfsFinishOrder(graph, neighbor, visited, finishOrder);
            }
        }
        
        finishOrder.Add(node);
    }
    
    // DFS to find SCC
    private static void DfsScc(List<List<int>> graph, int node, bool[] visited, List<int> scc)
    {
        visited[node] = true;
        scc.Add(node);
        
        foreach (int neighbor in graph[node])
        {
            if (!visited[neighbor])
            {
                DfsScc(graph, neighbor, visited, scc);
            }
        }
    }
    
    // Create condensation graph
    public static Dictionary<int, List<int>> CreateCondensationGraph(int n, List<List<int>> sccs, List<List<int>> originalGraph)
    {
        // Map each node to its SCC index
        Dictionary<int, int> nodeToScc = new Dictionary<int, int>();
        for (int i = 0; i < sccs.Count; i++)
        {
            foreach (int node in sccs[i])
            {
                nodeToScc[node] = i;
            }
        }
        
        // Create condensation graph
        Dictionary<int, List<int>> condensation = new Dictionary<int, List<int>>();
        
        // Initialize all SCCs as nodes in condensation graph
        for (int i = 0; i < sccs.Count; i++)
        {
            condensation[i] = new List<int>();
        }
        
        // Add edges between SCCs
        for (int u = 1; u <= n; u++)
        {
            foreach (int v in originalGraph[u])
            {
                int sccU = nodeToScc[u];
                int sccV = nodeToScc[v];
                
                if (sccU != sccV)
                {
                    if (!condensation[sccU].Contains(sccV))
                    {
                        condensation[sccU].Add(sccV);
                    }
                }
            }
        }
        
        return condensation;
    }
    
    // Check if condensation graph is a directed path
    public static bool IsDirectedPath(Dictionary<int, List<int>> condensation)
    {
        if (condensation.Count <= 1) return true;
        
        // Find nodes with in-degree 0
        HashSet<int> inDegreeZero = new HashSet<int>(condensation.Keys);
        foreach (var kvp in condensation)
        {
            foreach (int neighbor in kvp.Value)
            {
                inDegreeZero.Remove(neighbor);
            }
        }
        
        // There should be exactly one node with in-degree 0
        if (inDegreeZero.Count != 1) return false;
        
        int current = inDegreeZero.First();
        int nodesProcessed = 1;
        
        // Traverse the path
        while (condensation.ContainsKey(current) && condensation[current].Count > 0)
        {
            if (condensation[current].Count != 1) return false; // Multiple outgoing edges
            
            current = condensation[current][0];
            nodesProcessed++;
        }
        
        // All nodes should be processed
        return nodesProcessed == condensation.Count;
    }
}
```

## Explanation

This solution follows these steps:

1. **Kosaraju's Algorithm**: Find all strongly connected components (SCCs) in the directed graph
2. **Condensation Graph**: Create a new graph where each SCC is a single node, and edges represent connections between SCCs
3. **Path Check**: Verify that the condensation graph forms a directed path (linear ordering)

## Key Components

- **FindStronglyConnectedComponents**: Implements Kosaraju's algorithm using two DFS traversals
- **CreateCondensationGraph**: Contracts SCCs into single nodes and builds the reduced graph
- **IsDirectedPath**: Checks if the condensation graph forms a single directed path

## Time Complexity
- O(V + E) for Kosaraju's algorithm
- O(V + E) for condensation graph creation
- O(V + E) for path checking
- Overall: O(V + E)

## Space Complexity
- O(V + E) for storing the graph and auxiliary data structures

The algorithm correctly identifies whether a directed graph is semi-connected by checking if its condensation graph forms a directed path, which is the necessary and sufficient condition for semi-connectivity.

