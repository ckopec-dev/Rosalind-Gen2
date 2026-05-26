# Rosalind Problem: Bellman-Ford Algorithm Solution in C#

## Problem Understanding

The Bellman-Ford algorithm is used to find the shortest paths from a single source vertex to all other vertices in a weighted graph, even when the graph contains negative weight edges. The algorithm can also detect negative weight cycles.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class BellmanFord
{
    public class Edge
    {
        public int Source { get; set; }
        public int Destination { get; set; }
        public long Weight { get; set; }
        
        public Edge(int source, int destination, long weight)
        {
            Source = source;
            Destination = destination;
            Weight = weight;
        }
    }
    
    public static long[] BellmanFordAlgorithm(int vertices, int source, List<Edge> edges)
    {
        // Initialize distances array with maximum value
        long[] distances = new long[vertices];
        for (int i = 0; i < vertices; i++)
        {
            distances[i] = long.MaxValue;
        }
        distances[source] = 0;
        
        // Relax edges repeatedly
        for (int i = 0; i < vertices - 1; i++)
        {
            foreach (Edge edge in edges)
            {
                if (distances[edge.Source] != long.MaxValue && 
                    distances[edge.Source] + edge.Weight < distances[edge.Destination])
                {
                    distances[edge.Destination] = distances[edge.Source] + edge.Weight;
                }
            }
        }
        
        // Check for negative weight cycles
        bool hasNegativeCycle = false;
        foreach (Edge edge in edges)
        {
            if (distances[edge.Source] != long.MaxValue && 
                distances[edge.Source] + edge.Weight < distances[edge.Destination])
            {
                hasNegativeCycle = true;
                break;
            }
        }
        
        // If there's a negative cycle, return -1 for all vertices
        if (hasNegativeCycle)
        {
            for (int i = 0; i < vertices; i++)
            {
                distances[i] = -1;
            }
        }
        
        return distances;
    }
    
    public static void Main(string[] args)
    {
        // Read input from file (assuming input.txt contains the problem data)
        string[] lines = File.ReadAllLines("input.txt");
        
        // Parse the first line to get number of vertices and edges
        string[] firstLine = lines[0].Split(' ');
        int vertices = int.Parse(firstLine[0]);
        int edgesCount = int.Parse(firstLine[1]);
        
        // Parse edges
        List<Edge> edgeList = new List<Edge>();
        for (int i = 1; i <= edgesCount; i++)
        {
            string[] edgeData = lines[i].Split(' ');
            int source = int.Parse(edgeData[0]) - 1; // Convert to 0-based indexing
            int destination = int.Parse(edgeData[1]) - 1;
            long weight = long.Parse(edgeData[2]);
            
            edgeList.Add(new Edge(source, destination, weight));
        }
        
        // Run Bellman-Ford from vertex 0 (1-indexed, so 0 in 0-based)
        long[] distances = BellmanFordAlgorithm(vertices, 0, edgeList);
        
        // Output results
        Console.WriteLine(string.Join(" ", distances.Select(d => d == long.MaxValue ? "x" : d.ToString())));
    }
}
```

## Key Features of the Solution

1. **Edge Class**: Represents a weighted edge with source, destination, and weight properties
2. **BellmanFordAlgorithm Function**: Implements the core Bellman-Ford algorithm:
   - Initializes distances with maximum values
   - Relax edges repeatedly for (vertices-1) iterations
   - Detects negative weight cycles
   - Returns appropriate results based on cycle detection

3. **Input Handling**: 
   - Reads input from file
   - Parses vertices and edges
   - Converts 1-based indexing to 0-based indexing

4. **Output Format**: 
   - Uses "x" for unreachable vertices
   - Uses "-1" for vertices affected by negative cycles
   - Otherwise displays shortest distances

## Time and Space Complexity

- **Time Complexity**: O(VE) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V) for storing distances and edges

## Example Usage

For input format like:
```
6 7
1 2 8
1 3 10
2 4 1
3 4 3
4 5 1
4 6 7
5 6 2
```

The algorithm will compute shortest paths from vertex 1 to all other vertices, handling negative weights appropriately.

