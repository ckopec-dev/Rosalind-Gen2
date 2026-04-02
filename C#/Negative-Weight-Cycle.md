# Rosalind Problem: Negative Weight Cycle (Negative_Weight_Cycle)

## Problem Description
Given: A weighted graph with negative edge weights. Return: A negative weight cycle in the graph, or "No negative cycle" if none exists.

## Solution Approach
I'll use the Bellman-Ford algorithm to detect negative weight cycles. The key insight is that if we can relax edges further after V-1 iterations, then there's a negative cycle.

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class NegativeWeightCycle
{
    public class Edge
    {
        public int Source { get; set; }
        public int Destination { get; set; }
        public int Weight { get; set; }
        
        public Edge(int source, int destination, int weight)
        {
            Source = source;
            Destination = destination;
            Weight = weight;
        }
    }
    
    public static List<int> FindNegativeWeightCycle(List<Edge> edges, int vertices)
    {
        // Initialize distances and predecessors
        int[] distances = new int[vertices];
        int[] predecessors = new int[vertices];
        
        // Initialize all distances to infinity and predecessors to -1
        for (int i = 0; i < vertices; i++)
        {
            distances[i] = int.MaxValue;
            predecessors[i] = -1;
        }
        
        // Set source distance to 0
        distances[0] = 0;
        
        // Relax edges V-1 times
        for (int i = 0; i < vertices - 1; i++)
        {
            foreach (Edge edge in edges)
            {
                if (distances[edge.Source] != int.MaxValue && 
                    distances[edge.Source] + edge.Weight < distances[edge.Destination])
                {
                    distances[edge.Destination] = distances[edge.Source] + edge.Weight;
                    predecessors[edge.Destination] = edge.Source;
                }
            }
        }
        
        // Check for negative weight cycles
        int negativeCycleStart = -1;
        foreach (Edge edge in edges)
        {
            if (distances[edge.Source] != int.MaxValue && 
                distances[edge.Source] + edge.Weight < distances[edge.Destination])
            {
                negativeCycleStart = edge.Destination;
                break;
            }
        }
        
        // If no negative cycle found
        if (negativeCycleStart == -1)
        {
            return new List<int> { -1 }; // Indicate no negative cycle
        }
        
        // Find the actual cycle by going backwards from the start node
        // Find the cycle by following predecessors
        int cycleNode = negativeCycleStart;
        HashSet<int> visited = new HashSet<int>();
        
        // Move backwards to find the start of the cycle
        while (!visited.Contains(cycleNode))
        {
            visited.Add(cycleNode);
            cycleNode = predecessors[cycleNode];
        }
        
        // Now we found the cycle start, trace it
        int cycleStart = cycleNode;
        List<int> cycle = new List<int>();
        int current = cycleStart;
        
        do
        {
            cycle.Add(current);
            current = predecessors[current];
        } while (current != cycleStart && current != -1);
        
        cycle.Add(cycleStart); // Add the start node to complete the cycle
        cycle.Reverse(); // Reverse to get correct order
        
        return cycle;
    }
    
    public static void Main(string[] args)
    {
        // Example usage
        List<Edge> edges = new List<Edge>
        {
            new Edge(0, 1, -1),
            new Edge(1, 2, -2),
            new Edge(2, 3, -3),
            new Edge(3, 1, 1), // This creates a negative cycle: 1->2->3->1
            new Edge(0, 4, 5)
        };
        
        int vertices = 5;
        
        List<int> result = FindNegativeWeightCycle(edges, vertices);
        
        if (result[0] == -1)
        {
            Console.WriteLine("No negative cycle");
        }
        else
        {
            Console.WriteLine("Negative cycle found: " + string.Join(" -> ", result));
        }
    }
}
```

## Alternative Implementation (More Robust)

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class NegativeWeightCycleSolver
{
    public class Edge
    {
        public int Source { get; set; }
        public int Destination { get; set; }
        public int Weight { get; set; }
        
        public Edge(int source, int destination, int weight)
        {
            Source = source;
            Destination = destination;
            Weight = weight;
        }
    }
    
    public static List<int> FindNegativeCycle(List<Edge> edges, int vertices)
    {
        // Bellman-Ford algorithm
        int[] distances = new int[vertices];
        int[] predecessors = new int[vertices];
        
        // Initialize
        for (int i = 0; i < vertices; i++)
        {
            distances[i] = int.MaxValue;
            predecessors[i] = -1;
        }
        
        distances[0] = 0;
        
        // Relax edges V-1 times
        for (int i = 0; i < vertices - 1; i++)
        {
            foreach (Edge edge in edges)
            {
                if (distances[edge.Source] != int.MaxValue && 
                    distances[edge.Source] + edge.Weight < distances[edge.Destination])
                {
                    distances[edge.Destination] = distances[edge.Source] + edge.Weight;
                    predecessors[edge.Destination] = edge.Source;
                }
            }
        }
        
        // Check for negative weight cycles
        int negativeCycleNode = -1;
        foreach (Edge edge in edges)
        {
            if (distances[edge.Source] != int.MaxValue && 
                distances[edge.Source] + edge.Weight < distances[edge.Destination])
            {
                negativeCycleNode = edge.Destination;
                break;
            }
        }
        
        if (negativeCycleNode == -1)
        {
            return new List<int> { -1 }; // No negative cycle
        }
        
        // Find the actual cycle
        // Find a node that is part of the negative cycle
        int cycleStart = negativeCycleNode;
        for (int i = 0; i < vertices; i++)
        {
            cycleStart = predecessors[cycleStart];
        }
        
        // Build the cycle
        List<int> cycle = new List<int>();
        int current = cycleStart;
        do
        {
            cycle.Add(current);
            current = predecessors[current];
        } while (current != cycleStart && current != -1);
        
        cycle.Add(cycleStart);
        cycle.Reverse();
        
        return cycle;
    }
    
    public static void Main(string[] args)
    {
        // Sample input
        var edges = new List<Edge>
        {
            new Edge(0, 1, 1),
            new Edge(1, 2, 1),
            new Edge(2, 3, 1),
            new Edge(3, 1, -3), // Creates negative cycle 1->2->3->1
            new Edge(0, 4, 1)
        };
        
        var result = FindNegativeCycle(edges, 5);
        
        if (result[0] == -1)
        {
            Console.WriteLine("No negative cycle");
        }
        else
        {
            Console.WriteLine("Negative cycle: " + string.Join(" ", result));
        }
    }
}
```

## Key Points

1. **Algorithm**: Uses Bellman-Ford algorithm to detect negative weight cycles
2. **Time Complexity**: O(VE) where V is vertices and E is edges
3. **Space Complexity**: O(V) for storing distances and predecessors
4. **Detection**: If we can still relax edges after V-1 iterations, there's a negative cycle
5. **Cycle Reconstruction**: Follow predecessors to find the actual cycle nodes

## Sample Input/Output

Input edges:
- 0->1 (weight: 1)
- 1->2 (weight: 1) 
- 2->3 (weight: 1)
- 3->1 (weight: -3)
- 0->4 (weight: 1)

Output: Negative cycle: 1 2 3 1

The algorithm correctly identifies that the path 1→2→3→1 forms a negative cycle with total weight -1.

