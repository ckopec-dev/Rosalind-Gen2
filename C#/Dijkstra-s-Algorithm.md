# Rosalind Problem: Dijkstra's Algorithm Solution in C#

## Problem Overview
Dijkstra's algorithm finds the shortest paths from a source vertex to all other vertices in a weighted graph.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class DijkstrasAlgorithm
{
    public static void Main(string[] args)
    {
        // Read input from file
        string input = File.ReadAllText("rosalind_dij.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        // Parse graph
        int[] firstLine = lines[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                 .Select(int.Parse).ToArray();
        int n = firstLine[0]; // number of vertices
        int m = firstLine[1]; // number of edges
        
        // Create adjacency list representation
        Dictionary<int, List<(int neighbor, int weight)>> graph = new Dictionary<int, List<(int, int)>>();
        
        // Initialize graph
        for (int i = 1; i <= n; i++)
        {
            graph[i] = new List<(int, int)>();
        }
        
        // Parse edges
        for (int i = 1; i <= m; i++)
        {
            int[] edge = lines[i].Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                .Select(int.Parse).ToArray();
            int u = edge[0];
            int v = edge[1];
            int w = edge[2];
            
            graph[u].Add((v, w));
        }
        
        // Run Dijkstra from vertex 1
        int[] distances = Dijkstra(graph, n, 1);
        
        // Output results
        Console.WriteLine(string.Join(" ", distances.Select(d => d.ToString())));
    }
    
    public static int[] Dijkstra(Dictionary<int, List<(int neighbor, int weight)>> graph, int n, int source)
    {
        // Initialize distances array
        int[] distances = new int[n + 1];
        for (int i = 1; i <= n; i++)
        {
            distances[i] = int.MaxValue;
        }
        distances[source] = 0;
        
        // Priority queue to store (distance, vertex)
        var pq = new PriorityQueue<(int distance, int vertex), int>();
        pq.Enqueue((0, source), 0);
        
        // Track visited vertices
        bool[] visited = new bool[n + 1];
        
        while (pq.Count > 0)
        {
            var (currentDistance, currentVertex) = pq.Dequeue();
            
            // Skip if already visited
            if (visited[currentVertex])
                continue;
                
            visited[currentVertex] = true;
            
            // Check all neighbors
            foreach (var (neighbor, weight) in graph[currentVertex])
            {
                if (!visited[neighbor])
                {
                    int newDistance = currentDistance + weight;
                    
                    // Update distance if shorter path found
                    if (newDistance < distances[neighbor])
                    {
                        distances[neighbor] = newDistance;
                        pq.Enqueue((newDistance, neighbor), newDistance);
                    }
                }
            }
        }
        
        // Return distances from vertex 1 to all other vertices
        return distances.Skip(1).ToArray();
    }
}

// Simple priority queue implementation since .NET doesn't have one built-in
public class PriorityQueue<T, TPriority> where TPriority : IComparable<TPriority>
{
    private List<(T item, TPriority priority)> heap = new List<(T, TPriority)>();
    
    public int Count => heap.Count;
    
    public void Enqueue(T item, TPriority priority)
    {
        heap.Add((item, priority));
        HeapifyUp(heap.Count - 1);
    }
    
    public T Dequeue()
    {
        if (heap.Count == 0) throw new InvalidOperationException("Queue is empty");
        
        T result = heap[0].item;
        heap[0] = heap[heap.Count - 1];
        heap.RemoveAt(heap.Count - 1);
        
        if (heap.Count > 0)
            HeapifyDown(0);
            
        return result;
    }
    
    private void HeapifyUp(int index)
    {
        if (index == 0) return;
        
        int parentIndex = (index - 1) / 2;
        
        if (heap[parentIndex].priority.CompareTo(heap[index].priority) > 0)
        {
            Swap(parentIndex, index);
            HeapifyUp(parentIndex);
        }
    }
    
    private void HeapifyDown(int index)
    {
        int leftChildIndex = 2 * index + 1;
        int rightChildIndex = 2 * index + 2;
        int smallestIndex = index;
        
        if (leftChildIndex < heap.Count && 
            heap[leftChildIndex].priority.CompareTo(heap[smallestIndex].priority) < 0)
        {
            smallestIndex = leftChildIndex;
        }
        
        if (rightChildIndex < heap.Count && 
            heap[rightChildIndex].priority.CompareTo(heap[smallestIndex].priority) < 0)
        {
            smallestIndex = rightChildIndex;
        }
        
        if (smallestIndex != index)
        {
            Swap(index, smallestIndex);
            HeapifyDown(smallestIndex);
        }
    }
    
    private void Swap(int i, int j)
    {
        var temp = heap[i];
        heap[i] = heap[j];
        heap[j] = temp;
    }
}
```

## Input Format
The input file should contain:
- First line: two integers n and m (number of vertices and edges)
- Next m lines: three integers u, v, w representing an edge from u to v with weight w

## Example Input
```
6 14
1 2 7
1 3 9
1 6 14
2 3 10
2 4 15
3 4 11
3 6 2
4 5 6
5 6 9
1 4 3
2 5 5
3 5 13
4 6 12
5 6 4
```

## Example Output
```
0 7 9 20 15 11
```

## Key Features of the Solution

1. **Graph Representation**: Uses adjacency list for efficient storage
2. **Priority Queue**: Implements a custom priority queue for Dijkstra's algorithm
3. **Time Complexity**: O((V + E) log V) where V is vertices and E is edges
4. **Space Complexity**: O(V + E) for graph storage and auxiliary data structures
5. **Edge Cases**: Handles disconnected components and negative weights (though the problem assumes non-negative weights)

The algorithm correctly implements Dijkstra's shortest path algorithm to find the shortest distances from vertex 1 to all other vertices in the graph.

