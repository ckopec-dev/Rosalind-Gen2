# Rosalind Problem: Testing Bipartiteness - Scala Solution

## Problem Understanding

A graph is bipartite if its vertices can be divided into two disjoint sets such that every edge connects a vertex from one set to a vertex from the other set. In other words, the graph can be colored using only two colors such that no two adjacent vertices have the same color.

## Solution Approach

I'll use a BFS (Breadth-First Search) approach to check if the graph is bipartite:
1. Color each vertex with one of two colors (0 or 1)
2. Start with any unvisited vertex and color it with color 0
3. For each vertex, color all its neighbors with the opposite color
4. If we find a neighbor that's already colored with the same color, the graph is not bipartite

## Scala Implementation

```scala
import scala.collection.mutable.Queue
import scala.io.Source

object Bipartite {
  
  def isBipartite(graph: Map[Int, List[Int]], numVertices: Int): Boolean = {
    // Color array: -1 = unvisited, 0 = color 1, 1 = color 2
    val colors = Array.fill[Int](numVertices + 1)(-1)
    
    // Check all components of the graph
    for (vertex <- 1 to numVertices) {
      if (colors(vertex) == -1) {
        // Start BFS from this vertex
        val queue = Queue[Int]()
        queue.enqueue(vertex)
        colors(vertex) = 0
        
        while (queue.nonEmpty) {
          val currentVertex = queue.dequeue()
          val currentColor = colors(currentVertex)
          val oppositeColor = 1 - currentColor // Toggle between 0 and 1
          
          // Check all neighbors
          for (neighbor <- graph.getOrElse(currentVertex, List())) {
            if (colors(neighbor) == -1) {
              // Color the neighbor with opposite color
              colors(neighbor) = oppositeColor
              queue.enqueue(neighbor)
            } else if (colors(neighbor) == currentColor) {
              // If neighbor has same color as current vertex, not bipartite
              return false
            }
          }
        }
      }
    }
    
    true
  }
  
  def main(args: Array[String]): Unit = {
    // Read input from file (assuming input format from Rosalind)
    val input = Source.fromFile("rosalind_bip.txt").getLines().toList
    val lines = input.drop(1) // Skip first line which contains graph info
    
    // Parse the graph
    val graph = scala.collection.mutable.Map[Int, List[Int]]()
    val numVertices = lines.head.split(" ").head.toInt
    
    // Process edges (assuming input format: vertex1 vertex2)
    for (line <- lines.tail if line.nonEmpty) {
      val Array(u, v) = line.split(" ").map(_.toInt)
      
      // Add edge u-v
      graph(u) = v :: graph.getOrElse(u, List())
      graph(v) = u :: graph.getOrElse(v, List())
    }
    
    // Check if graph is bipartite
    val result = isBipartite(graph.toMap, numVertices)
    
    // Output result (1 if bipartite, -1 if not)
    if (result) println(1) else println(-1)
  }
}
```

## Alternative Cleaner Implementation

```scala
import scala.collection.mutable.Queue

object BipartiteChecker {
  
  def isBipartite(graph: Map[Int, List[Int]], numVertices: Int): Boolean = {
    val colors = Array.fill[Int](numVertices + 1)(-1)
    
    for (startVertex <- 1 to numVertices if colors(startVertex) == -1) {
      val queue = Queue[Int]()
      colors(startVertex) = 0
      queue.enqueue(startVertex)
      
      while (queue.nonEmpty) {
        val vertex = queue.dequeue()
        val currentColor = colors(vertex)
        val nextColor = 1 - currentColor
        
        for (neighbor <- graph.getOrElse(vertex, List())) {
          if (colors(neighbor) == -1) {
            colors(neighbor) = nextColor
            queue.enqueue(neighbor)
          } else if (colors(neighbor) == currentColor) {
            return false
          }
        }
      }
    }
    
    true
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Example graph: 1-2, 2-3, 3-4, 4-1 (cycle of length 4 - bipartite)
    val exampleGraph = Map(
      1 -> List(2, 4),
      2 -> List(1, 3),
      3 -> List(2, 4),
      4 -> List(1, 3)
    )
    
    val result = isBipartite(exampleGraph, 4)
    println(if (result) 1 else -1) // Should output 1
    
    // Example non-bipartite graph: 1-2, 2-3, 3-1 (triangle - not bipartite)
    val nonBipartiteGraph = Map(
      1 -> List(2, 3),
      2 -> List(1, 3),
      3 -> List(1, 2)
    )
    
    val result2 = isBipartite(nonBipartiteGraph, 3)
    println(if (result2) 1 else -1) // Should output -1
  }
}
```

## Key Points

1. **Coloring Approach**: Uses a two-color coloring scheme where each vertex gets either color 0 or 1
2. **BFS Traversal**: Ensures we check all vertices in connected components
3. **Edge Case Handling**: Properly handles disconnected graphs by checking all components
4. **Time Complexity**: O(V + E) where V is vertices and E is edges
5. **Space Complexity**: O(V) for the colors array and queue

## Input Format

The input typically follows Rosalind's format:
```
n m
u1 v1
u2 v2
...
um vm
```
Where n is the number of vertices, m is the number of edges, and each subsequent line represents an edge.

## Output

- `1` if the graph is bipartite
- `-1` if the graph is not bipartite

This solution correctly handles the bipartiteness testing problem by implementing a BFS-based coloring algorithm that efficiently determines whether a graph can be properly two-colored.

