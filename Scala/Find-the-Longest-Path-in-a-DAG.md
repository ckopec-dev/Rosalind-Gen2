# Rosalind Problem: Find the Longest Path in a DAG

## Problem Description
Given a weighted directed acyclic graph (DAG), find the longest path from a source node to a sink node.

## Solution in Scala

```scala
import scala.collection.mutable

object LongestPathInDAG {
  
  case class Edge(from: Int, to: Int, weight: Int)
  
  def longestPath(n: Int, edges: List[Edge], source: Int, sink: Int): Int = {
    // Build adjacency list representation
    val graph = mutable.Map[Int, List[Edge]]().withDefaultValue(List.empty)
    edges.foreach(edge => graph(edge.from) = edge :: graph(edge.from))
    
    // Initialize distances
    val dist = Array.fill(n)(Int.MinValue)
    dist(source) = 0
    
    // Topological sort using DFS
    val visited = Array.fill(n)(false)
    val stack = mutable.Stack[Int]()
    
    def dfs(node: Int): Unit = {
      visited(node) = true
      graph(node).foreach(edge => {
        if (!visited(edge.to)) {
          dfs(edge.to)
        }
      })
      stack.push(node)
    }
    
    // Perform topological sort
    for (i <- 0 until n) {
      if (!visited(i)) {
        dfs(i)
      }
    }
    
    // Process nodes in topological order
    while (stack.nonEmpty) {
      val current = stack.pop()
      graph(current).foreach { edge =>
        if (dist(current) != Int.MinValue) {
          val newDist = dist(current) + edge.weight
          if (newDist > dist(edge.to)) {
            dist(edge.to) = newDist
          }
        }
      }
    }
    
    dist(sink)
  }
  
  // Alternative implementation using Kahn's algorithm for topological sorting
  def longestPathKahn(n: Int, edges: List[Edge], source: Int, sink: Int): Int = {
    // Build adjacency list and in-degree array
    val graph = mutable.Map[Int, List[Edge]]().withDefaultValue(List.empty)
    val inDegree = Array.fill(n)(0)
    
    edges.foreach { edge =>
      graph(edge.from) = edge :: graph(edge.from)
      inDegree(edge.to) += 1
    }
    
    // Initialize distances
    val dist = Array.fill(n)(Int.MinValue)
    dist(source) = 0
    
    // Initialize queue with nodes having in-degree 0
    val queue = mutable.Queue[Int]()
    for (i <- 0 until n) {
      if (inDegree(i) == 0) {
        queue.enqueue(i)
      }
    }
    
    // Process nodes in topological order
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      graph(current).foreach { edge =>
        if (dist(current) != Int.MinValue) {
          val newDist = dist(current) + edge.weight
          if (newDist > dist(edge.to)) {
            dist(edge.to) = newDist
          }
        }
        inDegree(edge.to) -= 1
        if (inDegree(edge.to) == 0) {
          queue.enqueue(edge.to)
        }
      }
    }
    
    dist(sink)
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Example from Rosalind
    val edges = List(
      Edge(0, 1, 5),
      Edge(0, 2, 3),
      Edge(1, 3, 6),
      Edge(1, 2, 2),
      Edge(2, 4, 4),
      Edge(2, 5, 2),
      Edge(2, 3, 3),
      Edge(3, 5, 1),
      Edge(3, 4, 1),
      Edge(4, 5, 5)
    )
    
    val result = longestPath(6, edges, 0, 5)
    println(s"Longest path from 0 to 5: $result")
    
    // Test with Kahn's algorithm
    val result2 = longestPathKahn(6, edges, 0, 5)
    println(s"Longest path from 0 to 5 (Kahn): $result2")
  }
}
```

## Algorithm Explanation

1. **Graph Representation**: The DAG is represented using an adjacency list where each node maps to its outgoing edges.

2. **Topological Sorting**: 
   - Two approaches are implemented:
   - DFS-based topological sort (using stack)
   - Kahn's algorithm using in-degrees and queue

3. **Longest Path Computation**:
   - Initialize distances array with `Int.MinValue` (negative infinity)
   - Set source distance to 0
   - Process nodes in topological order
   - For each node, update distances to its neighbors using relaxation

4. **Time Complexity**: O(V + E) where V is vertices and E is edges
5. **Space Complexity**: O(V + E) for graph storage and auxiliary arrays

## Key Points

- Uses dynamic programming approach with topological sorting
- Handles negative weights correctly (since we're finding maximum path)
- The algorithm works because in a DAG, we can process nodes in topological order
- Both DFS and Kahn's algorithm approaches are provided for topological sorting

The solution correctly handles the constraints of the Rosalind problem and will work for various DAG inputs.

