# Rosalind Problem: Compute Distances Between Leaves

## Problem Description
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution Approach
I'll use a graph representation with adjacency lists and implement a breadth-first search (BFS) to find distances between leaves.

## Scala Implementation

```scala
import scala.collection.mutable

object ComputeDistancesBetweenLeaves {
  
  case class Edge(from: Int, to: Int, weight: Int)
  
  def computeDistances(tree: List[Edge], n: Int): List[List[Int]] = {
    // Build adjacency list representation
    val adjList = mutable.Map[Int, List[(Int, Int)]]()
    
    // Initialize adjacency list
    for (i <- 0 until n) {
      adjList(i) = List()
    }
    
    // Populate adjacency list
    for (edge <- tree) {
      adjList(edge.from) = adjList(edge.from) :+ (edge.to, edge.weight)
      adjList(edge.to) = adjList(edge.to) :+ (edge.from, edge.weight)
    }
    
    // Function to find distance between two nodes using BFS
    def bfsDistance(start: Int, end: Int): Int = {
      if (start == end) return 0
      
      val visited = mutable.Set[Int]()
      val queue = mutable.Queue[(Int, Int)]() // (node, distance)
      
      visited += start
      queue.enqueue((start, 0))
      
      while (queue.nonEmpty) {
        val (currentNode, currentDist) = queue.dequeue()
        
        for ((neighbor, weight) <- adjList(currentNode)) {
          if (neighbor == end) {
            return currentDist + weight
          }
          
          if (!visited.contains(neighbor)) {
            visited += neighbor
            queue.enqueue((neighbor, currentDist + weight))
          }
        }
      }
      
      -1 // Should not happen for connected tree
    }
    
    // Compute distances between all pairs of leaves (0 to n-1)
    val distances = mutable.ListBuffer[List[Int]]()
    
    for (i <- 0 until n) {
      val row = mutable.ListBuffer[Int]()
      for (j <- 0 until n) {
        row += bfsDistance(i, j)
      }
      distances += row.toList
    }
    
    distances.toList
  }
  
  // Alternative approach using tree traversal for more efficient computation
  def computeDistancesOptimized(tree: List[Edge], n: Int): List[List[Int]] = {
    // Build adjacency list
    val adjList = mutable.Map[Int, List[(Int, Int)]]()
    
    for (i <- 0 until n) {
      adjList(i) = List()
    }
    
    for (edge <- tree) {
      adjList(edge.from) = adjList(edge.from) :+ (edge.to, edge.weight)
      adjList(edge.to) = adjList(edge.to) :+ (edge.from, edge.weight)
    }
    
    // Precompute all distances using BFS from each node
    val distances = Array.ofDim[Int](n, n)
    
    for (i <- 0 until n) {
      val visited = mutable.Set[Int]()
      val queue = mutable.Queue[(Int, Int)]()
      
      visited += i
      queue.enqueue((i, 0))
      
      while (queue.nonEmpty) {
        val (currentNode, currentDist) = queue.dequeue()
        distances(i)(currentNode) = currentDist
        
        for ((neighbor, weight) <- adjList(currentNode)) {
          if (!visited.contains(neighbor)) {
            visited += neighbor
            queue.enqueue((neighbor, currentDist + weight))
          }
        }
      }
    }
    
    distances.map(_.toList).toList
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage:
    // Input format: List of edges in format (from, to, weight)
    val tree = List(
      Edge(0, 1, 3),
      Edge(1, 2, 2),
      Edge(2, 3, 1),
      Edge(3, 4, 4)
    )
    
    val n = 5
    val result = computeDistancesOptimized(tree, n)
    
    // Print results
    result.foreach(row => println(row.mkString(" ")))
  }
}
```

## Example Input/Output

**Input:**
```
Tree edges:
0-1: weight 3
1-2: weight 2
2-3: weight 1
3-4: weight 4
```

**Output:**
```
0 3 5 6 10
3 0 2 3 7
5 2 0 1 5
6 3 1 0 4
10 7 5 4 0
```

## Key Features

1. **Graph Representation**: Uses adjacency list to represent the tree
2. **BFS Algorithm**: Implements breadth-first search to find shortest paths
3. **Efficient Distance Calculation**: Precomputes all pairwise distances
4. **Scalable**: Works for trees with any number of nodes
5. **Memory Efficient**: Uses mutable collections for better performance

## Time Complexity
- **Time**: O(n²) where n is the number of nodes
- **Space**: O(n²) for storing the distance matrix

## Usage Notes
- The solution assumes the input represents a valid tree structure
- Leaves are numbered from 0 to n-1
- The output is a symmetric distance matrix where element [i][j] represents the distance between leaf i and leaf j

