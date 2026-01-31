# Rosalind Problem: Hamiltonian Path in DAG

## Problem Statement
Given a directed acyclic graph (DAG), determine whether there exists a Hamiltonian path (a path that visits each vertex exactly once).

## Solution Approach
We'll use dynamic programming with bitmasks to solve this problem efficiently:
1. Use bitmask DP where `dp[mask][v]` represents whether we can reach vertex `v` using exactly the vertices in `mask`
2. For each state, try extending it by moving to adjacent vertices
3. Check if any complete path (using all vertices) exists

## Scala Implementation

```scala
import scala.io.Source
import scala.collection.mutable

object HamiltonianPathInDAG {
  
  def main(args: Array[String]): Unit = {
    // Read input from file or stdin
    val input = Source.fromFile("rosalind_hdag.txt").getLines().toList
    val lines = input.drop(1) // Skip first line with vertex/edge count
    
    // Parse the graph
    val graph = parseGraph(lines)
    
    // Find Hamiltonian path
    val result = findHamiltonianPath(graph)
    
    // Output result
    println(result.map(_.toString).mkString(" "))
  }
  
  def parseGraph(lines: List[String]): Map[Int, List[Int]] = {
    val graph = mutable.Map[Int, List[Int]]()
    
    lines.foreach { line =>
      val parts = line.trim.split("\\s+").map(_.toInt)
      if (parts.length >= 2) {
        val from = parts(0)
        val to = parts(1)
        
        if (!graph.contains(from)) {
          graph(from) = List()
        }
        graph(from) = graph(from) :+ to
        
        // Ensure target vertex exists in graph
        if (!graph.contains(to)) {
          graph(to) = List()
        }
      }
    }
    
    graph.toMap
  }
  
  def findHamiltonianPath(graph: Map[Int, List[Int]]): Option[List[Int]] = {
    val vertices = graph.keys.toList
    val n = vertices.length
    
    if (n == 0) return Some(List())
    if (n == 1) return Some(List(vertices.head))
    
    // Create a mapping from vertex to index for bitmask operations
    val vertexToIndex = vertices.zipWithIndex.toMap
    val indexToVertex = vertices.zipWithIndex.map { case (v, i) => (i, v) }.toMap
    
    // dp[mask][v] = whether we can reach vertex v using exactly the vertices in mask
    val dp = Array.ofDim[Boolean](1 << n, n)
    
    // Initialize: start with each vertex alone
    for (i <- vertices.indices) {
      val vertex = vertices(i)
      val index = vertexToIndex(vertex)
      dp(1 << index)(index) = true
    }
    
    // Fill DP table
    for (mask <- 1 until (1 << n)) {
      for (u <- vertices.indices) {
        if ((mask & (1 << u)) != 0 && dp(mask)(u)) {
          // Try extending path with neighbors of vertex u
          val neighbors = graph.getOrElse(vertices(u), List())
          for (v <- neighbors) {
            val vIndex = vertexToIndex(v)
            if ((mask & (1 << vIndex)) == 0) { // v not yet visited
              val newMask = mask | (1 << vIndex)
              dp(newMask)(vIndex) = true
            }
          }
        }
      }
    }
    
    // Check if any complete path exists
    val fullMask = (1 << n) - 1
    for (lastVertex <- vertices.indices) {
      if (dp(fullMask)(lastVertex)) {
        // Reconstruct path
        val path = reconstructPath(dp, graph, vertices, fullMask, lastVertex)
        return Some(path)
      }
    }
    
    None
  }
  
  def reconstructPath(dp: Array[Array[Boolean]], 
                     graph: Map[Int, List[Int]], 
                     vertices: List[Int], 
                     fullMask: Int, 
                     lastVertex: Int): List[Int] = {
    val path = mutable.ListBuffer[Int]()
    var currentMask = fullMask
    var currentVertex = lastVertex
    
    // Backtrack to reconstruct path
    while (currentMask != 0) {
      path += vertices(currentVertex)
      currentMask ^= (1 << currentVertex)
      
      // Find previous vertex in path
      val prevMask = currentMask
      var found = false
      
      for (i <- vertices.indices) {
        if ((prevMask & (1 << i)) != 0 && dp(prevMask)(i)) {
          // Check if we can go from i to currentVertex
          val neighbors = graph.getOrElse(vertices(i), List())
          if (neighbors.contains(vertices(currentVertex))) {
            currentVertex = i
            found = true
            break
          }
        }
      }
      
      if (!found) {
        // This shouldn't happen if DP is correct
        return List()
      }
    }
    
    path.reverse.toList
  }
  
  // Alternative simpler approach for small graphs
  def findHamiltonianPathSimple(graph: Map[Int, List[Int]]): Option[List[Int]] = {
    val vertices = graph.keys.toList
    val n = vertices.length
    
    if (n == 0) return Some(List())
    if (n == 1) return Some(List(vertices.head))
    
    // Try all permutations (only for small graphs)
    if (n <= 8) {
      val permutations = permute(vertices)
      for (perm <- permutations) {
        if (isHamiltonianPath(graph, perm)) {
          return Some(perm)
        }
      }
    }
    
    // For larger graphs, use the DP approach
    findHamiltonianPath(graph)
  }
  
  def permute[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) return List(List())
    val head = list.head
    val tail = list.tail
    val perms = permute(tail)
    perms.flatMap(perm => (0 to perm.length).map(i => perm.take(i) ++ List(head) ++ perm.drop(i)))
  }
  
  def isHamiltonianPath(graph: Map[Int, List[Int]], path: List[Int]): Boolean = {
    if (path.length <= 1) return true
    
    for (i <- 0 until path.length - 1) {
      val current = path(i)
      val next = path(i + 1)
      val neighbors = graph.getOrElse(current, List())
      if (!neighbors.contains(next)) {
        return false
      }
    }
    
    true
  }
}
```

## Example Usage

For a sample input file `rosalind_hdag.txt`:
```
6 7
1 2
2 3
3 4
4 5
5 6
1 3
2 4
```

The program would output a Hamiltonian path if one exists, or handle the case appropriately.

## Time and Space Complexity

- **Time Complexity**: O(2^n × n) where n is the number of vertices
- **Space Complexity**: O(2^n × n) for the DP table

## Key Features

1. **Bitmask DP**: Efficiently tracks which vertices have been visited
2. **Path Reconstruction**: Rebuilds the actual Hamiltonian path from DP states
3. **Edge Case Handling**: Properly handles empty graphs and single vertex graphs
4. **Input Parsing**: Reads and parses the graph from Rosalind format

This solution efficiently handles the constraints of the Hamiltonian Path problem in DAGs using dynamic programming with bitmasks.

