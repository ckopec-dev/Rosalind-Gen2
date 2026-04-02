# Rosalind Problem: Reconstruct a String from its Paired Composition

## Problem Description
Given the paired composition of a string, reconstruct the original string.

## Solution

```scala
object ReconstructStringFromPairedComposition {
  
  def reconstructString(pairedComposition: List[(String, String)]): String = {
    // Build de Bruijn graph from paired composition
    val graph = buildDeBruijnGraph(pairedComposition)
    
    // Find Eulerian path in the graph
    val eulerianPath = findEulerianPath(graph)
    
    // Reconstruct the string from the path
    reconstructFromPath(eulerianPath)
  }
  
  def buildDeBruijnGraph(pairedComposition: List[(String, String)]): Map[String, List[String]] = {
    val graph = scala.collection.mutable.Map[String, List[String]]()
    
    for ((prefix, suffix) <- pairedComposition) {
      val k = prefix.length
      val prefixK1 = prefix.substring(0, k - 1)
      val suffixK1 = suffix.substring(0, k - 1)
      
      val node = (prefixK1, suffixK1)
      val nextNode = (prefix.substring(1), suffix.substring(1))
      
      if (!graph.contains(node)) {
        graph(node) = List()
      }
      
      graph(node) = graph(node) :+ nextNode
    }
    
    graph.toMap
  }
  
  def findEulerianPath(graph: Map[String, List[String]]): List[String] = {
    // Find starting node (node with out-degree - in-degree = 1)
    val allNodes = graph.keys.toList
    val inDegrees = allNodes.flatMap(node => graph.getOrElse(node, List()).map(_ -> node))
    val outDegrees = graph.keys.toList.flatMap(node => graph.getOrElse(node, List()).map(_ -> node))
    
    val startNode = graph.keys.find { node =>
      val outDeg = graph.getOrElse(node, List()).length
      val inDeg = inDegrees.count(_._2 == node)
      outDeg - inDeg == 1
    }.getOrElse(graph.keys.head)
    
    // Hierholzer's algorithm for Eulerian path
    val path = scala.collection.mutable.ListBuffer[String]()
    val stack = scala.collection.mutable.Stack[String]()
    stack.push(startNode)
    
    while (stack.nonEmpty) {
      val current = stack.top
      val neighbors = graph.getOrElse(current, List())
      
      if (neighbors.nonEmpty) {
        val next = neighbors.head
        stack.push(next)
        graph(current) = neighbors.tail
      } else {
        stack.pop()
        path.prepend(current)
      }
    }
    
    path.toList
  }
  
  def reconstructFromPath(path: List[String]): String = {
    if (path.isEmpty) return ""
    
    val first = path.head
    val k = first.length
    val k1 = k - 1
    
    val prefix = path.head._1
    val suffix = path.head._2
    
    val reconstructed = path.tail.map(node => node._1.takeRight(1)).mkString("")
    
    prefix + reconstructed
  }
  
  // Alternative cleaner implementation for paired de Bruijn graph
  def reconstructFromPairedComposition(pairedComposition: List[(String, String)]): String = {
    if (pairedComposition.isEmpty) return ""
    
    val k = pairedComposition.head._1.length
    val k1 = k - 1
    
    // Build adjacency list for paired de Bruijn graph
    val adjList = scala.collection.mutable.Map[String, List[(String, String)]]()
    
    for ((prefix, suffix) <- pairedComposition) {
      val prefixK1 = prefix.substring(0, k1)
      val suffixK1 = suffix.substring(0, k1)
      val node = (prefixK1, suffixK1)
      
      val nextPrefix = prefix.substring(1)
      val nextSuffix = suffix.substring(1)
      val nextNode = (nextPrefix, nextSuffix)
      
      if (!adjList.contains(node)) {
        adjList(node) = List()
      }
      adjList(node) = adjList(node) :+ nextNode
    }
    
    // Find Eulerian path using Hierholzer's algorithm
    val startNode = adjList.keys.head
    val path = scala.collection.mutable.ListBuffer[String]()
    val stack = scala.collection.mutable.Stack[String]()
    
    stack.push(startNode)
    
    while (stack.nonEmpty) {
      val current = stack.top
      val neighbors = adjList.getOrElse(current, List())
      
      if (neighbors.nonEmpty) {
        val next = neighbors.head
        stack.push(next)
        adjList(current) = neighbors.tail
      } else {
        stack.pop()
        path.prepend(current)
      }
    }
    
    // Reconstruct the string from the path
    val firstPrefix = path.head._1
    val firstSuffix = path.head._2
    
    val prefixReconstruction = path.tail.map(_._1.takeRight(1)).mkString("")
    val suffixReconstruction = path.tail.map(_._2.takeRight(1)).mkString("")
    
    firstPrefix + prefixReconstruction + "|" + firstSuffix + suffixReconstruction
  }
  
  // Simpler approach - reconstruct the full string
  def solve(pairedComposition: List[(String, String)]): String = {
    if (pairedComposition.isEmpty) return ""
    
    val k = pairedComposition.head._1.length
    val k1 = k - 1
    
    // Build the paired de Bruijn graph
    val graph = scala.collection.mutable.Map[String, List[String]]()
    
    for ((prefix, suffix) <- pairedComposition) {
      val prefixK1 = prefix.substring(0, k1)
      val suffixK1 = suffix.substring(0, k1)
      val node = prefixK1 + "|" + suffixK1
      
      val nextPrefix = prefix.substring(1)
      val nextSuffix = suffix.substring(1)
      val nextNode = nextPrefix + "|" + nextSuffix
      
      if (!graph.contains(node)) {
        graph(node) = List()
      }
      graph(node) = graph(node) :+ nextNode
    }
    
    // Find Eulerian path
    val path = scala.collection.mutable.ListBuffer[String]()
    val stack = scala.collection.mutable.Stack[String]()
    
    // Find start node
    val startNode = graph.keys.head
    stack.push(startNode)
    
    while (stack.nonEmpty) {
      val current = stack.top
      val neighbors = graph.getOrElse(current, List())
      
      if (neighbors.nonEmpty) {
        val next = neighbors.head
        stack.push(next)
        graph(current) = neighbors.tail
      } else {
        stack.pop()
        path.prepend(current)
      }
    }
    
    // Reconstruct string
    val first = path.head.split("\\|").toList
    val prefix = first.head
    val suffix = first(1)
    
    val prefixReconstruction = path.tail.map(_.split("\\|")(0).takeRight(1)).mkString("")
    val suffixReconstruction = path.tail.map(_.split("\\|")(1).takeRight(1)).mkString("")
    
    prefix + prefixReconstruction + "|" + suffix + suffixReconstruction
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val pairedComp = List(
      ("GAGA", "TTGA"),
      ("TCGT", "GAGA"),
      ("CGTG", "TTCG"),
      ("GAGA", "TTGA"),
      ("TTCG", "CGTG")
    )
    
    val result = solve(pairedComp)
    println(result)
  }
}
```

## Explanation

This solution implements the reconstruction of a string from its paired composition using:

1. **Graph Construction**: Build a paired de Bruijn graph where each node represents a k-1 mer pair and edges represent transitions.

2. **Eulerian Path Finding**: Use Hierholzer's algorithm to find an Eulerian path through the graph.

3. **String Reconstruction**: Reconstruct the original string from the Eulerian path.

## Key Steps:

1. **Build the graph**: Convert paired composition into a de Bruijn graph representation
2. **Find Eulerian path**: Use graph traversal to find the path that visits all edges exactly once
3. **Reconstruct string**: Convert the path back into the original DNA string

## Time Complexity: O(|E| + |V|) where E is the number of edges and V is the number of vertices
## Space Complexity: O(|E| + |V|)

The solution handles the paired composition format where each element consists of two strings (prefix and suffix) of equal length, and reconstructs the original DNA string that generated this paired composition.

