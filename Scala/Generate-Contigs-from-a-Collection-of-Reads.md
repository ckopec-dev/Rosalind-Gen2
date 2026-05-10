# Rosalind Problem: Generate Contigs from a Collection of Reads (Scala Solution)

## Problem Understanding

The task is to generate contigs from a collection of DNA reads. Contigs are maximal sequences that can be assembled from the given reads, where each contig is formed by overlapping reads.

## Solution Approach

We'll use a de Bruijn graph approach:
1. Create a graph where each k-1 length prefix and suffix of reads are nodes
2. Connect nodes with edges representing overlaps
3. Find Eulerian paths to build contigs

## Scala Implementation

```scala
import scala.collection.mutable

object GenerateContigs {
  
  def generateContigs(reads: List[String]): List[String] = {
    if (reads.isEmpty) return List()
    
    val k = reads.head.length
    val graph = mutable.Map[String, List[String]]()
    val inDegree = mutable.Map[String, Int]()
    val outDegree = mutable.Map[String, Int]()
    
    // Build de Bruijn graph
    for (read <- reads) {
      val prefix = read.substring(0, k - 1)
      val suffix = read.substring(1, k)
      
      // Add edges to graph
      if (!graph.contains(prefix)) {
        graph(prefix) = List()
      }
      graph(prefix) = graph(prefix) :+ suffix
      
      // Update degrees
      inDegree(suffix) = inDegree.getOrElse(suffix, 0) + 1
      outDegree(prefix) = outDegree.getOrElse(prefix, 0) + 1
    }
    
    // Find nodes with unequal in/out degrees
    val unbalancedNodes = (inDegree.keySet ++ outDegree.keySet).filter { node =>
      inDegree.getOrElse(node, 0) != outDegree.getOrElse(node, 0)
    }
    
    // Find all Eulerian paths
    val contigs = mutable.ListBuffer[String]()
    
    // Find all nodes with out-degree > 0
    val startNodes = outDegree.keySet.filter(outDegree(_) > 0)
    
    for (startNode <- startNodes) {
      val path = eulerianPath(graph, startNode)
      if (path.nonEmpty) {
        val contig = path.head + path.tail.map(_.last).mkString("")
        contigs += contig
      }
    }
    
    contigs.toList
  }
  
  def eulerianPath(graph: mutable.Map[String, List[String]], startNode: String): List[String] = {
    val stack = mutable.Stack[String]()
    val path = mutable.ListBuffer[String]()
    
    stack.push(startNode)
    
    while (stack.nonEmpty) {
      val current = stack.top
      if (graph.contains(current) && graph(current).nonEmpty) {
        val next = graph(current).head
        stack.push(next)
        graph(current) = graph(current).tail
      } else {
        stack.pop()
        path += current
      }
    }
    
    path.toList.reverse
  }
  
  // Alternative simpler approach for this specific problem
  def generateContigsSimple(reads: List[String]): List[String] = {
    if (reads.isEmpty) return List()
    
    val k = reads.head.length
    val prefixMap = mutable.Map[String, List[String]]()
    val suffixMap = mutable.Map[String, List[String]]()
    
    // Build prefix and suffix maps
    for (read <- reads) {
      val prefix = read.substring(0, k - 1)
      val suffix = read.substring(1, k)
      
      prefixMap(prefix) = prefixMap.getOrElse(prefix, List()) :+ suffix
      suffixMap(suffix) = suffixMap.getOrElse(suffix, List()) :+ prefix
    }
    
    // Find nodes that are not balanced (in-degree != out-degree)
    val startNodes = prefixMap.keySet.filter { prefix =>
      val outDegree = prefixMap(prefix).length
      val inDegree = suffixMap.getOrElse(prefix, List()).length
      outDegree > inDegree
    }
    
    val contigs = mutable.ListBuffer[String]()
    
    for (start <- startNodes) {
      val path = findPath(prefixMap, start)
      if (path.nonEmpty) {
        val contig = path.head + path.tail.map(_.last).mkString("")
        contigs += contig
      }
    }
    
    contigs.toList
  }
  
  def findPath(graph: mutable.Map[String, List[String]], start: String): List[String] = {
    val path = mutable.ListBuffer[String]()
    var current = start
    path += current
    
    while (graph.contains(current) && graph(current).nonEmpty) {
      val next = graph(current).head
      graph(current) = graph(current).tail
      current = next
      path += current
    }
    
    path.toList
  }
  
  // Most straightforward approach for this problem
  def generateContigsFinal(reads: List[String]): List[String] = {
    if (reads.isEmpty) return List()
    
    val k = reads.head.length
    val edges = mutable.Map[String, List[String]]()
    
    // Build edges from reads
    for (read <- reads) {
      val prefix = read.substring(0, k - 1)
      val suffix = read.substring(1, k)
      
      if (!edges.contains(prefix)) {
        edges(prefix) = List()
      }
      edges(prefix) = edges(prefix) :+ suffix
    }
    
    // Find all contigs
    val contigs = mutable.ListBuffer[String]()
    
    // For each node with outgoing edges
    for ((node, neighbors) <- edges if neighbors.nonEmpty) {
      // Start a path from this node
      val path = mutable.ListBuffer(node)
      var current = node
      
      // Follow the path until we hit a dead end
      while (edges.contains(current) && edges(current).nonEmpty) {
        val next = edges(current).head
        edges(current) = edges(current).tail
        path += next
        current = next
      }
      
      // Build contig from path
      if (path.length > 1) {
        val contig = path.head + path.tail.map(_.last).mkString("")
        contigs += contig
      }
    }
    
    contigs.toList
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val reads = List("ATGCG", "GCATG", "CATGC", "AGGCA", "GGCAT")
    val contigs = generateContigsFinal(reads)
    println(contigs.mkString(" "))
  }
}
```

## Key Concepts

1. **De Bruijn Graph**: Each read is represented as a path of k-1 length nodes
2. **Eulerian Path**: Finding paths that traverse each edge exactly once
3. **Contig Generation**: Concatenating paths to form maximal sequences

## Time and Space Complexity

- **Time Complexity**: O(|E| + |V|) where E is the number of edges and V is the number of vertices in the graph
- **Space Complexity**: O(|E| + |V|) for storing the graph

## Example

For input reads: `["ATGCG", "GCATG", "CATGC", "AGGCA", "GGCAT"]`
The solution would generate contigs by finding overlapping patterns and assembling maximal sequences.

This approach efficiently handles the contig generation problem by leveraging graph theory concepts and Eulerian path algorithms.

