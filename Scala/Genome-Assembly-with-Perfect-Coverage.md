# Rosalind Problem: Genome Assembly with Perfect Coverage (Scala Solution)

## Problem Understanding

In this problem, we need to perform genome assembly using De Bruijn graphs when we have perfect coverage. Given a set of k-mers (reads), we need to reconstruct the original DNA sequence.

## Approach

1. Build a De Bruijn graph from the k-mers
2. Find an Eulerian path in the graph
3. Reconstruct the genome sequence from the path

## Solution

```scala
import scala.collection.mutable
import scala.io.Source

object GenomeAssembly {
  
  def main(args: Array[String]): Unit = {
    // Read input from file or stdin
    val lines = Source.fromFile("rosalind_gasm.txt").getLines().toList
    val kMers = lines.filter(_.nonEmpty)
    
    val result = assembleGenome(kMers)
    println(result)
  }
  
  def assembleGenome(kMers: List[String]): String = {
    // Build De Bruijn graph
    val graph = buildDeBruijnGraph(kMers)
    
    // Find Eulerian path
    val eulerianPath = findEulerianPath(graph)
    
    // Reconstruct genome
    if (eulerianPath.nonEmpty) {
      reconstructGenome(eulerianPath, kMers.head.length)
    } else {
      "No Eulerian path found"
    }
  }
  
  def buildDeBruijnGraph(kMers: List[String]): Map[String, List[String]] = {
    val graph = mutable.Map[String, List[String]]().withDefaultValue(List())
    
    for (kMer <- kMers) {
      val prefix = kMer.dropRight(1)
      val suffix = kMer.tail
      
      if (graph.contains(prefix)) {
        graph(prefix) = graph(prefix) :+ suffix
      } else {
        graph(prefix) = List(suffix)
      }
    }
    
    graph.toMap
  }
  
  def findEulerianPath(graph: Map[String, List[String]]): List[String] = {
    // Find starting node (node with out-degree - in-degree = 1)
    val allNodes = graph.keys.toList ++ graph.values.flatten.toList
    val uniqueNodes = allNodes.distinct
    
    val inDegrees = mutable.Map[String, Int]().withDefaultValue(0)
    val outDegrees = mutable.Map[String, Int]().withDefaultValue(0)
    
    for ((node, neighbors) <- graph) {
      outDegrees(node) = neighbors.length
      for (neighbor <- neighbors) {
        inDegrees(neighbor) = inDegrees(neighbor) + 1
      }
    }
    
    // Find start node
    val startNode = uniqueNodes.find(node => outDegrees(node) - inDegrees(node) == 1)
      .getOrElse(uniqueNodes.head)
    
    // Find Eulerian path using Hierholzer's algorithm
    val path = mutable.ListBuffer[String]()
    val stack = mutable.Stack[String]()
    stack.push(startNode)
    
    while (stack.nonEmpty) {
      val current = stack.top
      if (graph.contains(current) && graph(current).nonEmpty) {
        val next = graph(current).head
        stack.push(next)
        // Remove the edge we just used
        val newNeighbors = graph(current).tail
        // Update graph (this is a simplified approach)
        stack.pop()
        stack.push(next)
      } else {
        path += stack.pop()
      }
    }
    
    path.reverse.toList
  }
  
  def reconstructGenome(path: List[String], k: Int): String = {
    if (path.isEmpty) return ""
    
    val firstKMer = path.head
    val rest = path.tail
    
    val genome = new StringBuilder()
    genome.append(firstKMer)
    
    for (kMer <- rest) {
      genome.append(kMer.last)
    }
    
    genome.toString
  }
  
  // Alternative implementation using a more robust Eulerian path finding
  def findEulerianPathRobust(graph: Map[String, List[String]]): List[String] = {
    val edges = mutable.Map[String, List[String]]().withDefaultValue(List())
    val edgeCount = mutable.Map[String, Int]().withDefaultValue(0)
    
    // Build adjacency list and count edges
    for ((node, neighbors) <- graph) {
      edges(node) = neighbors
      edgeCount(node) += neighbors.length
    }
    
    // Find starting node
    val startNode = edges.keys.find(node => 
      edgeCount.getOrElse(node, 0) - edges.getOrElse(node, List()).length == 1
    ).getOrElse(edges.keys.head)
    
    // Hierholzer's algorithm
    val stack = mutable.Stack[String]()
    val circuit = mutable.ListBuffer[String]()
    
    stack.push(startNode)
    
    while (stack.nonEmpty) {
      val current = stack.top
      if (edges.contains(current) && edges(current).nonEmpty) {
        val next = edges(current).head
        stack.push(next)
        // Remove the edge
        edges(current) = edges(current).tail
      } else {
        circuit += stack.pop()
      }
    }
    
    circuit.reverse.toList
  }
}

// More concise version for the specific problem
object RosalindGASM {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("rosalind_gasm.txt").getLines().toList
    val kmers = input.filter(_.nonEmpty)
    
    val result = solve(kmers)
    println(result)
  }
  
  def solve(kmers: List[String]): String = {
    // Build De Bruijn graph
    val graph = buildGraph(kmers)
    
    // Find Eulerian path
    val path = findEulerianPath(graph)
    
    // Reconstruct genome
    val k = kmers.head.length
    reconstruct(path, k)
  }
  
  def buildGraph(kmers: List[String]): Map[String, List[String]] = {
    val graph = mutable.Map[String, List[String]]()
    
    for (kmer <- kmers) {
      val prefix = kmer.take(kmer.length - 1)
      val suffix = kmer.drop(1)
      
      if (graph.contains(prefix)) {
        graph(prefix) = graph(prefix) :+ suffix
      } else {
        graph(prefix) = List(suffix)
      }
    }
    
    graph.toMap
  }
  
  def findEulerianPath(graph: Map[String, List[String]]): List[String] = {
    // Simple implementation for perfect coverage case
    val visited = mutable.Set[String]()
    val path = mutable.ListBuffer[String]()
    
    def dfs(node: String): Unit = {
      val neighbors = graph.getOrElse(node, List())
      for (neighbor <- neighbors) {
        val edge = node + "->" + neighbor
        if (!visited.contains(edge)) {
          visited += edge
          dfs(neighbor)
        }
      }
      path += node
    }
    
    // Start with first kmer
    val startNode = graph.keys.head
    dfs(startNode)
    
    path.reverse.toList
  }
  
  def reconstruct(path: List[String], k: Int): String = {
    if (path.isEmpty) return ""
    
    val result = new StringBuilder()
    result.append(path.head)
    
    for (i <- 1 until path.length) {
      result.append(path(i).last)
    }
    
    result.toString
  }
}
```

## Explanation

This solution implements genome assembly using the De Bruijn graph approach:

1. **Graph Construction**: For each k-mer, we extract its prefix (k-1 characters) and suffix (k-1 characters) and create edges between them.

2. **Eulerian Path Finding**: We use a modified version of Hierholzer's algorithm to find an Eulerian path through the graph.

3. **Genome Reconstruction**: We reconstruct the original sequence by concatenating the first k-mer and the last character of each subsequent k-mer in the path.

## Time Complexity
- Graph construction: O(n × k) where n is the number of k-mers and k is the k-mer length
- Eulerian path finding: O(n + k)
- Overall: O(n × k)

## Space Complexity
- O(n × k) for storing the graph and path

This solution handles the perfect coverage case where each position in the original genome is covered exactly once by the k-mers.

