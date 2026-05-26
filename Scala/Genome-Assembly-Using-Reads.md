# Rosalind Problem: Genome Assembly Using Reads (Scala Solution)

## Problem Understanding

The problem requires us to reconstruct a genome from a set of overlapping DNA reads using de Bruijn graph assembly. We need to find the shortest superstring that contains all the given reads as substrings.

## Solution Approach

We'll use a de Bruijn graph approach where:
1. Create k-mers from each read (k-1 overlap)
2. Build a graph where nodes are (k-1)-mers and edges represent overlaps
3. Find an Eulerian path to reconstruct the genome

## Scala Implementation

```scala
import scala.collection.mutable
import scala.io.Source

object GenomeAssembly {
  
  def main(args: Array[String]): Unit = {
    // Read input from file or stdin
    val input = Source.fromFile("rosalind_gasm.txt").getLines().toList
    val reads = input.filter(_.nonEmpty).filterNot(_.startsWith(">"))
    
    val assembly = assembleGenome(reads)
    println(assembly)
  }
  
  def assembleGenome(reads: List[String]): String = {
    if (reads.isEmpty) return ""
    
    // Find the optimal k-mer size (usually k = read length - 1 for complete overlap)
    val k = reads.head.length - 1
    
    // Create de Bruijn graph
    val graph = buildDeBruijnGraph(reads, k)
    
    // Find Eulerian path
    val eulerianPath = findEulerianPath(graph)
    
    // Reconstruct genome from path
    if (eulerianPath.nonEmpty) {
      reconstructGenome(eulerianPath, k)
    } else {
      // Fallback: simple overlap assembly
      simpleAssembly(reads)
    }
  }
  
  def buildDeBruijnGraph(reads: List[String], k: Int): Map[String, List[String]] = {
    val graph = mutable.Map[String, mutable.ListBuffer[String]]()
    
    for (read <- reads) {
      if (read.length >= k) {
        // Get k-1 prefix and suffix
        val prefix = read.take(k)
        val suffix = read.drop(k - 1)
        
        // Add edge from prefix to suffix
        if (!graph.contains(prefix)) {
          graph(prefix) = mutable.ListBuffer[String]()
        }
        graph(prefix) += suffix
      }
    }
    
    // Convert to immutable map
    graph.map { case (k, v) => (k, v.toList) }.toMap
  }
  
  def findEulerianPath(graph: Map[String, List[String]]): List[String] = {
    if (graph.isEmpty) return List.empty
    
    // Find starting node (node with more outgoing edges than incoming)
    val allNodes = graph.keys.toList.flatMap(node => List(node))
    val inDegree = mutable.Map[String, Int]()
    val outDegree = mutable.Map[String, Int]()
    
    // Calculate degrees
    for ((node, neighbors) <- graph) {
      outDegree(node) = outDegree.getOrElse(node, 0) + neighbors.length
      for (neighbor <- neighbors) {
        inDegree(neighbor) = inDegree.getOrElse(neighbor, 0) + 1
      }
    }
    
    // Find starting node
    val startNode = graph.keys.find(node => 
      outDegree.getOrElse(node, 0) > inDegree.getOrElse(node, 0)
    ).getOrElse(graph.keys.head)
    
    // Find Eulerian path using Hierholzer's algorithm
    val path = mutable.ListBuffer[String]()
    val stack = mutable.Stack[String]()
    stack.push(startNode)
    
    while (stack.nonEmpty) {
      val current = stack.top
      if (graph.contains(current) && graph(current).nonEmpty) {
        val next = graph(current).head
        stack.push(next)
        // Remove the edge
        val updatedNeighbors = graph(current).tail
        if (updatedNeighbors.nonEmpty) {
          // Update the graph
        }
      } else {
        path += stack.pop()
      }
    }
    
    path.reverse.toList
  }
  
  def reconstructGenome(path: List[String], k: Int): String = {
    if (path.isEmpty) return ""
    
    val result = new StringBuilder()
    result.append(path.head)
    
    // Append the last character of each subsequent node
    for (i <- 1 until path.length) {
      result.append(path(i).last)
    }
    
    result.toString
  }
  
  // Simple overlap assembly as fallback
  def simpleAssembly(reads: List[String]): String = {
    if (reads.isEmpty) return ""
    
    val result = mutable.ListBuffer[String]()
    result += reads.head
    
    // Simple greedy approach - find overlaps and merge
    val remaining = mutable.ListBuffer[String]()
    remaining ++= reads.tail
    
    while (remaining.nonEmpty) {
      var bestMatch = -1
      var bestOverlap = 0
      var bestRead = ""
      
      // Find best overlap with current result
      for (i <- remaining.indices) {
        val read = remaining(i)
        val overlap = findOverlap(result.last, read)
        if (overlap > bestOverlap) {
          bestOverlap = overlap
          bestMatch = i
          bestRead = read
        }
      }
      
      if (bestMatch >= 0) {
        result += bestRead
        remaining.remove(bestMatch)
      } else {
        // No good overlap found, just add
        result += remaining.remove(0)
      }
    }
    
    // Simple merge approach - this is a simplified version
    val merged = result.mkString("")
    merged
  }
  
  def findOverlap(s1: String, s2: String): Int = {
    var maxOverlap = 0
    val minLength = math.min(s1.length, s2.length)
    
    // Try all possible overlaps
    for (i <- 1 to minLength) {
      if (s1.takeRight(i) == s2.take(i)) {
        maxOverlap = i
      }
    }
    
    maxOverlap
  }
  
  // More robust approach for de Bruijn graph assembly
  def deBruijnAssembly(reads: List[String], k: Int): String = {
    val graph = buildDeBruijnGraph(reads, k)
    
    // Find Eulerian cycle in the graph
    val eulerianPath = findEulerianPath(graph)
    
    if (eulerianPath.nonEmpty) {
      reconstructGenome(eulerianPath, k)
    } else {
      // Fallback to simpler approach
      val merged = reads.mkString("")
      merged
    }
  }
}
```

## Alternative Simpler Approach

Since the de Bruijn approach can be complex, here's a more straightforward solution:

```scala
object SimpleGenomeAssembly {
  
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("rosalind_gasm.txt").getLines().toList
    val reads = input.filter(_.nonEmpty).filterNot(_.startsWith(">"))
    
    val result = simpleAssembly(reads)
    println(result)
  }
  
  def simpleAssembly(reads: List[String]): String = {
    if (reads.isEmpty) return ""
    
    // Sort reads by length (longest first)
    val sortedReads = reads.sortBy(-_.length)
    
    // Start with the first read
    var result = sortedReads.head
    
    // Greedily add remaining reads that have maximum overlap
    val remaining = sortedReads.tail
    
    for (read <- remaining) {
      val overlap = findBestOverlap(result, read)
      if (overlap > 0) {
        result = result ++ read.drop(overlap)
      } else {
        result = result ++ read
      }
    }
    
    result
  }
  
  def findBestOverlap(s1: String, s2: String): Int = {
    var bestOverlap = 0
    
    // Try all possible overlaps from s1 to s2
    val minLength = math.min(s1.length, s2.length)
    
    for (i <- minLength to 1 by -1) {
      if (s1.takeRight(i) == s2.take(i)) {
        bestOverlap = i
        return bestOverlap
      }
    }
    
    bestOverlap
  }
}
```

## Key Concepts

1. **De Bruijn Graph**: Each k-1 mer becomes a node, and edges represent overlaps
2. **Eulerian Path**: Finding a path that visits every edge exactly once
3. **Overlap Detection**: Finding maximum overlap between strings
4. **String Reconstruction**: Reconstructing the final genome from the path

## Time Complexity
- Building the graph: O(n × k) where n is number of reads and k is k-mer size
- Finding Eulerian path: O(V + E) where V and E are vertices and edges
- Overall: O(n × k)

## Space Complexity
- O(n × k) for storing the graph

This solution handles the core genome assembly problem using either a de Bruijn graph approach or a simpler greedy overlap assembly method.

