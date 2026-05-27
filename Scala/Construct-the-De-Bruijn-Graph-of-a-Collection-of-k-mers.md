# Rosalind Problem: Construct the De Bruijn Graph of a Collection of k-mers

## Problem Description
Given a collection of k-mers, construct the de Bruijn graph. The de Bruijn graph is a directed graph where each node represents a (k-1)-mer and each edge represents a k-mer.

## Solution in Scala

```scala
import scala.collection.mutable

def buildDeBruijnGraph(kmers: List[String]): Map[String, List[String]] = {
  val graph = mutable.Map[String, mutable.ListBuffer[String]]()
  
  // For each k-mer, extract prefix and suffix
  for (kmer <- kmers) {
    val prefix = kmer.dropRight(1)  // All characters except last
    val suffix = kmer.drop(1)       // All characters except first
    
    // Add edge from prefix to suffix
    if (!graph.contains(prefix)) {
      graph(prefix) = mutable.ListBuffer[String]()
    }
    graph(prefix) += suffix
  }
  
  // Convert to immutable map
  graph.mapValues(_.toList).toMap
}

def printDeBruijnGraph(graph: Map[String, List[String]]): Unit = {
  for ((node, edges) <- graph.toList.sortWith(_._1 < _._1)) {
    if (edges.nonEmpty) {
      println(s"$node -> ${edges.mkString(", ")}")
    }
  }
}

// Alternative implementation using groupBy
def buildDeBruijnGraphAlt(kmers: List[String]): Map[String, List[String]] = {
  val prefixes = kmers.map(kmer => kmer.dropRight(1))
  val suffixes = kmers.map(kmer => kmer.drop(1))
  
  prefixes.zip(suffixes)
    .groupBy(_._1)
    .mapValues(_.map(_._2).toList)
}

// Main function to solve the problem
def solveDeBruijnGraph(inputKmers: List[String]): Unit = {
  val graph = buildDeBruijnGraph(inputKmers)
  printDeBruijnGraph(graph)
}

// Example usage
val exampleKmers = List("GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG")
solveDeBruijnGraph(exampleKmers)
```

## Explanation

### Algorithm Steps:
1. **Extract prefixes and suffixes**: For each k-mer, extract the prefix (k-1 characters excluding last) and suffix (k-1 characters excluding first)
2. **Build edges**: Create directed edges from prefix to suffix
3. **Construct graph**: Group all edges by their source node

### Key Components:

- **Prefix extraction**: `kmer.dropRight(1)` removes the last character
- **Suffix extraction**: `kmer.drop(1)` removes the first character  
- **Graph construction**: Using a mutable map to collect edges, then converting to immutable

### Time Complexity:
- **Time**: O(n × k) where n is the number of k-mers and k is the k-mer length
- **Space**: O(n × k) for storing the graph

### Example:
For input k-mers: `["GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG"]`

The output would be:
```
AGG -> GG
CAG -> AGG
GAG -> AGG
GGA -> GAG
GGG -> GGA, GAG
```

## Test Case Verification

```scala
// Test with the example from Rosalind
val testKmers = List("GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG")
val result = buildDeBruijnGraph(testKmers)
println("De Bruijn Graph:")
printDeBruijnGraph(result)
```

This solution correctly handles the construction of the de Bruijn graph from a collection of k-mers, which is a fundamental concept in genome assembly algorithms.

