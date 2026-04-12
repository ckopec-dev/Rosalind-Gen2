# Rosalind Problem: Genome Assembly with Perfect Coverage and Repeats

## Problem Understanding

This problem asks us to reconstruct a genome from a set of reads that have perfect coverage and may contain repeats. The key insight is that we need to find the shortest superstring that contains all given reads as substrings.

## Approach

1. **Build a De Bruijn Graph**: Convert reads into k-mers and create edges between overlapping k-mers
2. **Find Eulerian Path**: Since we have perfect coverage, there should be an Eulerian path in the graph
3. **Reconstruct the genome**: Follow the Eulerian path to get the final sequence

## Solution

```kotlin
import java.io.File
import kotlin.math.max

fun main() {
    // Read input from file
    val input = File("rosalind_gasm.txt").readLines()
    
    // Parse reads
    val reads = mutableListOf<String>()
    for (line in input) {
        if (line.startsWith(">")) {
            continue // Skip FASTA headers
        }
        reads.add(line.trim())
    }
    
    // Find the shortest superstring
    val result = findShortestSuperstring(reads)
    println(result)
}

fun findShortestSuperstring(reads: List<String>): String {
    if (reads.isEmpty()) return ""
    
    // Create overlap graph
    val overlaps = createOverlapGraph(reads)
    
    // Find Eulerian path
    val eulerianPath = findEulerianPath(overlaps)
    
    // Reconstruct the genome
    return reconstructGenome(reads, eulerianPath)
}

fun createOverlapGraph(reads: List<String>): Map<String, List<String>> {
    val graph = mutableMapOf<String, MutableList<String>>()
    
    // For each read, create k-1 length prefixes and suffixes
    for (read in reads) {
        if (!graph.containsKey(read)) {
            graph[read] = mutableListOf()
        }
        
        // Add edges to other reads that overlap
        for (otherRead in reads) {
            if (read == otherRead) continue
            
            // Check if suffix of read matches prefix of otherRead
            val prefix = read.substring(0, read.length - 1)
            val suffix = otherRead.substring(1, otherRead.length)
            
            if (prefix == suffix) {
                graph[read]?.add(otherRead)
            }
        }
    }
    
    return graph
}

fun findEulerianPath(overlaps: Map<String, List<String>>): List<String> {
    // Simple approach: find a path that visits all nodes exactly once
    // This is a simplified version for the perfect coverage case
    
    val visited = mutableSetOf<String>()
    val path = mutableListOf<String>()
    
    // Start with any node
    val startNode = overlaps.keys.first()
    dfs(startNode, overlaps, visited, path)
    
    return path.reversed()
}

fun dfs(node: String, graph: Map<String, List<String>>, visited: MutableSet<String>, path: MutableList<String>) {
    visited.add(node)
    path.add(node)
    
    for (neighbor in graph[node] ?: emptyList()) {
        if (!visited.contains(neighbor)) {
            dfs(neighbor, graph, visited, path)
        }
    }
}

fun reconstructGenome(reads: List<String>, path: List<String>): String {
    if (path.isEmpty()) return ""
    
    val result = StringBuilder()
    
    // Add first read completely
    result.append(path[0])
    
    // Add the remaining parts
    for (i in 1 until path.size) {
        val current = path[i]
        val previous = path[i-1]
        
        // Find overlap length between previous and current
        val overlap = findOverlapLength(previous, current)
        val toAdd = current.substring(overlap)
        result.append(toAdd)
    }
    
    return result.toString()
}

fun findOverlapLength(s1: String, s2: String): Int {
    // Find maximum overlap between s1 (suffix) and s2 (prefix)
    val maxOverlap = minOf(s1.length, s2.length)
    
    for (i in maxOverlap downTo 1) {
        if (s1.substring(s1.length - i) == s2.substring(0, i)) {
            return i
        }
    }
    
    return 0
}

// Alternative approach using greedy method for perfect coverage
fun findShortestSuperstringGreedy(reads: List<String>): String {
    if (reads.isEmpty()) return ""
    
    val remaining = mutableListOf<String>().apply { addAll(reads) }
    val result = StringBuilder()
    
    // Start with the first read
    result.append(remaining.removeAt(0))
    
    // Greedily add the remaining reads
    while (remaining.isNotEmpty()) {
        var bestRead = ""
        var bestOverlap = -1
        var bestIndex = -1
        
        // Find the read that gives maximum overlap
        for (i in remaining.indices) {
            val read = remaining[i]
            val overlap = findMaxOverlap(result.toString(), read)
            
            if (overlap > bestOverlap) {
                bestOverlap = overlap
                bestRead = read
                bestIndex = i
            }
        }
        
        if (bestIndex >= 0) {
            result.append(bestRead.substring(bestOverlap))
            remaining.removeAt(bestIndex)
        } else {
            // No overlap found, append the entire read
            result.append(remaining.removeAt(0))
        }
    }
    
    return result.toString()
}

fun findMaxOverlap(s1: String, s2: String): Int {
    var maxOverlap = 0
    
    // Try all possible overlaps
    val maxLen = minOf(s1.length, s2.length)
    
    for (i in maxLen downTo 1) {
        if (s1.substring(s1.length - i) == s2.substring(0, i)) {
            maxOverlap = i
            break
        }
    }
    
    return maxOverlap
}
```

## Key Concepts

1. **De Bruijn Graph**: Each read is represented as a node, and edges represent overlaps between suffixes and prefixes.

2. **Eulerian Path**: In a De Bruijn graph of perfect coverage, there exists an Eulerian path that visits each edge exactly once.

3. **Superstring Reconstruction**: The path through the Eulerian circuit gives us the shortest superstring containing all reads.

## Time Complexity
- Building the graph: O(n² × k) where n is the number of reads and k is the read length
- Finding Eulerian path: O(n + m) where m is the number of edges
- Overall: O(n² × k)

## Space Complexity
O(n × k) for storing the graph and results

This solution handles the case where we have perfect coverage (every position in the original genome is covered by at least one read) and accounts for potential repeats in the genome.

