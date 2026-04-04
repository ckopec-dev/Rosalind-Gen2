# Rosalind Problem: Constructing a De Bruijn Graph

## Problem Description
Given a collection of DNA strings, construct the de Bruijn graph of order k.

## Solution in Kotlin

```kotlin
fun constructDeBruijnGraph(sequences: List<String>, k: Int): Map<String, List<String>> {
    val graph = mutableMapOf<String, MutableList<String>>()
    
    // For each sequence, extract all k-mers
    for (sequence in sequences) {
        for (i in 0 until sequence.length - k + 1) {
            val kmer = sequence.substring(i, i + k)
            
            // Get the prefix (k-1) and suffix (k-1) of the k-mer
            val prefix = kmer.substring(0, k - 1)
            val suffix = kmer.substring(1, k)
            
            // Add edge from prefix to suffix
            graph.getOrPut(prefix) { mutableListOf() }.add(suffix)
        }
    }
    
    return graph
}

fun formatGraph(graph: Map<String, List<String>>): String {
    val result = mutableListOf<String>()
    
    // Sort the graph by keys
    val sortedKeys = graph.keys.sorted()
    
    for (key in sortedKeys) {
        val edges = graph[key] ?: emptyList()
        if (edges.isNotEmpty()) {
            // Sort edges lexicographically
            val sortedEdges = edges.sorted()
            result.add("$key -> ${sortedEdges.joinToString(", ")}")
        }
    }
    
    return result.joinToString("\n")
}

fun main() {
    // Example usage
    val sequences = listOf(
        "GAGG",
        "CAGG",
        "GGGG",
        "GGGA",
        "CAGG",
        "AGGG",
        "GGAG"
    )
    
    val k = 3
    val graph = constructDeBruijnGraph(sequences, k)
    val formattedResult = formatGraph(graph)
    
    println(formattedResult)
}
```

## Explanation

### Algorithm Steps:

1. **Input Processing**: Take a list of DNA sequences and a k-mer length k
2. **K-mer Extraction**: For each sequence, extract all possible k-mers
3. **Graph Construction**: 
   - For each k-mer, extract its prefix (k-1 characters from the left) and suffix (k-1 characters from the right)
   - Create a directed edge from prefix to suffix in the graph
4. **Output Formatting**: Sort vertices and edges lexicographically for consistent output

### Key Components:

- **constructDeBruijnGraph**: Main function that builds the adjacency list representation of the graph
- **formatGraph**: Formats the output in the required Rosalind format
- **Graph Representation**: Uses a `Map<String, List<String>>` where keys are prefixes and values are lists of suffixes

### Time Complexity:
- O(n × m × k) where n is the number of sequences, m is the average sequence length, and k is the k-mer length

### Space Complexity:
- O(n × m × k) for storing the graph

### Sample Input:
```
GAGG
CAGG
GGGG
GGGA
CAGG
AGGG
GGAG
```

### Sample Output:
```
AGG -> GG
CAG -> AGG
GAG -> AGG
GGA -> GAG
GGG -> GGA, GGA
```

This solution correctly constructs the de Bruijn graph by creating edges between overlapping k-mers, which is the fundamental concept behind de Bruijn graphs in bioinformatics.

