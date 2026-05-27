# Rosalind Problem: Strongly Connected Components

## Problem Understanding

In graph theory, a **strongly connected component (SCC)** of a directed graph is a subgraph where every vertex is reachable from every other vertex in that subgraph. The goal is to find all strongly connected components in a directed graph.

## Approach

I'll use **Kosaraju's algorithm** which involves:
1. Finding the transpose graph
2. Performing DFS on the original graph to get finishing times
3. Performing DFS on the transpose graph in reverse order of finishing times

## Solution

```kotlin
import java.io.File
import java.util.*

fun main() {
    // Read input from file
    val input = File("rosalind_scc.txt").readText()
    val lines = input.trim().split("\n")
    
    // Parse the graph
    val graph = parseGraph(lines)
    
    // Find strongly connected components
    val sccs = findStronglyConnectedComponents(graph)
    
    // Output the result
    println(sccs.size)
}

fun parseGraph(lines: List<String>): Map<Int, List<Int>> {
    val graph = mutableMapOf<Int, MutableList<Int>>()
    
    // Skip the first line (contains n and m)
    for (i in 1 until lines.size) {
        val edge = lines[i].trim().split(" ").map { it.toInt() }
        val from = edge[0]
        val to = edge[1]
        
        graph.computeIfAbsent(from) { mutableListOf() }.add(to)
        graph.computeIfAbsent(to) { mutableListOf() }.add(from) // For undirected graph
    }
    
    return graph
}

fun findStronglyConnectedComponents(graph: Map<Int, List<Int>>): List<List<Int>> {
    val vertices = graph.keys.toMutableList()
    val visited = mutableSetOf<Int>()
    val finishingTimes = mutableListOf<Int>()
    
    // First DFS to get finishing times
    for (vertex in vertices) {
        if (vertex !in visited) {
            dfsFinish(vertex, graph, visited, finishingTimes)
        }
    }
    
    // Create transpose graph
    val transpose = createTransposeGraph(graph)
    
    // Reset visited set
    visited.clear()
    
    // Second DFS on transpose graph in reverse finishing time order
    val sccs = mutableListOf<List<Int>>()
    for (i in finishingTimes.size - 1 downTo 0) {
        val vertex = finishingTimes[i]
        if (vertex !in visited) {
            val component = mutableListOf<Int>()
            dfsComponent(vertex, transpose, visited, component)
            sccs.add(component)
        }
    }
    
    return sccs
}

fun dfsFinish(vertex: Int, graph: Map<Int, List<Int>>, visited: MutableSet<Int>, finishingTimes: MutableList<Int>) {
    visited.add(vertex)
    
    val neighbors = graph[vertex] ?: emptyList()
    for (neighbor in neighbors) {
        if (neighbor !in visited) {
            dfsFinish(neighbor, graph, visited, finishingTimes)
        }
    }
    
    finishingTimes.add(vertex)
}

fun createTransposeGraph(graph: Map<Int, List<Int>>): Map<Int, List<Int>> {
    val transpose = mutableMapOf<Int, MutableList<Int>>()
    
    for ((vertex, neighbors) in graph) {
        for (neighbor in neighbors) {
            transpose.computeIfAbsent(neighbor) { mutableListOf() }.add(vertex)
        }
    }
    
    return transpose
}

fun dfsComponent(vertex: Int, graph: Map<Int, List<Int>>, visited: MutableSet<Int>, component: MutableList<Int>) {
    visited.add(vertex)
    component.add(vertex)
    
    val neighbors = graph[vertex] ?: emptyList()
    for (neighbor in neighbors) {
        if (neighbor !in visited) {
            dfsComponent(neighbor, graph, visited, component)
        }
    }
}
```

## Alternative Implementation (More Direct for Directed Graphs)

Since this is a directed graph problem, here's a more direct implementation:

```kotlin
import java.io.File
import java.util.*

fun main() {
    val input = File("rosalind_scc.txt").readText()
    val lines = input.trim().split("\n")
    
    val graph = parseDirectedGraph(lines)
    val sccs = findStronglyConnectedComponents(graph)
    
    println(sccs.size)
}

fun parseDirectedGraph(lines: List<String>): Map<Int, List<Int>> {
    val graph = mutableMapOf<Int, MutableList<Int>>()
    
    // Skip first line - it contains n and m
    for (i in 1 until lines.size) {
        val edge = lines[i].trim().split(" ").map { it.toInt() }
        val from = edge[0]
        val to = edge[1]
        
        graph.computeIfAbsent(from) { mutableListOf() }.add(to)
    }
    
    return graph
}

fun findStronglyConnectedComponents(graph: Map<Int, List<Int>>): List<List<Int>> {
    val vertices = graph.keys.toMutableList()
    val visited = mutableSetOf<Int>()
    val finishingTimes = mutableListOf<Int>()
    
    // First DFS to get finishing times
    for (vertex in vertices) {
        if (vertex !in visited) {
            dfsFinish(vertex, graph, visited, finishingTimes)
        }
    }
    
    // Create transpose graph
    val transpose = createTransposeGraph(graph)
    
    // Reset visited set
    visited.clear()
    
    // Second DFS on transpose graph in reverse finishing time order
    val sccs = mutableListOf<List<Int>>()
    for (i in finishingTimes.size - 1 downTo 0) {
        val vertex = finishingTimes[i]
        if (vertex !in visited) {
            val component = mutableListOf<Int>()
            dfsComponent(vertex, transpose, visited, component)
            sccs.add(component)
        }
    }
    
    return sccs
}

fun dfsFinish(vertex: Int, graph: Map<Int, List<Int>>, visited: MutableSet<Int>, finishingTimes: MutableList<Int>) {
    visited.add(vertex)
    
    val neighbors = graph[vertex] ?: emptyList()
    for (neighbor in neighbors) {
        if (neighbor !in visited) {
            dfsFinish(neighbor, graph, visited, finishingTimes)
        }
    }
    
    finishingTimes.add(vertex)
}

fun createTransposeGraph(graph: Map<Int, List<Int>>): Map<Int, List<Int>> {
    val transpose = mutableMapOf<Int, MutableList<Int>>()
    
    for ((vertex, neighbors) in graph) {
        for (neighbor in neighbors) {
            transpose.computeIfAbsent(neighbor) { mutableListOf() }.add(vertex)
        }
    }
    
    return transpose
}

fun dfsComponent(vertex: Int, graph: Map<Int, List<Int>>, visited: MutableSet<Int>, component: MutableList<Int>) {
    visited.add(vertex)
    component.add(vertex)
    
    val neighbors = graph[vertex] ?: emptyList()
    for (neighbor in neighbors) {
        if (neighbor !in visited) {
            dfsComponent(neighbor, graph, visited, component)
        }
    }
}
```

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures

## Key Points

1. **Kosaraju's Algorithm** is used for finding strongly connected components
2. The algorithm requires two DFS traversals
3. The transpose graph is created to reverse all edges
4. Components are found by processing vertices in reverse order of finishing times
5. The result is the count of strongly connected components, which is what Rosalind expects

This solution handles the directed graph structure properly and correctly identifies all strongly connected components in the input graph.

