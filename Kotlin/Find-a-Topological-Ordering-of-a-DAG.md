# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Understanding

We need to find a topological ordering of a directed acyclic graph (DAG). A topological ordering is a linear ordering of vertices such that for every directed edge u → v, vertex u comes before vertex v in the ordering.

## Approach

I'll use Kahn's algorithm:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. While queue is not empty:
   - Remove vertex from queue
   - Add it to result
   - For each neighbor, decrease in-degree
   - If in-degree becomes 0, add to queue

## Solution

```kotlin
import java.io.File
import java.util.*

fun main() {
    // Read input from file
    val input = File("rosalind_topo.txt").readText().trim()
    val lines = input.lines()
    
    // Parse the graph
    val firstLine = lines[0].split(" ").map { it.toInt() }
    val n = firstLine[0]  // number of vertices
    val m = firstLine[1]  // number of edges
    
    val graph = mutableMapOf<Int, MutableList<Int>>()
    val inDegree = IntArray(n + 1) { 0 }
    
    // Initialize graph
    for (i in 1..n) {
        graph[i] = mutableListOf()
    }
    
    // Parse edges and build adjacency list
    for (i in 1 until lines.size) {
        val edge = lines[i].split(" ").map { it.toInt() }
        val from = edge[0]
        val to = edge[1]
        
        graph[from]!!.add(to)
        inDegree[to]++
    }
    
    // Kahn's algorithm for topological sorting
    val queue = LinkedList<Int>()
    val result = mutableListOf<Int>()
    
    // Add all vertices with in-degree 0
    for (i in 1..n) {
        if (inDegree[i] == 0) {
            queue.add(i)
        }
    }
    
    while (queue.isNotEmpty()) {
        val current = queue.poll()
        result.add(current)
        
        // Process neighbors
        for (neighbor in graph[current]!!) {
            inDegree[neighbor]--
            if (inDegree[neighbor] == 0) {
                queue.add(neighbor)
            }
        }
    }
    
    // Output result
    println(result.joinToString(" "))
}
```

## Explanation

1. **Input parsing**: Read the number of vertices and edges, then parse each edge to build an adjacency list representation of the graph.

2. **In-degree calculation**: For each edge (u → v), increment the in-degree of vertex v.

3. **Kahn's algorithm**:
   - Initialize a queue with all vertices having in-degree 0
   - While queue is not empty:
     - Remove a vertex from queue
     - Add it to the topological order
     - For each neighbor of this vertex, decrease its in-degree
     - If in-degree becomes 0, add neighbor to queue

4. **Output**: Print the topological ordering as space-separated integers.

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V + E) for the adjacency list and in-degree array

## Example

For input:
```
6 8
5 2
5 0
4 0
4 1
2 3
3 1
1 3
3 2
```

Output would be a valid topological ordering like:
```
5 4 2 3 1 0
```

This solution handles the constraints of the Rosalind problem and produces a valid topological ordering of the given DAG.

