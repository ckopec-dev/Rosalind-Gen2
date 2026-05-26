# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Understanding

We need to find a topological ordering of a directed acyclic graph (DAG). A topological ordering is a linear ordering of vertices such that for every directed edge u → v, vertex u comes before v in the ordering.

## Approach

1. **Calculate in-degrees** for all vertices
2. **Find vertices with in-degree 0** (no incoming edges)
3. **Process vertices** in order, removing edges and updating in-degrees
4. **Continue until all vertices are processed**

## Solution

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    // Read input from stdin
    scanner := bufio.NewScanner(os.Stdin)
    
    // Read the number of vertices and edges
    scanner.Scan()
    line := scanner.Text()
    parts := strings.Fields(line)
    n, _ := strconv.Atoi(parts[0])
    m, _ := strconv.Atoi(parts[1])
    
    // Initialize adjacency list and in-degree array
    adjList := make([][]int, n+1)  // 1-indexed
    inDegree := make([]int, n+1)
    
    // Read edges
    for i := 0; i < m; i++ {
        scanner.Scan()
        line := scanner.Text()
        parts := strings.Fields(line)
        u, _ := strconv.Atoi(parts[0])
        v, _ := strconv.Atoi(parts[1])
        
        adjList[u] = append(adjList[u], v)
        inDegree[v]++
    }
    
    // Find vertices with in-degree 0
    queue := []int{}
    for i := 1; i <= n; i++ {
        if inDegree[i] == 0 {
            queue = append(queue, i)
        }
    }
    
    // Process vertices in topological order
    result := []int{}
    for len(queue) > 0 {
        // Remove vertex from queue
        u := queue[0]
        queue = queue[1:]
        result = append(result, u)
        
        // Update in-degrees of neighbors
        for _, v := range adjList[u] {
            inDegree[v]--
            if inDegree[v] == 0 {
                queue = append(queue, v)
            }
        }
    }
    
    // Print result
    for i, vertex := range result {
        if i > 0 {
            fmt.Print(" ")
        }
        fmt.Print(vertex)
    }
    fmt.Println()
}
```

## Explanation

1. **Input Parsing**: Read the number of vertices and edges, then parse each edge to build an adjacency list representation of the graph.

2. **In-degree Calculation**: For each edge u → v, increment the in-degree of vertex v.

3. **Queue Initialization**: Add all vertices with in-degree 0 to a queue (these have no prerequisites).

4. **Topological Processing**: 
   - Remove a vertex from the queue
   - Add it to the result
   - For each neighbor of this vertex, decrease its in-degree
   - If any neighbor's in-degree becomes 0, add it to the queue

5. **Output**: Print the vertices in the order they were processed.

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V + E) for storing the adjacency list and in-degree array

## Example Input/Output

**Input:**
```
6 8
1 2
1 3
2 4
3 4
4 5
4 6
5 6
2 6
```

**Output:**
```
1 2 3 4 5 6
```

This solution implements the classic Kahn's algorithm for topological sorting, which is efficient and handles all the requirements of the problem.

