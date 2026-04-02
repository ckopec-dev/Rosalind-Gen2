# Rosalind Problem: Find an Eulerian Path in a Graph

## Problem Description
Find an Eulerian path in a directed graph. An Eulerian path is a path that visits every edge exactly once.

## Solution in Go

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    // Read input from file or stdin
    var input string
    if len(os.Args) > 1 {
        // Read from file
        data, err := os.ReadFile(os.Args[1])
        if err != nil {
            panic(err)
        }
        input = string(data)
    } else {
        // Read from stdin
        var line string
        fmt.Scanln(&line)
        input = line
    }
    
    // Parse the graph
    graph := parseGraph(input)
    
    // Find Eulerian path
    path := findEulerianPath(graph)
    
    // Output the path
    fmt.Println(strings.Join(path, " "))
}

func parseGraph(input string) map[int][]int {
    graph := make(map[int][]int)
    
    lines := strings.Split(strings.TrimSpace(input), "\n")
    
    for _, line := range lines {
        if line == "" {
            continue
        }
        
        parts := strings.Fields(line)
        if len(parts) < 2 {
            continue
        }
        
        from, _ := strconv.Atoi(parts[0])
        to, _ := strconv.Atoi(parts[1])
        
        graph[from] = append(graph[from], to)
    }
    
    return graph
}

func findEulerianPath(graph map[int][]int) []string {
    // Find the starting node (node with out-degree - in-degree = 1)
    inDegree := make(map[int]int)
    outDegree := make(map[int]int)
    
    // Calculate in-degrees and out-degrees
    for node := range graph {
        outDegree[node] = len(graph[node])
        for _, neighbor := range graph[node] {
            inDegree[neighbor]++
        }
    }
    
    // Find starting node
    startNode := -1
    for node := range graph {
        if outDegree[node]-inDegree[node] == 1 {
            startNode = node
            break
        }
    }
    
    // If no node has out-degree - in-degree = 1, start from any node
    if startNode == -1 {
        for node := range graph {
            if outDegree[node] > 0 {
                startNode = node
                break
            }
        }
    }
    
    // Find Eulerian path using Hierholzer's algorithm
    path := []int{}
    stack := []int{startNode}
    
    for len(stack) > 0 {
        current := stack[len(stack)-1]
        
        if len(graph[current]) > 0 {
            // Get next node
            next := graph[current][0]
            // Remove the edge
            graph[current] = graph[current][1:]
            // Add to stack
            stack = append(stack, next)
        } else {
            // Backtrack
            path = append([]int{stack[len(stack)-1]}, path...)
            stack = stack[:len(stack)-1]
        }
    }
    
    // Convert path to strings
    result := make([]string, len(path))
    for i, node := range path {
        result[i] = strconv.Itoa(node)
    }
    
    return result
}
```

## Explanation

This solution implements an algorithm to find an Eulerian path in a directed graph:

1. **Input parsing**: Reads the graph from input, where each line represents an edge from one node to another.

2. **Degree calculation**: Calculates in-degrees and out-degrees for all nodes.

3. **Finding starting node**: 
   - The starting node is the one where out-degree - in-degree = 1
   - If no such node exists, start from any node with outgoing edges

4. **Eulerian path finding**: Uses Hierholzer's algorithm:
   - Start with the identified starting node
   - Follow edges until reaching a node with no outgoing edges
   - Backtrack and continue until all edges are visited
   - The path is built in reverse order during backtracking

## Example Usage

Input:
```
0 1
1 2
2 0
```

Output:
```
0 1 2 0
```

## Time Complexity
- O(E) where E is the number of edges

## Space Complexity
- O(V + E) where V is the number of vertices and E is the number of edges

