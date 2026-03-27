# Rosalind Problem: Dijkstra's Algorithm Solution in Go

## Problem Understanding

Dijkstra's algorithm finds the shortest paths from a source vertex to all other vertices in a weighted graph. Given a weighted directed graph, we need to compute the shortest distances from a source node to all other nodes.

## Solution Approach

1. Use a priority queue (min-heap) to always process the node with minimum distance
2. Initialize distances to infinity except source node (distance 0)
3. For each node, update distances to its neighbors if a shorter path is found
4. Return the shortest distances to all nodes

## Go Implementation

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

// Node represents a vertex in the graph
type Node struct {
    id       int
    distance int
}

// MinHeap implements a min-heap for priority queue
type MinHeap struct {
    nodes []*Node
}

func (h *MinHeap) Push(node *Node) {
    h.nodes = append(h.nodes, node)
    h.heapifyUp(len(h.nodes) - 1)
}

func (h *MinHeap) Pop() *Node {
    if len(h.nodes) == 0 {
        return nil
    }
    
    root := h.nodes[0]
    last := h.nodes[len(h.nodes)-1]
    h.nodes[0] = last
    h.nodes = h.nodes[:len(h.nodes)-1]
    
    if len(h.nodes) > 0 {
        h.heapifyDown(0)
    }
    
    return root
}

func (h *MinHeap) isEmpty() bool {
    return len(h.nodes) == 0
}

func (h *MinHeap) heapifyUp(index int) {
    if index == 0 {
        return
    }
    
    parentIndex := (index - 1) / 2
    if h.nodes[parentIndex].distance > h.nodes[index].distance {
        h.nodes[parentIndex], h.nodes[index] = h.nodes[index], h.nodes[parentIndex]
        h.heapifyUp(parentIndex)
    }
}

func (h *MinHeap) heapifyDown(index int) {
    leftChildIndex := 2*index + 1
    rightChildIndex := 2*index + 2
    smallest := index
    
    if leftChildIndex < len(h.nodes) && 
       h.nodes[leftChildIndex].distance < h.nodes[smallest].distance {
        smallest = leftChildIndex
    }
    
    if rightChildIndex < len(h.nodes) && 
       h.nodes[rightChildIndex].distance < h.nodes[smallest].distance {
        smallest = rightChildIndex
    }
    
    if smallest != index {
        h.nodes[index], h.nodes[smallest] = h.nodes[smallest], h.nodes[index]
        h.heapifyDown(smallest)
    }
}

// Dijkstra's algorithm implementation
func dijkstra(graph map[int][][2]int, start int, numNodes int) []int {
    // Initialize distances
    distances := make([]int, numNodes+1)
    for i := 1; i <= numNodes; i++ {
        distances[i] = -1 // -1 represents infinity
    }
    distances[start] = 0
    
    // Priority queue
    heap := &MinHeap{}
    heap.Push(&Node{id: start, distance: 0})
    
    // Keep track of visited nodes
    visited := make(map[int]bool)
    
    for !heap.isEmpty() {
        current := heap.Pop()
        currentNode := current.id
        
        if visited[currentNode] {
            continue
        }
        visited[currentNode] = true
        
        // Check all neighbors
        for _, edge := range graph[currentNode] {
            neighbor, weight := edge[0], edge[1]
            
            if !visited[neighbor] {
                newDistance := distances[currentNode] + weight
                
                // If we found a shorter path, update it
                if distances[neighbor] == -1 || newDistance < distances[neighbor] {
                    distances[neighbor] = newDistance
                    heap.Push(&Node{id: neighbor, distance: newDistance})
                }
            }
        }
    }
    
    return distances
}

func main() {
    // Read input from stdin
    scanner := bufio.NewScanner(os.Stdin)
    
    // Read the graph description
    scanner.Scan()
    line := scanner.Text()
    parts := strings.Fields(line)
    
    numNodes, _ := strconv.Atoi(parts[0])
    numEdges, _ := strconv.Atoi(parts[1])
    
    // Build adjacency list representation
    graph := make(map[int][][2]int)
    
    // Read edges
    for i := 0; i < numEdges; i++ {
        scanner.Scan()
        line := scanner.Text()
        parts := strings.Fields(line)
        
        from, _ := strconv.Atoi(parts[0])
        to, _ := strconv.Atoi(parts[1])
        weight, _ := strconv.Atoi(parts[2])
        
        graph[from] = append(graph[from], [2]int{to, weight})
    }
    
    // Read start node
    scanner.Scan()
    startNode, _ := strconv.Atoi(scanner.Text())
    
    // Run Dijkstra's algorithm
    distances := dijkstra(graph, startNode, numNodes)
    
    // Output results
    result := make([]string, 0)
    for i := 1; i <= numNodes; i++ {
        if distances[i] == -1 {
            result = append(result, "x") // x for unreachable
        } else {
            result = append(result, strconv.Itoa(distances[i]))
        }
    }
    
    fmt.Println(strings.Join(result, " "))
}
```

## Input Format

The input format for this problem is:
```
n m
u1 v1 w1
u2 v2 w2
...
un vn wn
start_node
```

Where:
- `n` is the number of nodes
- `m` is the number of edges
- Each line after the first contains an edge (u, v, w) where u is the source, v is the destination, and w is the weight
- `start_node` is the node from which to compute shortest paths

## Example

**Input:**
```
6 10
1 2 4
1 3 2
2 3 1
2 4 5
3 4 8
3 5 10
4 5 2
4 6 6
5 6 3
1 6 100
1
```

**Output:**
```
0 4 2 9 11 14
```

## Time and Space Complexity

- **Time Complexity**: O((V + E) log V) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures

## Key Features

1. **Min-heap implementation** for efficient priority queue operations
2. **Graph representation** using adjacency list
3. **Proper handling** of unreachable nodes (represented as "x")
4. **Robust input parsing** for Rosalind-style input format
5. **Memory efficient** approach using maps and slices

This solution correctly implements Dijkstra's algorithm to find shortest paths in weighted directed graphs and handles the specific requirements of the Rosalind problem.

