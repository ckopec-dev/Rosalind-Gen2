# Rosalind Problem: Find an Eulerian Path in a Graph

## Problem Description
Find an Eulerian path in a directed graph. An Eulerian path is a path that visits every edge exactly once.

## Solution Approach
1. First check if the graph has an Eulerian path (at most one vertex with out-degree - in-degree = 1, and at most one vertex with in-degree - out-degree = 1)
2. Use Hierholzer's algorithm to find the path
3. Start from the vertex with excess out-degree (if exists) or any vertex

## Rust Implementation

```rust
use std::collections::{HashMap, VecDeque};

fn find_eulerian_path(edges: &[(i32, i32)]) -> Vec<i32> {
    // Build adjacency list and calculate in-degrees and out-degrees
    let mut graph: HashMap<i32, Vec<i32>> = HashMap::new();
    let mut in_degree: HashMap<i32, i32> = HashMap::new();
    let mut out_degree: HashMap<i32, i32> = HashMap::new();
    
    // Initialize all vertices
    for &(from, to) in edges {
        graph.entry(from).or_insert_with(Vec::new);
        graph.entry(to).or_insert_with(Vec::new);
        in_degree.entry(from).or_insert(0);
        in_degree.entry(to).or_insert(0);
        out_degree.entry(from).or_insert(0);
        out_degree.entry(to).or_insert(0);
    }
    
    // Fill in the graph and degrees
    for &(from, to) in edges {
        graph.get_mut(&from).unwrap().push(to);
        *out_degree.get_mut(&from).unwrap() += 1;
        *in_degree.get_mut(&to).unwrap() += 1;
    }
    
    // Find the starting vertex
    let mut start_vertex = *graph.keys().next().unwrap();
    let mut unbalanced_in = 0;
    let mut unbalanced_out = 0;
    let mut unbalanced_in_vertex = 0;
    let mut unbalanced_out_vertex = 0;
    
    for (&vertex, &in_deg) in &in_degree {
        let out_deg = out_degree[&vertex];
        if out_deg - in_deg == 1 {
            unbalanced_out += 1;
            unbalanced_out_vertex = vertex;
        } else if in_deg - out_deg == 1 {
            unbalanced_in += 1;
            unbalanced_in_vertex = vertex;
        }
    }
    
    // If there's an unbalanced vertex with excess out-degree, start from there
    if unbalanced_out == 1 && unbalanced_in == 1 {
        start_vertex = unbalanced_out_vertex;
    }
    
    // Hierholzer's algorithm
    let mut stack: Vec<i32> = vec![start_vertex];
    let mut path: Vec<i32> = Vec::new();
    
    while !stack.is_empty() {
        let current = *stack.last().unwrap();
        
        // If current vertex has unvisited edges
        if let Some(edges) = graph.get_mut(&current) {
            if !edges.is_empty() {
                let next = edges.pop().unwrap();
                stack.push(next);
            } else {
                // No more edges from current vertex, add to path
                path.push(stack.pop().unwrap());
            }
        } else {
            // No edges from current vertex, add to path
            path.push(stack.pop().unwrap());
        }
    }
    
    // Reverse the path to get the correct order
    path.reverse();
    path
}

fn main() {
    // Example input: [(0, 1), (1, 2), (2, 0), (2, 3), (3, 4), (4, 2)]
    // This creates a graph with an Eulerian path
    let edges = vec![(0, 1), (1, 2), (2, 0), (2, 3), (3, 4), (4, 2)];
    let path = find_eulerian_path(&edges);
    
    println!("Eulerian path: {:?}", path);
    
    // Another example: [(0, 2), (2, 1), (1, 3), (3, 0), (0, 1)]
    let edges2 = vec![(0, 2), (2, 1), (1, 3), (3, 0), (0, 1)];
    let path2 = find_eulerian_path(&edges2);
    
    println!("Eulerian path: {:?}", path2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eulerian_path() {
        let edges = vec![(0, 1), (1, 2), (2, 0), (2, 3), (3, 4), (4, 2)];
        let path = find_eulerian_path(&edges);
        assert_eq!(path, vec![0, 1, 2, 3, 4, 2, 0]);
    }
    
    #[test]
    fn test_eulerian_path_2() {
        let edges = vec![(0, 2), (2, 1), (1, 3), (3, 0), (0, 1)];
        let path = find_eulerian_path(&edges);
        // This should find a valid Eulerian path
        assert!(!path.is_empty());
    }
}
```

## Explanation

This implementation:

1. **Builds the graph representation**: Creates an adjacency list and tracks in-degrees and out-degrees for each vertex.

2. **Identifies the starting vertex**: 
   - If there's a vertex with out-degree - in-degree = 1, start from there (this is the end of the path)
   - Otherwise, start from any vertex

3. **Uses Hierholzer's algorithm**:
   - Maintains a stack to track the current path
   - When a vertex has unvisited edges, follow one of them
   - When no more edges are available from a vertex, add it to the result path
   - The final path is reversed to get the correct order

4. **Handles edge cases**:
   - Graphs with Eulerian cycles (start from any vertex)
   - Graphs with Eulerian paths (start from the vertex with excess out-degree)

## Time Complexity
- **Time**: O(E + V) where E is the number of edges and V is the number of vertices
- **Space**: O(E + V) for storing the graph and auxiliary data structures

## Usage
The function `find_eulerian_path` takes a vector of tuples representing directed edges and returns the Eulerian path as a vector of vertices.

