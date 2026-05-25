# Rosalind Problem: Testing Acyclicity - Rust Solution

## Problem Understanding

The problem asks us to determine whether a given directed graph is acyclic (i.e., contains no cycles). This is a classic graph theory problem that can be solved using topological sorting or cycle detection algorithms.

## Approach

I'll use a **topological sorting approach** with Kahn's algorithm:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. Process vertices from the queue, reducing in-degrees of neighbors
4. If we process all vertices, the graph is acyclic (returns 0)
5. If we can't process all vertices, there's a cycle (returns 1)

## Solution

```rust
use std::collections::VecDeque;
use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    
    // Read number of vertices and edges
    let first_line = lines.next().unwrap().unwrap();
    let mut parts = first_line.split_whitespace();
    let n: usize = parts.next().unwrap().parse().unwrap();
    let m: usize = parts.next().unwrap().parse().unwrap();
    
    // Build adjacency list and calculate in-degrees
    let mut adj_list: Vec<Vec<usize>> = vec![Vec::new(); n + 1]; // 1-indexed
    let mut in_degree = vec![0; n + 1];
    
    // Read edges
    for _ in 0..m {
        let line = lines.next().unwrap().unwrap();
        let mut parts = line.split_whitespace();
        let u: usize = parts.next().unwrap().parse().unwrap();
        let v: usize = parts.next().unwrap().parse().unwrap();
        
        adj_list[u].push(v);
        in_degree[v] += 1;
    }
    
    // Apply Kahn's algorithm
    let result = is_acyclic(n, &adj_list, &in_degree);
    println!("{}", result);
}

fn is_acyclic(n: usize, adj_list: &[Vec<usize>], in_degree: &[usize]) -> i32 {
    let mut queue = VecDeque::new();
    let mut visited_count = 0;
    
    // Add all vertices with in-degree 0 to queue
    for i in 1..=n {
        if in_degree[i] == 0 {
            queue.push_back(i);
        }
    }
    
    // Process vertices
    while let Some(current) = queue.pop_front() {
        visited_count += 1;
        
        // Reduce in-degree of all neighbors
        for &neighbor in &adj_list[current] {
            in_degree[neighbor] -= 1;
            if in_degree[neighbor] == 0 {
                queue.push_back(neighbor);
            }
        }
    }
    
    // If we visited all vertices, graph is acyclic (return 0)
    // Otherwise, there's a cycle (return 1)
    if visited_count == n { 0 } else { 1 }
}
```

## Explanation

1. **Input Parsing**: Read the number of vertices and edges, then parse each edge to build an adjacency list representation of the graph.

2. **In-degree Calculation**: For each edge (u,v), increment the in-degree of vertex v since it has an incoming edge.

3. **Kahn's Algorithm**:
   - Initialize a queue with all vertices having in-degree 0
   - Process vertices one by one
   - For each processed vertex, reduce the in-degrees of its neighbors
   - If any neighbor's in-degree becomes 0, add it to the queue
   - Count total visited vertices

4. **Cycle Detection**: If we can visit all vertices, the graph is acyclic (return 0). If not, there's a cycle (return 1).

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V + E) for adjacency list and in-degree array

## Sample Input/Output

**Input:**
```
5 7
1 2
2 3
3 1
3 4
4 5
5 3
5 4
```

**Output:**
```
1
```

This indicates the graph contains a cycle (return 1 for cyclic).

**Input:**
```
4 4
1 2
2 3
3 4
4 1
```

**Output:**
```
1
```

**Input:**
```
3 2
1 2
2 3
```

**Output:**
```
0
```

The second example shows a simple directed path with no cycles, returning 0.

