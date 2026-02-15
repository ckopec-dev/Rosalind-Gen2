# Rosalind Problem: Degree Array

## Problem Description
Given an undirected graph with n nodes and m edges, return the degree of each node.

## Solution in Rust

```rust
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    
    // Read the number of nodes and edges
    let first_line = lines.next().unwrap().unwrap();
    let mut parts = first_line.split_whitespace();
    let n: usize = parts.next().unwrap().parse().unwrap();
    let _m: usize = parts.next().unwrap().parse().unwrap();
    
    // Initialize degree array
    let mut degrees = vec![0; n + 1]; // 1-indexed
    
    // Read edges and count degrees
    for line in lines {
        let edge_line = line.unwrap();
        let mut edge_parts = edge_line.split_whitespace();
        let u: usize = edge_parts.next().unwrap().parse().unwrap();
        let v: usize = edge_parts.next().unwrap().parse().unwrap();
        
        degrees[u] += 1;
        degrees[v] += 1;
    }
    
    // Print degrees (excluding index 0)
    let result = (1..=n)
        .map(|i| degrees[i].to_string())
        .collect::<Vec<String>>()
        .join(" ");
    
    println!("{}", result);
    
    Ok(())
}
```

## Explanation

1. **Input parsing**: 
   - Read the first line to get the number of nodes (n) and edges (m)
   - Initialize a vector `degrees` of size n+1 to handle 1-indexed nodes

2. **Edge processing**:
   - For each edge (u, v), increment the degree count for both nodes
   - This works because in an undirected graph, each edge contributes to the degree of both connected nodes

3. **Output**:
   - Print the degrees of all nodes from 1 to n in space-separated format

## Example

**Input:**
```
6 7
1 2
2 3
3 4
4 5
5 6
6 1
2 4
```

**Output:**
```
2 3 3 3 3 3
```

## Time and Space Complexity

- **Time Complexity**: O(n + m) where n is the number of nodes and m is the number of edges
- **Space Complexity**: O(n) for storing the degree array

## Key Points

- The solution uses 1-indexed node numbering as specified in the problem
- Each edge contributes exactly 1 to the degree of each of its two endpoints
- The algorithm efficiently processes all edges in a single pass
- Uses standard input/output handling with proper error checking

