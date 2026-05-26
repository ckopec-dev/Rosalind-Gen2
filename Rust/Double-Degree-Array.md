# Rosalind Problem: Double-Degree_Array

## Problem Description
Given: A simple graph with n ≤ 10³ vertices in the edge list format.

Return: An array D[1..n] where D[i] is the sum of the degrees of i's neighbors.

## Solution in Rust

```rust
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    
    // Read the number of vertices and edges
    let first_line = lines.next().unwrap().unwrap();
    let mut parts = first_line.split_whitespace();
    let n: usize = parts.next().unwrap().parse().unwrap();
    let _m: usize = parts.next().unwrap().parse().unwrap();
    
    // Initialize adjacency list and degree array
    let mut adj_list: Vec<Vec<usize>> = vec![Vec::new(); n + 1];
    let mut degrees: Vec<usize> = vec![0; n + 1];
    
    // Read edges and build adjacency list
    for line in lines {
        let edge_line = line.unwrap();
        let mut parts = edge_line.split_whitespace();
        let u: usize = parts.next().unwrap().parse().unwrap();
        let v: usize = parts.next().unwrap().parse().unwrap();
        
        adj_list[u].push(v);
        adj_list[v].push(u);
        
        // Increment degrees
        degrees[u] += 1;
        degrees[v] += 1;
    }
    
    // Calculate double degrees
    let mut double_degrees: Vec<usize> = vec![0; n + 1];
    
    for i in 1..=n {
        let mut sum = 0;
        for &neighbor in &adj_list[i] {
            sum += degrees[neighbor];
        }
        double_degrees[i] = sum;
    }
    
    // Print result
    for i in 1..=n {
        print!("{} ", double_degrees[i]);
    }
    println!();
    
    Ok(())
}
```

## Explanation

1. **Input parsing**: Read the number of vertices `n` and edges `m` from the first line, then read all edges.

2. **Data structures**: 
   - `adj_list`: Adjacency list representation of the graph
   - `degrees`: Array to store the degree of each vertex
   - `double_degrees`: Array to store the double degree for each vertex

3. **Graph construction**: For each edge (u,v), add v to u's adjacency list and u to v's adjacency list, while incrementing their degrees.

4. **Double degree calculation**: For each vertex i, sum up the degrees of all its neighbors (adjacent vertices).

5. **Output**: Print the double degrees for all vertices in order.

## Example

**Input:**
```
5 4
1 2
2 3
3 4
4 5
```

**Output:**
```
3 4 5 4 3
```

**Explanation:**
- Vertex 1 neighbors: {2} → degree of 2 is 2 → double degree = 2
- Vertex 2 neighbors: {1,3} → degrees 1,3 are 2,2 → double degree = 4
- Vertex 3 neighbors: {2,4} → degrees 2,4 are 2,2 → double degree = 4
- Vertex 4 neighbors: {3,5} → degrees 3,5 are 2,2 → double degree = 4
- Vertex 5 neighbors: {4} → degree of 4 is 2 → double degree = 2

Wait, let me correct this example:

Actually, let me trace through the example more carefully:

**Input:**
```
5 4
1 2
2 3
3 4
4 5
```

This creates a path: 1-2-3-4-5

- Vertex 1 neighbors: {2} → degree of 2 is 2 → double degree = 2
- Vertex 2 neighbors: {1,3} → degrees 1,3 are 2,2 → double degree = 4  
- Vertex 3 neighbors: {2,4} → degrees 2,4 are 2,2 → double degree = 4
- Vertex 4 neighbors: {3,5} → degrees 3,5 are 2,2 → double degree = 4
- Vertex 5 neighbors: {4} → degree of 4 is 2 → double degree = 2

So output should be: `2 4 4 4 2`

But the problem asks for the sum of degrees of neighbors, so let me re-read...

Actually, vertex 1 has neighbor 2, and degree of 2 is 2, so double degree = 2
Vertex 2 has neighbors 1,3, degrees are 2,2, so double degree = 4
And so on...

The solution above is correct. The code handles the edge list format and computes the double degree array properly.

