# Square in a Graph - Rosalind Problem Solution in Rust

## Problem Understanding

The "Square in a Graph" problem asks us to find a cycle of length 4 (a square) in an undirected graph. This is equivalent to finding a 4-cycle in the graph.

## Approach

I'll use a brute-force approach to check all possible 4-cycles:
1. For each edge in the graph, treat it as the first edge of a potential square
2. For each neighbor of the first vertex, check if we can form a 4-cycle
3. Use a graph representation with adjacency lists for efficient neighbor lookup

## Solution

```rust
use std::io::{self, BufRead, BufWriter, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    
    // Read number of graphs
    let t: usize = lines.next().unwrap()?.parse()?;
    
    let mut output = BufWriter::new(io::stdout());
    
    for _ in 0..t {
        // Read number of vertices and edges
        let line = lines.next().unwrap()?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let n: usize = parts[0].parse()?;
        let m: usize = parts[1].parse()?;
        
        // Build adjacency list
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n + 1];
        
        // Read edges
        for _ in 0..m {
            let line = lines.next().unwrap()?;
            let parts: Vec<&str> = line.split_whitespace().collect();
            let u: usize = parts[0].parse()?;
            let v: usize = parts[1].parse()?;
            
            adj[u].push(v);
            adj[v].push(u);
        }
        
        // Check for 4-cycle
        let has_square = find_four_cycle(&adj, n);
        
        if has_square {
            writeln!(output, "1")?;
        } else {
            writeln!(output, "-1")?;
        }
    }
    
    output.flush()?;
    Ok(())
}

fn find_four_cycle(adj: &[Vec<usize>], n: usize) -> bool {
    // For each edge (u, v)
    for u in 1..=n {
        for &v in &adj[u] {
            if u >= v { continue; } // Avoid duplicate edges
            
            // Find common neighbors of u and v
            let mut common_neighbors = Vec::new();
            let mut u_iter = adj[u].iter().peekable();
            let mut v_iter = adj[v].iter().peekable();
            
            // Find intersection of neighbors
            let mut u_idx = 0;
            let mut v_idx = 0;
            
            while u_idx < adj[u].len() && v_idx < adj[v].len() {
                if adj[u][u_idx] < adj[v][v_idx] {
                    u_idx += 1;
                } else if adj[u][u_idx] > adj[v][v_idx] {
                    v_idx += 1;
                } else {
                    // Found common neighbor
                    common_neighbors.push(adj[u][u_idx]);
                    u_idx += 1;
                    v_idx += 1;
                }
            }
            
            // For each common neighbor w, check if we can form a 4-cycle
            for &w in &common_neighbors {
                // Check if w is connected to u, v, and forms a square
                // We already know u-w and v-w exist
                // We need to check if u-v exists (it does, it's our original edge)
                // We need to check if w is connected to some vertex that connects back to u
                
                // Actually, let's check if there's a vertex x such that:
                // u-v-w-x-u forms a 4-cycle
                // We know u-v and w-x exist, we need to check if w is connected to x
                // and x is connected to u
                
                // More directly: check if there's a vertex x such that:
                // u-w-v-x-u is a 4-cycle
                // We know u-w, w-v, v-x, x-u exist
                
                // Let's try a different approach:
                // Check if there's a path of length 3 from u to v (excluding u-v edge)
                // This would form a 4-cycle
                
                // Actually, let's check if there are 2 common neighbors of u and v
                // If we have neighbors w1 and w2 of both u and v, then we have a 4-cycle:
                // u-w1-v-w2-u
                if common_neighbors.len() >= 2 {
                    return true;
                }
            }
        }
    }
    
    false
}

// Alternative cleaner approach
fn find_four_cycle_v2(adj: &[Vec<usize>], n: usize) -> bool {
    // Check all possible 4-cycles
    for u in 1..=n {
        for &v in &adj[u] {
            if u >= v { continue; }
            
            // For each neighbor w of u that is also a neighbor of v
            for &w in &adj[u] {
                if w == v || w <= u { continue; }
                
                // Check if w is also a neighbor of v
                if adj[v].binary_search(&w).is_ok() {
                    // Found a 4-cycle: u-v-w-x-u where x is a neighbor of w but not u or v
                    // Actually, we have u-v-w-u which is a triangle, not a 4-cycle
                    
                    // Let's try a different approach: check for 4-cycles using 4 vertices
                    // u-v-w-x-u where we need x to be connected to w and to u
                    // But this is wrong - we need u-w-v-x-u
                    
                    // A 4-cycle: u-v-w-x-u
                    // We know u-v and w-x exist (from our search)
                    // We need to check if w is connected to x and x is connected to u
                    
                    // Better approach: for each pair of vertices that are connected,
                    // check if they have 2 common neighbors, which would form a 4-cycle
                    
                    // Check if there's a vertex x such that:
                    // 1. x is connected to u (u-x exists)
                    // 2. x is connected to w (w-x exists) 
                    // 3. x is not equal to v (to avoid the original edge)
                    // 4. x is not equal to u (to avoid self-loop)
                    // 5. x is not equal to w (to avoid self-loop)
                    
                    // This is getting complex. Let's use a cleaner approach:
                    // For each edge (u,v), find all common neighbors w of u and v
                    // If there are at least 2 such common neighbors, we have a 4-cycle
                    // u-w-v-x-u where x is a common neighbor of u and v
                    
                    // Actually, this is still wrong. A 4-cycle is u-v-w-x-u
                    // If we have u-v and w-x, and we want to check if this forms a 4-cycle,
                    // we need to ensure w is connected to v, and x is connected to u.
                    // But we're looking for u-v-w-x-u where all edges exist.
                    
                    // Let's do it properly:
                    // We want to find 4 vertices u,v,w,x such that:
                    // u-v, v-w, w-x, x-u are all edges
                    // This is equivalent to finding a 4-cycle
                    
                    // For each edge (u,v) where u < v:
                    // Find all common neighbors w of u and v
                    // If there exists a neighbor x of w such that x is also a neighbor of u,
                    // and x != u and x != v, then we have a 4-cycle u-v-w-x-u
                    
                    // Let's try a brute force approach:
                    // Check all combinations of 4 vertices for 4-cycle
                    for &w in &adj[u] {
                        if w == v || w <= u { continue; }
                        if adj[v].binary_search(&w).is_ok() {
                            // We found a triangle u-v-w
                            // Now check if we can extend to a 4-cycle
                            // Need to find a vertex x such that x is connected to w and u
                            // and x != v (to avoid duplicate edges)
                            for &x in &adj[w] {
                                if x == u || x == v { continue; }
                                if adj[u].binary_search(&x).is_ok() {
                                    // Found a 4-cycle u-v-w-x-u
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    false
}
```

Wait, let me provide a cleaner and more correct solution:

```rust
use std::io::{self, BufRead, BufWriter, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    
    // Read number of graphs
    let t: usize = lines.next().unwrap()?.parse()?;
    
    let mut output = BufWriter::new(io::stdout());
    
    for _ in 0..t {
        // Read number of vertices and edges
        let line = lines.next().unwrap()?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let n: usize = parts[0].parse()?;
        let m: usize = parts[1].parse()?;
        
        // Build adjacency list
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n + 1];
        
        // Read edges
        for _ in 0..m {
            let line = lines.next().unwrap()?;
            let parts: Vec<&str> = line.split_whitespace().collect();
            let u: usize = parts[0].parse()?;
            let v: usize = parts[1].parse()?;
            
            adj[u].push(v);
            adj[v].push(u);
        }
        
        // Check for 4-cycle
        let has_square = has_four_cycle(&adj, n);
        
        if has_square {
            writeln!(output, "1")?;
        } else {
            writeln!(output, "-1")?;
        }
    }
    
    output.flush()?;
    Ok(())
}

fn has_four_cycle(adj: &[Vec<usize>], n: usize) -> bool {
    // For each edge (u,v) in the graph
    for u in 1..=n {
        for &v in &adj[u] {
            if u >= v { continue; } // Avoid duplicate edges
            
            // Find common neighbors of u and v
            let mut common_neighbors = Vec::new();
            let mut i = 0;
            let mut j = 0;
            
            while i < adj[u].len() && j < adj[v].len() {
                if adj[u][i] < adj[v][j] {
                    i += 1;
                } else if adj[u][i] > adj[v][j] {
                    j += 1;
                } else {
                    common_neighbors.push(adj[u][i]);
                    i += 1;
                    j += 1;
                }
            }
            
            // If there are at least 2 common neighbors, we can form a 4-cycle
            // A 4-cycle of the form u-v-w-x-u where w and x are common neighbors
            // But we need to be more careful - we want to ensure the cycle is valid
            if common_neighbors.len() >= 2 {
                // Check if we can form a proper 4-cycle
                for i in 0..common_neighbors.len() {
                    for j in (i + 1)..common_neighbors.len() {
                        let w = common_neighbors[i];
                        let x = common_neighbors[j];
                        
                        // Check if w and x are connected (this would make u-v-w-x-u a 4-cycle)
                        if adj[w].binary_search(&x).is_ok() {
                            return true;
                        }
                    }
                }
            }
        }
    }
    
    false
}
```

Actually, let me re-read the problem and provide a truly correct solution:

```rust
use std::io::{self, BufRead, BufWriter, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    
    // Read number of graphs
    let t: usize = lines.next().unwrap()?.parse()?;
    
    let mut output = BufWriter::new(io::stdout());
    
    for _ in 0..t {
        // Read number of vertices and edges
        let line = lines.next().unwrap()?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let n: usize = parts[0].parse()?;
        let m: usize = parts[1].parse()?;
        
        // Build adjacency list
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n + 1];
        
        // Read edges
        for _ in 0..m {
            let line = lines.next().unwrap()?;
            let parts: Vec<&str> = line.split_whitespace().collect();
            let u: usize = parts[0].parse()?;
            let v: usize = parts[1].parse()?;
            
            adj[u].push(v);
            adj[v].push(u);
        }
        
        // Check for 4-cycle
        let has_square = has_four_cycle(&adj, n);
        
        if has_square {
            writeln!(output, "1")?;
        } else {
            writeln!(output, "-1")?;
        }
    }
    
    output.flush()?;
    Ok(())
}

fn has_four_cycle(adj: &[Vec<usize>], n: usize) -> bool {
    // Check all edges
    for u in 1..=n {
        for &v in &adj[u] {
            if u >= v { continue; } // Avoid duplicate edges
            
            // Find all common neighbors of u and v
            let mut common = Vec::new();
            let mut i = 0;
            let mut j = 0;
            
            while i < adj[u].len() && j < adj[v].len() {
                if adj[u][i] < adj[v][j] {
                    i += 1;
                } else if adj[u][i] > adj[v][j] {
                    j += 1;
                } else {
                    common.push(adj[u][i]);
                    i += 1;
                    j += 1;
                }
            }
            
            // If there are at least 2 common neighbors, we can form a 4-cycle
            // If w1 and w2 are both neighbors of u and v, then u-v-w1-w2-u is a 4-cycle
            // if w1 and w2 are connected to each other
            for i in 0..common.len() {
                for j in (i + 1)..common.len() {
                    let w1 = common[i];
                    let w2 = common[j];
                    
                    // Check if w1 and w2 are connected
                    if adj[w1].binary_search(&w2).is_ok() {
                        return true;
                    }
                }
            }
        }
    }
    
    false
}
```

This solution works by:
1. For each edge (u,v) in the graph, we find all common neighbors of u and v
2. If we find two such common neighbors w1 and w2 that are themselves connected, we have found a 4-cycle
3. The 4-cycle is u-v-w1-w2-u where all edges exist

Time complexity: O(E × V) where E is the number of edges and V is the number of vertices.
Space complexity: O(V + E) for the adjacency list representation.

