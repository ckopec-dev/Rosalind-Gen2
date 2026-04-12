# Rosalind Problem 2-Satisfiability in Rust

## Problem Understanding

2-Satisfiability (2-SAT) is a special case of the Boolean satisfiability problem where each clause contains exactly 2 literals. Given a Boolean formula in conjunctive normal form (CNF) with 2 literals per clause, we need to determine if there exists an assignment of truth values to variables that makes the entire formula true.

## Approach

I'll solve this using the **implication graph** approach:
1. Convert each 2-clause into implications
2. Build a directed graph of implications
3. Check if any variable and its negation are in the same strongly connected component
4. If they are, the formula is unsatisfiable

## Solution

```rust
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
struct Graph {
    adj_list: HashMap<i32, Vec<i32>>,
    reverse_adj_list: HashMap<i32, Vec<i32>>,
    vertices: Vec<i32>,
}

impl Graph {
    fn new() -> Self {
        Graph {
            adj_list: HashMap::new(),
            reverse_adj_list: HashMap::new(),
            vertices: Vec::new(),
        }
    }

    fn add_vertex(&mut self, vertex: i32) {
        if !self.adj_list.contains_key(&vertex) {
            self.adj_list.insert(vertex, Vec::new());
            self.reverse_adj_list.insert(vertex, Vec::new());
            self.vertices.push(vertex);
        }
    }

    fn add_edge(&mut self, from: i32, to: i32) {
        self.add_vertex(from);
        self.add_vertex(to);
        self.adj_list.get_mut(&from).unwrap().push(to);
        self.reverse_adj_list.get_mut(&to).unwrap().push(from);
    }

    fn reverse(&self) -> Graph {
        let mut reversed = Graph::new();
        for &vertex in &self.vertices {
            reversed.add_vertex(vertex);
        }
        for (&from, neighbors) in &self.adj_list {
            for &to in neighbors {
                reversed.add_edge(to, from);
            }
        }
        reversed
    }

    fn dfs(&self, start: i32, visited: &mut Vec<bool>, stack: &mut Vec<i32>) {
        visited[start as usize] = true;
        if let Some(neighbors) = self.adj_list.get(&start) {
            for &neighbor in neighbors {
                if !visited[neighbor as usize] {
                    self.dfs(neighbor, visited, stack);
                }
            }
        }
        stack.push(start);
    }

    fn dfs_reverse(&self, start: i32, visited: &mut Vec<bool>, component: &mut Vec<i32>) {
        visited[start as usize] = true;
        component.push(start);
        if let Some(neighbors) = self.reverse_adj_list.get(&start) {
            for &neighbor in neighbors {
                if !visited[neighbor as usize] {
                    self.dfs_reverse(neighbor, visited, component);
                }
            }
        }
    }

    fn get_scc(&self) -> Vec<Vec<i32>> {
        let mut visited = vec![false; 200000]; // Assuming max vertices
        let mut stack = Vec::new();
        let mut sccs = Vec::new();

        // First DFS to get finishing times
        for &vertex in &self.vertices {
            if !visited[vertex as usize] {
                self.dfs(vertex, &mut visited, &mut stack);
            }
        }

        // Create reversed graph
        let reversed = self.reverse();

        // Reset visited array
        visited = vec![false; 200000];

        // Second DFS on reversed graph in reverse finishing time order
        while let Some(vertex) = stack.pop() {
            if !visited[vertex as usize] {
                let mut component = Vec::new();
                reversed.dfs_reverse(vertex, &mut visited, &mut component);
                sccs.push(component);
            }
        }

        sccs
    }
}

fn solve_2sat(clauses: Vec<(i32, i32)>, num_vars: i32) -> String {
    let mut graph = Graph::new();
    
    // For each clause (a, b), we add two implications:
    // 1. !a -> b (if a is false, then b must be true)
    // 2. !b -> a (if b is false, then a must be true)
    for (a, b) in clauses {
        // Add implications: !a -> b and !b -> a
        graph.add_edge(-a, b);
        graph.add_edge(-b, a);
    }

    // Find strongly connected components
    let sccs = graph.get_scc();
    
    // Check if any variable and its negation are in the same SCC
    let mut var_to_scc: HashMap<i32, usize> = HashMap::new();
    
    for (scc_index, scc) in sccs.iter().enumerate() {
        for &var in scc {
            var_to_scc.insert(var, scc_index);
        }
    }
    
    // If a variable and its negation are in the same SCC, the formula is unsatisfiable
    for var in 1..=num_vars {
        if let (Some(scc_a), Some(scc_neg_a)) = (var_to_scc.get(&var), var_to_scc.get(&(-var))) {
            if scc_a == scc_neg_a {
                return "UNSATISFIABLE".to_string();
            }
        }
    }
    
    "SATISFIABLE".to_string()
}

fn main() {
    // Example input from Rosalind problem
    let clauses = vec![
        (1, 2),
        (-1, 2),
        (-1, -2),
    ];
    let num_vars = 2;
    
    let result = solve_2sat(clauses, num_vars);
    println!("{}", result);
    
    // Another example
    let clauses2 = vec![
        (1, -2),
        (2, -3),
        (3, -1),
    ];
    let num_vars2 = 3;
    
    let result2 = solve_2sat(clauses2, num_vars2);
    println!("{}", result2);
}

// Alternative implementation using a more direct approach
fn solve_2sat_direct(clauses: Vec<(i32, i32)>, num_vars: i32) -> String {
    // This is a simplified version that works for small inputs
    // For the full implementation, we'd use the SCC approach above
    
    // For demonstration, let's try a simple approach:
    // Try all possible assignments for small numbers of variables
    
    if num_vars <= 20 { // Only for small cases
        // Try all 2^num_vars assignments
        for assignment in 0..(1 << num_vars) {
            let mut is_satisfiable = true;
            
            for &(a, b) in &clauses {
                let var_a = (assignment & (1 << (a.abs() - 1))) != 0;
                let var_b = (assignment & (1 << (b.abs() - 1))) != 0;
                
                let val_a = if a > 0 { var_a } else { !var_a };
                let val_b = if b > 0 { var_b } else { !var_b };
                
                if !val_a && !val_b {
                    is_satisfiable = false;
                    break;
                }
            }
            
            if is_satisfiable {
                return "SATISFIABLE".to_string();
            }
        }
        return "UNSATISFIABLE".to_string();
    }
    
    // For larger cases, use the SCC approach
    solve_2sat(clauses, num_vars)
}
```

## Explanation

### Key Components:

1. **Graph Structure**: 
   - Stores adjacency lists for both forward and reverse edges
   - Supports graph operations needed for SCC detection

2. **Implication Conversion**:
   - For clause (a ∨ b), we convert to implications: ¬a → b and ¬b → a
   - This transforms the 2-SAT problem into a graph problem

3. **Strongly Connected Components (SCC)**:
   - Uses Kosaraju's algorithm with two DFS passes
   - If variable x and ¬x are in the same SCC, the formula is unsatisfiable

4. **Solution Logic**:
   - Build implication graph from clauses
   - Find SCCs using Kosaraju's algorithm
   - Check if any variable and its negation are in the same component
   - Return "SATISFIABLE" or "UNSATISFIABLE" accordingly

## Time Complexity
- **Time**: O(V + E) where V is the number of variables and E is the number of clauses
- **Space**: O(V + E) for storing the graph

## Usage
The solution handles the standard 2-SAT problem format and can be adapted for different input formats as required by Rosalind's specific problem requirements.

