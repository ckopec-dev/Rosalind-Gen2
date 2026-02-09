# Rosalind Problem: Compute the 2-Break Distance Between a Pair of Genomes

## Problem Understanding

The 2-break distance between two genomes is the minimum number of 2-break operations needed to transform one genome into another. A 2-break operation splits two edges and creates two new edges, effectively rearranging genome structure.

## Approach

1. Convert genomes to adjacency lists
2. Find cycles in the breakpoint graph
3. Calculate distance using the formula: `distance = (number of blocks - number of cycles) / 2`

## Solution

```rust
use std::collections::{HashMap, HashSet};
use std::io;

fn read_input() -> Vec<String> {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().split('\n').map(|s| s.to_string()).collect()
}

fn parse_genome(genome_str: &str) -> Vec<Vec<i32>> {
    let genome_str = genome_str.trim();
    if genome_str.is_empty() {
        return vec![];
    }
    
    let mut cycles = Vec::new();
    let parts: Vec<&str> = genome_str[1..genome_str.len()-1].split(")(").collect();
    
    for part in parts {
        let mut cycle = Vec::new();
        let numbers: Vec<i32> = part.split(' ')
            .filter_map(|s| s.parse::<i32>().ok())
            .collect();
        cycle.extend(numbers);
        cycles.push(cycle);
    }
    
    cycles
}

fn get_adjacency_list(genome: &Vec<Vec<i32>>) -> HashMap<i32, i32> {
    let mut adj_list = HashMap::new();
    
    for cycle in genome {
        let n = cycle.len();
        for i in 0..n {
            let u = cycle[i];
            let v = cycle[(i + 1) % n];
            adj_list.insert(u, v);
            adj_list.insert(-v, -u);
        }
    }
    
    adj_list
}

fn get_blocks_count(genome: &Vec<Vec<i32>>) -> usize {
    let mut blocks = HashSet::new();
    for cycle in genome {
        for &block in cycle {
            blocks.insert(block.abs());
        }
    }
    blocks.len()
}

fn count_cycles(genome1: &Vec<Vec<i32>>, genome2: &Vec<Vec<i32>>) -> usize {
    let adj1 = get_adjacency_list(genome1);
    let adj2 = get_adjacency_list(genome2);
    
    let mut visited = HashSet::new();
    let mut cycles = 0;
    
    // Build the breakpoint graph
    let mut graph = HashMap::new();
    
    // Add edges from genome1
    for (u, &v) in &adj1 {
        graph.entry(u).or_insert_with(Vec::new).push(v);
        graph.entry(v).or_insert_with(Vec::new).push(u);
    }
    
    // Add edges from genome2
    for (u, &v) in &adj2 {
        graph.entry(u).or_insert_with(Vec::new).push(v);
        graph.entry(v).or_insert_with(Vec::new).push(u);
    }
    
    // Count connected components (cycles)
    for &node in graph.keys() {
        if !visited.contains(&node) {
            let mut stack = vec![node];
            visited.insert(node);
            
            while let Some(current) = stack.pop() {
                if let Some(neighbors) = graph.get(&current) {
                    for &neighbor in neighbors {
                        if !visited.contains(&neighbor) {
                            visited.insert(neighbor);
                            stack.push(neighbor);
                        }
                    }
                }
            }
            cycles += 1;
        }
    }
    
    cycles
}

fn compute_2break_distance(genome1: &Vec<Vec<i32>>, genome2: &Vec<Vec<i32>>) -> usize {
    let blocks = get_blocks_count(genome1);
    let cycles = count_cycles(genome1, genome2);
    (blocks - cycles) / 2
}

fn main() {
    let input_lines = read_input();
    
    if input_lines.len() < 2 {
        println!("Not enough input lines");
        return;
    }
    
    let genome1 = parse_genome(&input_lines[0]);
    let genome2 = parse_genome(&input_lines[1]);
    
    let distance = compute_2break_distance(&genome1, &genome2);
    println!("{}", distance);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_case() {
        let genome1 = vec![vec![1, 2, 3, 4, 5, 6]];
        let genome2 = vec![vec![1, -3, -6, -5, 4, 2]];
        let distance = compute_2break_distance(&genome1, &genome2);
        assert_eq!(distance, 2);
    }
}
```

## Explanation

1. **Input Parsing**: The solution reads two genomes from input, each represented as cycles of signed integers.

2. **Adjacency List Construction**: For each genome, we build an adjacency list mapping each element to its neighbor in the circular permutation.

3. **Cycle Detection**: We construct the breakpoint graph by combining edges from both genomes and count the connected components (cycles).

4. **Distance Calculation**: Using the formula `distance = (number of blocks - number of cycles) / 2`, we compute the 2-break distance.

## Time Complexity
- O(n) where n is the total number of elements in both genomes
- Building adjacency lists: O(n)
- Finding cycles: O(n)

## Space Complexity
- O(n) for storing adjacency lists and visited sets

This solution handles the standard Rosalind format for genome input and correctly computes the 2-break distance between two genomes.

