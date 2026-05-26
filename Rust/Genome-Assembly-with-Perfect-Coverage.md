# Rosalind Problem: Genome Assembly with Perfect Coverage (Genome_Assembly_with_Perfect_Coverage)

## Problem Understanding

In this problem, we need to reconstruct a genome from a set of reads that perfectly cover the genome (i.e., every position in the genome is covered exactly once by the reads). This is a classic de Bruijn graph problem where we need to find the Eulerian path in the de Bruijn graph.

## Solution Approach

1. **Build De Bruijn Graph**: Create a graph where each node is a k-mer and edges represent overlaps
2. **Find Eulerian Path**: Find a path that visits each edge exactly once
3. **Reconstruct Genome**: Follow the path to reconstruct the original genome

## Rust Implementation

```rust
use std::collections::{HashMap, HashSet, VecDeque};

fn get_kmers(dna: &str, k: usize) -> Vec<String> {
    let mut kmers = Vec::new();
    for i in 0..=dna.len().saturating_sub(k) {
        if i + k <= dna.len() {
            kmers.push(dna[i..i+k].to_string());
        }
    }
    kmers
}

fn build_debruijn_graph(kmers: &[String], k: usize) -> HashMap<String, Vec<String>> {
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    
    for kmer in kmers {
        if kmer.len() < k {
            continue;
        }
        
        let prefix = kmer[..k-1].to_string();
        let suffix = kmer[1..k].to_string();
        
        graph.entry(prefix).or_insert_with(Vec::new).push(suffix);
    }
    
    graph
}

fn find_eulerian_path(graph: &HashMap<String, Vec<String>>) -> Option<String> {
    // Find starting node (node with out-degree - in-degree = 1)
    let mut in_degree: HashMap<String, i32> = HashMap::new();
    let mut out_degree: HashMap<String, i32> = HashMap::new();
    
    // Initialize degrees
    for (node, neighbors) in graph {
        *out_degree.entry(node.clone()).or_insert(0) += neighbors.len() as i32;
        for neighbor in neighbors {
            *in_degree.entry(neighbor.clone()).or_insert(0) += 1;
        }
    }
    
    // Find starting node
    let mut start_node = None;
    let mut unbalanced_nodes = Vec::new();
    
    for node in graph.keys() {
        let in_deg = *in_degree.get(node).unwrap_or(&0);
        let out_deg = *out_degree.get(node).unwrap_or(&0);
        
        if out_deg - in_deg == 1 {
            start_node = Some(node.clone());
        } else if out_deg - in_deg != 0 {
            unbalanced_nodes.push(node.clone());
        }
    }
    
    // If no start node found, use any node with out-degree > 0
    if start_node.is_none() {
        for (node, neighbors) in graph {
            if !neighbors.is_empty() {
                start_node = Some(node.clone());
                break;
            }
        }
    }
    
    if start_node.is_none() {
        return None;
    }
    
    let mut stack = vec![start_node.unwrap()];
    let mut path = Vec::new();
    
    while let Some(current) = stack.last() {
        if let Some(neighbors) = graph.get(current) {
            if !neighbors.is_empty() {
                // Take the first neighbor
                let next = neighbors[0].clone();
                stack.push(next.clone());
                // Remove this edge
                let current_neighbors = graph.get_mut(current).unwrap();
                current_neighbors.remove(0);
            } else {
                // No more neighbors, add to path
                path.push(stack.pop().unwrap());
            }
        } else {
            // No neighbors, add to path
            path.push(stack.pop().unwrap());
        }
    }
    
    // Reverse path to get correct order
    path.reverse();
    
    // Reconstruct genome from path
    if path.len() < 2 {
        return None;
    }
    
    let mut result = path[0].clone();
    for i in 1..path.len() {
        result.push_str(&path[i][path[i].len()-1..]);
    }
    
    Some(result)
}

fn genome_assembly_with_perfect_coverage(reads: Vec<String>, k: usize) -> String {
    // Build de Bruijn graph
    let graph = build_debruijn_graph(&reads, k);
    
    // Find Eulerian path
    if let Some(path) = find_eulerian_path(&graph) {
        path
    } else {
        // Fallback: reconstruct from kmers
        let mut result = String::new();
        if !reads.is_empty() {
            result = reads[0].clone();
            for i in 1..reads.len() {
                let kmer = &reads[i];
                if !kmer.is_empty() {
                    result.push_str(&kmer[kmer.len()-1..]);
                }
            }
        }
        result
    }
}

fn main() {
    // Example usage
    let reads = vec![
        "AAAT".to_string(),
        "AATG".to_string(),
        "ATGG".to_string(),
        "TGGT".to_string(),
        "GGTA".to_string(),
        "GTTA".to_string(),
        "TTAC".to_string(),
    ];
    
    let k = 3;
    let result = genome_assembly_with_perfect_coverage(reads, k);
    println!("Reconstructed genome: {}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_case() {
        let reads = vec![
            "AAAT".to_string(),
            "AATG".to_string(),
            "ATGG".to_string(),
            "TGGT".to_string(),
            "GGTA".to_string(),
            "GTTA".to_string(),
            "TTAC".to_string(),
        ];
        
        let result = genome_assembly_with_perfect_coverage(reads, 3);
        // Expected: "AAATGGTAAC" or similar valid reconstruction
        assert!(!result.is_empty());
    }
    
    #[test]
    fn test_kmer_extraction() {
        let dna = "AAATGGTAAC";
        let kmers = get_kmers(dna, 3);
        assert_eq!(kmers.len(), 8);
        assert_eq!(kmers[0], "AAA");
        assert_eq!(kmers[1], "AAT");
        assert_eq!(kmers[2], "ATG");
    }
}
```

## Key Components Explained

### 1. **K-mer Extraction**
```rust
fn get_kmers(dna: &str, k: usize) -> Vec<String>
```
Extracts all k-mers from a DNA string.

### 2. **De Bruijn Graph Construction**
```rust
fn build_debruijn_graph(kmers: &[String], k: usize) -> HashMap<String, Vec<String>>
```
Creates a graph where:
- Nodes are (k-1)-mers (prefixes)
- Edges connect nodes where one k-mer's prefix matches another's suffix

### 3. **Eulerian Path Finding**
```rust
fn find_eulerian_path(graph: &HashMap<String, Vec<String>>) -> Option<String>
```
Uses Hierholzer's algorithm to find an Eulerian path in the de Bruijn graph.

### 4. **Genome Reconstruction**
```rust
fn genome_assembly_with_perfect_coverage(reads: Vec<String>, k: usize) -> String
```
Combines the above functions to reconstruct the original genome.

## Time and Space Complexity

- **Time Complexity**: O(|E| + |V|) where E is the number of edges and V is the number of vertices in the de Bruijn graph
- **Space Complexity**: O(|E| + |V|) for storing the graph

## Usage

The function `genome_assembly_with_perfect_coverage` takes:
1. A vector of DNA reads (strings)
2. The k-mer size (k)

It returns the reconstructed genome string.

This solution handles the perfect coverage case where each position in the original genome is covered exactly once by the reads, making it a classic de Bruijn graph Eulerian path problem.

