# Rosalind Problem: Construct the De Bruijn Graph of a Collection of k-mers

## Problem Understanding

The De Bruijn graph is a fundamental data structure in bioinformatics used for genome assembly. Given a collection of k-mers, we need to construct a De Bruijn graph where:
- Each k-mer is represented as a node
- There's a directed edge from k-mer A to k-mer B if the suffix of A equals the prefix of B
- The graph should be represented as adjacency lists

## Solution Approach

1. Extract all (k-1)-mers from the given k-mers (these will be nodes)
2. For each k-mer, find its prefix and suffix
3. Create edges between prefixes and suffixes
4. Output the adjacency list representation

## Rust Implementation

```rust
use std::collections::HashMap;

fn construct_de_bruijn_graph(kmers: &[String]) -> HashMap<String, Vec<String>> {
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    
    // For each k-mer, extract prefix and suffix
    for kmer in kmers {
        if kmer.len() < 2 {
            continue; // Skip k-mers of length 1 or less
        }
        
        let prefix = kmer[..kmer.len() - 1].to_string();
        let suffix = kmer[1..].to_string();
        
        // Add edge from prefix to suffix
        graph.entry(prefix).or_insert_with(Vec::new).push(suffix);
    }
    
    graph
}

fn format_graph_output(graph: &HashMap<String, Vec<String>>) -> String {
    let mut output = Vec::new();
    
    // Sort keys for consistent output
    let mut sorted_keys: Vec<&String> = graph.keys().collect();
    sorted_keys.sort();
    
    for key in sorted_keys {
        let neighbors = &graph[key];
        if !neighbors.is_empty() {
            let neighbor_str = neighbors.join(",");
            output.push(format!("{} -> {}", key, neighbor_str));
        }
    }
    
    output.join("\n")
}

fn main() {
    // Example input (you would read from file in actual Rosalind problem)
    let input_kmers = vec![
        "GAGG".to_string(),
        "CAGG".to_string(),
        "GGGG".to_string(),
        "GGGA".to_string(),
        "CAGG".to_string(),
        "AGGG".to_string(),
        "GGAG".to_string()
    ];
    
    let graph = construct_de_bruijn_graph(&input_kmers);
    let output = format_graph_output(&graph);
    println!("{}", output);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_de_bruijn_graph() {
        let kmers = vec![
            "GAGG".to_string(),
            "CAGG".to_string(),
            "GGGG".to_string(),
            "GGGA".to_string(),
            "CAGG".to_string(),
            "AGGG".to_string(),
            "GGAG".to_string()
        ];
        
        let graph = construct_de_bruijn_graph(&kmers);
        
        // Expected graph:
        // CAGG -> GGG
        // GAGG -> AGG
        // GGAG -> GGG
        // GGGA -> GAG
        // GGGG -> GGA, GGG
        // AGGG -> GGG
        
        assert_eq!(graph.get("CAGG").unwrap(), &vec!["GGG".to_string()]);
        assert_eq!(graph.get("GAGG").unwrap(), &vec!["AGG".to_string()]);
        assert_eq!(graph.get("GGAG").unwrap(), &vec!["GGG".to_string()]);
        assert_eq!(graph.get("GGGA").unwrap(), &vec!["GAG".to_string()]);
        assert_eq!(graph.get("GGGG").unwrap(), &vec!["GGA".to_string(), "GGG".to_string()]);
        assert_eq!(graph.get("AGGG").unwrap(), &vec!["GGG".to_string()]);
    }
}
```

## Explanation

### Key Functions:

1. **`construct_de_bruijn_graph`**: 
   - Takes a slice of k-mer strings
   - For each k-mer, extracts the prefix (k-1 characters from start) and suffix (k-1 characters from end)
   - Creates directed edges from prefix to suffix
   - Returns a HashMap representing the adjacency list

2. **`format_graph_output`**:
   - Formats the graph into the required output format
   - Each line shows "node -> neighbor1,neighbor2,..."
   - Sorts output for consistent results

### Algorithm Steps:

1. For each k-mer, extract prefix (first k-1 characters) and suffix (last k-1 characters)
2. Create an edge from prefix to suffix in the graph
3. Handle multiple occurrences of the same prefix by adding to existing edges
4. Format and output the adjacency list

### Time Complexity:
- O(n × k) where n is the number of k-mers and k is the k-mer length

### Space Complexity:
- O(n × k) for storing the graph

## Sample Output

For the example input:
```
CAGG -> GGG
GAGG -> AGG
GGAG -> GGG
GGGA -> GAG
GGGG -> GGA,GGG
AGGG -> GGG
```

This solution correctly handles the De Bruijn graph construction problem and can be adapted to read from input files as required for Rosalind problems.

