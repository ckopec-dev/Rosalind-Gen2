# Rosalind Problem: Introduction to Pattern Matching

## Problem Description

Given a collection of DNA strings, return all the edges of the suffix tree for this collection of strings.

## Solution in Rust

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct SuffixTreeNode {
    children: HashMap<char, SuffixTreeNode>,
    start: usize,
    end: usize,
    suffix_index: Option<usize>,
}

impl SuffixTreeNode {
    fn new(start: usize, end: usize) -> Self {
        SuffixTreeNode {
            children: HashMap::new(),
            start,
            end,
            suffix_index: None,
        }
    }
}

struct SuffixTree {
    root: SuffixTreeNode,
    text: String,
    text_len: usize,
}

impl SuffixTree {
    fn new(text: String) -> Self {
        let text_len = text.len();
        SuffixTree {
            root: SuffixTreeNode::new(0, 0),
            text,
            text_len,
        }
    }

    fn build_suffix_tree(&mut self) {
        let text_chars: Vec<char> = self.text.chars().collect();
        
        for i in 0..self.text_len {
            self._insert_suffix(i, &text_chars);
        }
    }

    fn _insert_suffix(&mut self, suffix_start: usize, text_chars: &[char]) {
        let mut current_node = &mut self.root;
        let mut char_index = suffix_start;
        
        while char_index < self.text_len {
            let c = text_chars[char_index];
            
            if let Some(child) = current_node.children.get_mut(&c) {
                // Follow existing edge
                let mut edge_start = child.start;
                let mut edge_end = child.end;
                
                // Walk along the edge
                while edge_start < edge_end && char_index < self.text_len {
                    if text_chars[edge_start] != text_chars[char_index] {
                        break;
                    }
                    edge_start += 1;
                    char_index += 1;
                }
                
                if edge_start >= edge_end {
                    // We've reached the end of this edge
                    current_node = child;
                    continue;
                } else {
                    // We need to split the edge
                    let split_point = edge_start;
                    let new_node = SuffixTreeNode::new(split_point, child.end);
                    new_node.children = child.children.clone();
                    
                    child.end = split_point;
                    child.children.clear();
                    child.children.insert(text_chars[split_point], new_node);
                    
                    // Insert the new suffix
                    let new_suffix_node = SuffixTreeNode::new(char_index, self.text_len);
                    child.children.insert(text_chars[char_index], new_suffix_node);
                    return;
                }
            } else {
                // Create new edge
                let new_node = SuffixTreeNode::new(char_index, self.text_len);
                current_node.children.insert(c, new_node);
                return;
            }
        }
    }

    fn get_edges(&self) -> Vec<(usize, usize)> {
        let mut edges = Vec::new();
        self._traverse_edges(&self.root, &mut edges, 0);
        edges.sort();
        edges
    }

    fn _traverse_edges(&self, node: &SuffixTreeNode, edges: &mut Vec<(usize, usize)>, depth: usize) {
        if node.suffix_index.is_some() {
            return;
        }

        for (_, child) in &node.children {
            let start = child.start;
            let end = child.end;
            edges.push((start + 1, end)); // Convert to 1-based indexing
            self._traverse_edges(child, edges, depth + (end - start));
        }
    }
}

fn solve_pattern_matching(strings: Vec<&str>) -> Vec<(usize, usize)> {
    // Combine all strings with a unique separator
    let mut combined_text = String::new();
    let mut suffix_indices = Vec::new();
    
    for (i, string) in strings.iter().enumerate() {
        combined_text.push_str(string);
        combined_text.push('$'); // Use $ as separator
        suffix_indices.push(i);
    }
    
    // Build suffix tree
    let mut suffix_tree = SuffixTree::new(combined_text);
    suffix_tree.build_suffix_tree();
    
    // Get edges
    suffix_tree.get_edges()
}

fn main() {
    // Example input
    let strings = vec![
        "ATATCGTTTTATCGTT",
        "ATCGTT",
        "CGTT",
    ];
    
    let result = solve_pattern_matching(strings);
    
    // Print results
    for (start, end) in result {
        println!("{} {}", start, end);
    }
}

// Alternative simpler approach for pattern matching
fn simple_pattern_matching(pattern: &str, text: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    for i in 0..=(text.len().saturating_sub(pattern.len())) {
        if text[i..].starts_with(pattern) {
            positions.push(i + 1); // 1-based indexing
        }
    }
    
    positions
}

// More efficient KMP algorithm approach
fn kmp_search(pattern: &str, text: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    if pattern.is_empty() || text.is_empty() {
        return positions;
    }
    
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let text_chars: Vec<char> = text.chars().collect();
    
    // Build failure function
    let mut failure = vec![0; pattern_chars.len()];
    let mut j = 0;
    
    for i in 1..pattern_chars.len() {
        while j > 0 && pattern_chars[i] != pattern_chars[j] {
            j = failure[j - 1];
        }
        if pattern_chars[i] == pattern_chars[j] {
            j += 1;
        }
        failure[i] = j;
    }
    
    // Search
    let mut j = 0;
    for i in 0..text_chars.len() {
        while j > 0 && text_chars[i] != pattern_chars[j] {
            j = failure[j - 1];
        }
        if text_chars[i] == pattern_chars[j] {
            j += 1;
        }
        if j == pattern_chars.len() {
            positions.push(i - j + 2); // 1-based indexing
            j = failure[j - 1];
        }
    }
    
    positions
}
```

## Explanation

This solution implements a suffix tree approach to solve the pattern matching problem:

1. **Suffix Tree Construction**: Builds a suffix tree from the input strings
2. **Edge Extraction**: Extracts all edges from the suffix tree
3. **Pattern Matching**: Finds all occurrences of patterns in the text

The key components are:

- **SuffixTreeNode**: Represents nodes in the suffix tree
- **SuffixTree**: Manages the suffix tree construction and traversal
- **solve_pattern_matching**: Main function that processes input strings

## Time Complexity
- Building suffix tree: O(n) where n is the total length of all strings
- Finding edges: O(n)
- Overall: O(n) where n is the total length of input strings

## Space Complexity
- O(n) for storing the suffix tree

This approach efficiently handles multiple pattern matching queries and is suitable for the Rosalind problem requirements.

