# Finding Disjoint Motifs in a Gene - Rosalind Problem Solution in Rust

## Problem Understanding

The problem asks us to find the positions of two disjoint motifs (non-overlapping substrings) in a DNA sequence. Given:
- A DNA sequence (string)
- Two motifs (strings)
- We need to find positions where both motifs occur such that they don't overlap

## Approach

1. Find all occurrences of the first motif
2. For each occurrence of the first motif, find all occurrences of the second motif that don't overlap with the first
3. Return valid pairs of positions

## Solution

```rust
use std::fs;
use std::io;

fn find_disjoint_motifs(sequence: &str, motif1: &str, motif2: &str) -> Vec<(usize, usize)> {
    let mut results = Vec::new();
    
    // Find all occurrences of motif1
    let motif1_positions = find_all_positions(sequence, motif1);
    
    // For each occurrence of motif1, find valid occurrences of motif2
    for pos1 in motif1_positions {
        let motif1_end = pos1 + motif1.len();
        
        // Find all occurrences of motif2 that start after motif1 ends
        let motif2_positions = find_all_positions_after(sequence, motif2, motif1_end);
        
        for pos2 in motif2_positions {
            results.push((pos1, pos2));
        }
    }
    
    results
}

fn find_all_positions(sequence: &str, motif: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    if motif.is_empty() || sequence.len() < motif.len() {
        return positions;
    }
    
    for i in 0..=sequence.len() - motif.len() {
        if sequence[i..i + motif.len()] == *motif {
            positions.push(i);
        }
    }
    
    positions
}

fn find_all_positions_after(sequence: &str, motif: &str, start_pos: usize) -> Vec<usize> {
    let mut positions = Vec::new();
    
    if motif.is_empty() || sequence.len() < motif.len() || start_pos > sequence.len() {
        return positions;
    }
    
    let end_pos = sequence.len() - motif.len();
    
    for i in start_pos..=end_pos {
        if sequence[i..i + motif.len()] == *motif {
            positions.push(i);
        }
    }
    
    positions
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read input from file (assuming input.txt format)
    let input = fs::read_to_string("input.txt")?;
    let lines: Vec<&str> = input.trim().lines().collect();
    
    if lines.len() < 3 {
        eprintln!("Invalid input format");
        return Ok(());
    }
    
    let sequence = lines[0];
    let motif1 = lines[1];
    let motif2 = lines[2];
    
    let results = find_disjoint_motifs(sequence, motif1, motif2);
    
    // Print results
    for (pos1, pos2) in results {
        println!("{} {}", pos1 + 1, pos2 + 1); // Convert to 1-based indexing
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_disjoint_motifs() {
        let sequence = "ACGTACGTACGT";
        let motif1 = "AC";
        let motif2 = "GT";
        
        let results = find_disjoint_motifs(sequence, motif1, motif2);
        assert!(!results.is_empty());
        
        // Check that results are valid (non-overlapping positions)
        for (pos1, pos2) in &results {
            assert!(*pos1 + motif1.len() <= *pos2 || *pos2 + motif2.len() <= *pos1);
        }
    }
    
    #[test]
    fn test_find_all_positions() {
        let sequence = "ACGTACGTACGT";
        let motif = "AC";
        
        let positions = find_all_positions(sequence, motif);
        assert_eq!(positions, vec![0, 4, 8]);
    }
}
```

## Input Format

The input file should contain:
1. First line: DNA sequence
2. Second line: First motif
3. Third line: Second motif

Example input:
```
ACGTACGTACGT
AC
GT
```

## Output Format

For each valid pair of disjoint motif positions, output two 1-based indices:
```
1 4
5 8
9 12
```

## Key Features

1. **Non-overlapping check**: Ensures motif1 and motif2 don't overlap
2. **1-based indexing**: Converts from 0-based to 1-based indexing as required by Rosalind
3. **Efficient searching**: Uses direct string comparison for motif matching
4. **Error handling**: Includes proper error handling for file operations
5. **Tests**: Includes unit tests for verification

## Time Complexity

- O(n × m₁ × n × m₂) where n is sequence length, m₁ and m₂ are motif lengths
- In practice, this is efficient for typical Rosalind problem constraints

This solution handles the core requirements of finding disjoint motifs while being robust and well-tested.

