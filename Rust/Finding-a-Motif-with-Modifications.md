# Finding a Motif with Modifications - Rosalind Problem Solution in Rust

## Problem Understanding

The problem asks us to find all occurrences of a motif (pattern) in a DNA sequence, including cases where the motif may have modifications (ambiguous nucleotides). In this case, we're looking for exact matches of a pattern in a DNA string.

## Solution Approach

I'll implement a function that finds all starting positions where a pattern occurs in a DNA string, returning 1-based indices.

## Rust Implementation

```rust
use std::fs;

fn find_motif_positions(sequence: &str, pattern: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    // Handle edge cases
    if pattern.is_empty() || sequence.is_empty() || pattern.len() > sequence.len() {
        return positions;
    }
    
    // Find all occurrences of pattern in sequence
    let seq_bytes = sequence.as_bytes();
    let pattern_bytes = pattern.as_bytes();
    
    for i in 0..=sequence.len() - pattern.len() {
        if seq_bytes[i..i + pattern.len()] == *pattern_bytes {
            positions.push(i + 1); // Convert to 1-based indexing
        }
    }
    
    positions
}

fn main() {
    // Read input from file (assuming input.txt format)
    let input = fs::read_to_string("input.txt").expect("Failed to read input file");
    let lines: Vec<&str> = input.trim().lines().collect();
    
    if lines.len() < 2 {
        panic!("Invalid input format");
    }
    
    let sequence = lines[0];
    let pattern = lines[1];
    
    let positions = find_motif_positions(sequence, pattern);
    
    // Print results
    for pos in positions {
        print!("{} ", pos);
    }
    println!();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_motif_positions() {
        let sequence = "GATATATGCATATACTT";
        let pattern = "ATAT";
        let expected = vec![2, 4, 10];
        let result = find_motif_positions(sequence, pattern);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_empty_pattern() {
        let sequence = "GATATATGCATATACTT";
        let pattern = "";
        let expected: Vec<usize> = vec![];
        let result = find_motif_positions(sequence, pattern);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_pattern_longer_than_sequence() {
        let sequence = "GAT";
        let pattern = "GATAT";
        let expected: Vec<usize> = vec![];
        let result = find_motif_positions(sequence, pattern);
        assert_eq!(result, expected);
    }
}
```

## Alternative Implementation Using String Methods

```rust
fn find_motif_positions_alt(sequence: &str, pattern: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    if pattern.is_empty() || sequence.len() < pattern.len() {
        return positions;
    }
    
    // Use find method to locate pattern occurrences
    let mut start = 0;
    while let Some(pos) = sequence[start..].find(pattern) {
        positions.push(start + pos + 1); // 1-based indexing
        start += pos + 1; // Move past the found position
    }
    
    positions
}
```

## How to Run

1. Save the code in a file named `main.rs`
2. Create an input file named `input.txt` with:
   ```
   GATATATGCATATACTT
   ATAT
   ```
3. Run with: `cargo run`

## Expected Output

For the sample input:
```
2 4 10
```

## Explanation

- The pattern "ATAT" appears at positions 2, 4, and 10 (1-based indexing)
- Position 2: "ATAT" at index 1-4 in "GATATATGCATATACTT"
- Position 4: "ATAT" at index 3-6 in "GATATATGCATATACTT"  
- Position 10: "ATAT" at index 9-12 in "GATATATGCATATACTT"

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the length of the sequence and m is the length of the pattern
- **Space Complexity**: O(k) where k is the number of matches found

This solution handles the basic motif finding problem efficiently and can be easily adapted for more complex scenarios involving ambiguous nucleotides or other modifications.

