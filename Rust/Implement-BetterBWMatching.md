# Rosalind Problem: Implement_BetterBWMatching in Rust

## Problem Understanding

The BetterBWMatching algorithm is used to efficiently find the number of occurrences of a pattern in a Burrows-Wheeler Transform (BWT) string. This is an optimization over the basic BWMatching that uses precomputed data structures to avoid recomputing the same values repeatedly.

## Solution Approach

The algorithm uses:
1. **First Occurrence array**: For each character, stores the first position it appears in the sorted BWT
2. **Count array**: For each position in BWT and each character, stores how many times that character appears up to that position
3. **Last-to-First mapping**: Maps positions in the last column to positions in the first column

## Implementation

```rust
use std::collections::HashMap;

fn better_bw_matching(
    bwt: &str,
    patterns: &[&str],
) -> Vec<usize> {
    // Preprocessing step
    let (first_column, first_to_last, count, first_occurrence) = preprocess_bwt(bwt);
    
    let mut results = Vec::new();
    
    for pattern in patterns {
        let count = bw_matching(
            pattern,
            &first_column,
            &first_to_last,
            &count,
            &first_occurrence,
        );
        results.push(count);
    }
    
    results
}

fn preprocess_bwt(bwt: &str) -> (String, Vec<usize>, Vec<Vec<usize>>, Vec<usize>) {
    let mut bwt_chars: Vec<char> = bwt.chars().collect();
    
    // Create first column (sorted BWT)
    let mut first_column = bwt_chars.clone();
    first_column.sort_unstable();
    let first_column_str: String = first_column.iter().collect();
    
    // Create first_to_last mapping
    let mut first_to_last = vec![0; bwt_chars.len()];
    let mut char_positions: HashMap<char, Vec<usize>> = HashMap::new();
    
    // Group positions by character in BWT
    for (i, &ch) in bwt_chars.iter().enumerate() {
        char_positions.entry(ch).or_insert_with(Vec::new).push(i);
    }
    
    // Fill first_to_last mapping
    let mut first_positions: HashMap<char, usize> = HashMap::new();
    for (i, ch) in first_column.iter().enumerate() {
        let pos = first_positions.entry(*ch).or_insert(0);
        first_to_last[char_positions[ch].get(*pos).unwrap()] = i;
        *pos += 1;
    }
    
    // Create count array
    let mut count = vec![vec![0; 256]; bwt_chars.len() + 1];
    for i in 0..bwt_chars.len() {
        for j in 0..256 {
            count[i + 1][j] = count[i][j];
        }
        count[i + 1][bwt_chars[i] as usize] += 1;
    }
    
    // Create first occurrence array
    let mut first_occurrence = vec![0; 256];
    let mut seen = vec![false; 256];
    let mut first_pos = 0;
    
    for (i, &ch) in first_column.iter().enumerate() {
        if !seen[ch as usize] {
            first_occurrence[ch as usize] = i;
            seen[ch as usize] = true;
        }
    }
    
    (first_column_str, first_to_last, count, first_occurrence)
}

fn bw_matching(
    pattern: &str,
    first_column: &str,
    first_to_last: &[usize],
    count: &[Vec<usize>],
    first_occurrence: &[usize],
) -> usize {
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let mut top = 0;
    let mut bottom = first_column.len() - 1;
    
    // Process pattern from right to left
    for i in (0..pattern_chars.len()).rev() {
        let symbol = pattern_chars[i];
        
        // Check if symbol exists in first_column
        if first_occurrence[symbol as usize] == 0 && 
           count[bottom + 1][symbol as usize] == count[top][symbol as usize] {
            return 0;
        }
        
        // Update top and bottom
        top = first_occurrence[symbol as usize] + count[top][symbol as usize];
        bottom = first_occurrence[symbol as usize] + count[bottom + 1][symbol as usize] - 1;
    }
    
    bottom - top + 1
}

// Alternative cleaner implementation
fn better_bw_matching_v2(bwt: &str, patterns: &[&str]) -> Vec<usize> {
    let bwt_chars: Vec<char> = bwt.chars().collect();
    let n = bwt_chars.len();
    
    // Create first column (sorted BWT)
    let mut first_column = bwt_chars.clone();
    first_column.sort_unstable();
    
    // Create first_to_last mapping
    let mut first_to_last = vec![0; n];
    let mut last_to_first = vec![0; n];
    
    // Build mapping from last to first column
    let mut char_count = HashMap::new();
    let mut char_positions = HashMap::new();
    
    // Count characters and store positions
    for (i, &ch) in bwt_chars.iter().enumerate() {
        *char_count.entry(ch).or_insert(0) += 1;
        char_positions.entry(ch).or_insert_with(Vec::new).push(i);
    }
    
    // Build first_to_last mapping
    let mut position_in_first = HashMap::new();
    let mut first_pos = 0;
    
    for ch in first_column.iter() {
        if !position_in_first.contains_key(ch) {
            position_in_first.insert(ch, first_pos);
            first_pos += 1;
        }
    }
    
    for (i, &ch) in bwt_chars.iter().enumerate() {
        let pos = char_positions.get(&ch).unwrap();
        let idx = pos.iter().position(|&x| x == i).unwrap();
        first_to_last[i] = position_in_first[&ch] + idx;
    }
    
    // Build last_to_first mapping (reverse of first_to_last)
    for i in 0..n {
        last_to_first[first_to_last[i]] = i;
    }
    
    // Create count matrix
    let mut count = vec![vec![0; 256]; n + 1];
    for i in 0..n {
        for j in 0..256 {
            count[i + 1][j] = count[i][j];
        }
        count[i + 1][bwt_chars[i] as usize] += 1;
    }
    
    // Create first occurrence array
    let mut first_occurrence = vec![0; 256];
    let mut seen = vec![false; 256];
    
    for (i, &ch) in first_column.iter().enumerate() {
        if !seen[ch as usize] {
            first_occurrence[ch as usize] = i;
            seen[ch as usize] = true;
        }
    }
    
    // Process each pattern
    let mut results = Vec::new();
    for pattern in patterns {
        let count = bw_matching_v2(
            pattern,
            &first_column,
            &first_to_last,
            &count,
            &first_occurrence,
        );
        results.push(count);
    }
    
    results
}

fn bw_matching_v2(
    pattern: &str,
    first_column: &[char],
    first_to_last: &[usize],
    count: &[Vec<usize>],
    first_occurrence: &[usize],
) -> usize {
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let mut top = 0;
    let mut bottom = first_column.len() - 1;
    
    for i in (0..pattern_chars.len()).rev() {
        let symbol = pattern_chars[i];
        
        // Check if symbol exists in the BWT
        if first_occurrence[symbol as usize] == 0 && 
           count[bottom + 1][symbol as usize] == count[top][symbol as usize] {
            return 0;
        }
        
        // Update top and bottom boundaries
        let top_count = count[top][symbol as usize];
        let bottom_count = count[bottom + 1][symbol as usize];
        
        top = first_occurrence[symbol as usize] + top_count;
        bottom = first_occurrence[symbol as usize] + bottom_count - 1;
    }
    
    if bottom >= top {
        bottom - top + 1
    } else {
        0
    }
}

// Main function for testing
fn main() {
    // Example usage
    let bwt = "TCCTCTATGAGATCCTGAAGTATTTGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATGCTGCTGGATTATTGCAGAGGCATGAGAGGATGAGAGGATAGATG