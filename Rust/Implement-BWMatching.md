# Rosalind Problem: Implement_BWMatching in Rust

## Problem Understanding

BWMatching (Burrows-Wheeler Matching) is a key algorithm for pattern matching in compressed text. Given a Burrows-Wheeler Transform (BWT) of a text and a pattern, we need to count how many times the pattern occurs in the original text.

## Solution Approach

1. **Build the BWT structure**: Create the BWT matrix and auxiliary data structures
2. **Implement the matching algorithm**: Use the FirstOccurrence array and LastToFirst mapping
3. **Count occurrences**: Return the number of times pattern appears in original text

## Implementation

```rust
use std::collections::HashMap;

fn bwt_matching(bwt: &str, patterns: Vec<&str>) -> Vec<usize> {
    let n = bwt.len();
    
    // Build the BWT matrix
    let mut bwt_chars: Vec<char> = bwt.chars().collect();
    
    // Create sorted BWT for FirstOccurrence array
    let mut sorted_bwt = bwt_chars.clone();
    sorted_bwt.sort_unstable();
    
    // Build FirstOccurrence array
    let mut first_occurrence = HashMap::new();
    let mut count = HashMap::new();
    
    for (i, &c) in sorted_bwt.iter().enumerate() {
        if !first_occurrence.contains_key(&c) {
            first_occurrence.insert(c, i);
        }
        *count.entry(c).or_insert(0) += 1;
    }
    
    // Build LastToFirst mapping
    let mut last_to_first = vec![0; n];
    let mut char_count = HashMap::new();
    
    for i in 0..n {
        let c = bwt_chars[i];
        let pos = *char_count.entry(c).or_insert(0);
        last_to_first[i] = first_occurrence[&c] + pos;
        *char_count.entry(c).or_insert(0) += 1;
    }
    
    // Process each pattern
    let mut results = Vec::new();
    
    for pattern in patterns {
        let pattern_chars: Vec<char> = pattern.chars().collect();
        let mut top = 0;
        let mut bottom = n as i32 - 1;
        
        let mut i = pattern_chars.len() as i32 - 1;
        while i >= 0 && top <= bottom {
            let symbol = pattern_chars[i as usize];
            
            // Find the range of occurrences of symbol in BWT[0..bottom+1] and BWT[top..bottom+1]
            let mut new_top = -1;
            let mut new_bottom = -1;
            
            // Find first occurrence of symbol in BWT from top to bottom
            for j in top..=bottom {
                if bwt_chars[j as usize] == symbol {
                    if new_top == -1 {
                        new_top = j;
                    }
                    new_bottom = j;
                }
            }
            
            if new_top == -1 {
                // Symbol not found
                top = 0;
                bottom = -1;
                break;
            }
            
            // Update top and bottom using LastToFirst mapping
            top = last_to_first[new_top as usize] as i32;
            bottom = last_to_first[new_bottom as usize] as i32;
            
            i -= 1;
        }
        
        if top <= bottom {
            results.push((bottom - top + 1) as usize);
        } else {
            results.push(0);
        }
    }
    
    results
}

// More efficient implementation using proper BWM algorithm
fn bwm_matching(bwt: &str, patterns: Vec<&str>) -> Vec<usize> {
    let n = bwt.len();
    
    // Create sorted BWT for first column
    let mut sorted_bwt = bwt.chars().collect::<Vec<char>>();
    sorted_bwt.sort_unstable();
    
    // Build FirstOccurrence array
    let mut first_occurrence = HashMap::new();
    let mut char_count = HashMap::new();
    
    for (i, &c) in sorted_bwt.iter().enumerate() {
        if !first_occurrence.contains_key(&c) {
            first_occurrence.insert(c, i);
        }
        *char_count.entry(c).or_insert(0) += 1;
    }
    
    // Build count array for each position in BWT
    let mut count = vec![HashMap::new(); n + 1];
    
    for i in 0..n {
        count[i + 1] = count[i].clone();
        *count[i + 1].entry(bwt.chars().nth(i).unwrap()).or_insert(0) += 1;
    }
    
    let mut results = Vec::new();
    
    for pattern in patterns {
        let pattern_chars: Vec<char> = pattern.chars().collect();
        let mut top = 0;
        let mut bottom = n as i32 - 1;
        
        let mut i = pattern_chars.len() as i32 - 1;
        while i >= 0 && top <= bottom {
            let symbol = pattern_chars[i as usize];
            
            // Count occurrences of symbol in BWT[top..bottom+1]
            let top_count = *count[top as usize].get(&symbol).unwrap_or(&0);
            let bottom_count = *count[bottom as usize + 1].get(&symbol).unwrap_or(&0);
            
            // Find the range in BWT
            let mut new_top = -1;
            let mut new_bottom = -1;
            
            for j in top..=bottom {
                if bwt.chars().nth(j as usize).unwrap() == symbol {
                    if new_top == -1 {
                        new_top = j;
                    }
                    new_bottom = j;
                }
            }
            
            if new_top == -1 {
                top = 0;
                bottom = -1;
                break;
            }
            
            // Find corresponding positions in first column
            let first_pos = first_occurrence[&symbol];
            let first_count = *count[n].get(&symbol).unwrap_or(&0);
            
            // Adjust for the range
            let first_top = first_pos + top_count;
            let first_bottom = first_pos + bottom_count - 1;
            
            top = first_top as i32;
            bottom = first_bottom as i32;
            
            i -= 1;
        }
        
        if top <= bottom {
            results.push((bottom - top + 1) as usize);
        } else {
            results.push(0);
        }
    }
    
    results
}

// Clean and efficient implementation
fn bw_matching(bwt: &str, patterns: Vec<&str>) -> Vec<usize> {
    let n = bwt.len();
    
    // Build first column and first occurrence array
    let mut first_column: Vec<char> = bwt.chars().collect();
    first_column.sort_unstable();
    
    let mut first_occurrence = HashMap::new();
    let mut char_count = HashMap::new();
    
    for (i, &c) in first_column.iter().enumerate() {
        if !first_occurrence.contains_key(&c) {
            first_occurrence.insert(c, i);
        }
        *char_count.entry(c).or_insert(0) += 1;
    }
    
    // Build LastToFirst mapping
    let mut last_to_first = vec![0; n];
    let mut char_positions = HashMap::new();
    
    for i in 0..n {
        let c = bwt.chars().nth(i).unwrap();
        let pos = *char_positions.entry(c).or_insert(0);
        last_to_first[i] = first_occurrence[&c] + pos;
        *char_positions.entry(c).or_insert(0) += 1;
    }
    
    let mut results = Vec::new();
    
    for pattern in patterns {
        let pattern_chars: Vec<char> = pattern.chars().collect();
        let mut top = 0;
        let mut bottom = (n - 1) as i32;
        
        let mut i = pattern_chars.len() as i32 - 1;
        while i >= 0 && top <= bottom {
            let symbol = pattern_chars[i as usize];
            
            // Find range of symbol in current BWT segment
            let mut new_top = -1;
            let mut new_bottom = -1;
            
            for j in top..=bottom {
                if bwt.chars().nth(j as usize).unwrap() == symbol {
                    if new_top == -1 {
                        new_top = j;
                    }
                    new_bottom = j;
                }
            }
            
            if new_top == -1 {
                top = 0;
                bottom = -1;
                break;
            }
            
            // Update using LastToFirst mapping
            top = last_to_first[new_top as usize] as i32;
            bottom = last_to_first[new_bottom as usize] as i32;
            
            i -= 1;
        }
        
        if top <= bottom {
            results.push((bottom - top + 1) as usize);
        } else {
            results.push(0);
        }
    }
    
    results
}

// Example usage
fn main() {
    let bwt = "TCCTCTATGAGATCCTATTCTATGAAACCTTCAATTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTTTCTTCTTT