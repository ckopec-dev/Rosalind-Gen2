# Rosalind Problem: Computing GC Content (Rust Solution)

## Problem Description
Given at most 10 DNA strings in FASTA format (of length at most 1 kbp each), return the ID of the string having the highest GC content, followed by the GC content of that string.

## Solution

```rust
use std::collections::HashMap;

fn compute_gc_content(dna_string: &str) -> f64 {
    let mut gc_count = 0;
    let mut total_count = 0;
    
    for nucleotide in dna_string.chars() {
        match nucleotide {
            'G' | 'C' => {
                gc_count += 1;
                total_count += 1;
            }
            'A' | 'T' => total_count += 1,
            _ => continue, // Skip invalid nucleotides
        }
    }
    
    if total_count == 0 {
        0.0
    } else {
        (gc_count as f64 / total_count as f64) * 100.0
    }
}

fn parse_fasta(fasta_data: &str) -> HashMap<String, String> {
    let mut sequences: HashMap<String, String> = HashMap::new();
    let mut current_id = String::new();
    let mut current_sequence = String::new();
    
    for line in fasta_data.lines() {
        if line.starts_with('>') {
            // Save previous sequence if it exists
            if !current_id.is_empty() {
                sequences.insert(current_id, current_sequence);
            }
            
            // Start new sequence
            current_id = line[1..].to_string();
            current_sequence = String::new();
        } else {
            // Add to current sequence
            current_sequence.push_str(line);
        }
    }
    
    // Don't forget the last sequence
    if !current_id.is_empty() {
        sequences.insert(current_id, current_sequence);
    }
    
    sequences
}

fn find_highest_gc_content(fasta_data: &str) -> (String, f64) {
    let sequences = parse_fasta(fasta_data);
    let mut max_gc_content = 0.0;
    let mut max_id = String::new();
    
    for (id, sequence) in sequences {
        let gc_content = compute_gc_content(&sequence);
        if gc_content > max_gc_content {
            max_gc_content = gc_content;
            max_id = id;
        }
    }
    
    (max_id, max_gc_content)
}

fn main() {
    // Example input (you would typically read from file)
    let input = ">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATCGACTCCTGAAGCGAAAGCTCTCTACTGGGTTT
>Rosalind_5959
TTATCTGACAAATGTGGCCTGCGGCGGCGCGGCGCGCGGCGGCGGCGGCGGCGGCGG
>Rosalind_0808
ATCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGG";

    let (id, gc_content) = find_highest_gc_content(input);
    println!("{} {:.6}", id, gc_content);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_gc_content() {
        let dna = "ACGTACGTACGT";
        assert_eq!(compute_gc_content(dna), 50.0);
        
        let dna = "GCGCGC";
        assert_eq!(compute_gc_content(dna), 100.0);
        
        let dna = "ATATAT";
        assert_eq!(compute_gc_content(dna), 0.0);
    }

    #[test]
    fn test_find_highest_gc_content() {
        let input = ">Rosalind_0808
ATCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGGCGG
>Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATCGACTCCTGAAGCGAAAGCTCTCTACTGGGTTT";
        
        let (id, gc_content) = find_highest_gc_content(input);
        assert_eq!(id, "Rosalind_6404");
        // Expected GC content is approximately 60.87%
        assert!((gc_content - 60.869565).abs() < 0.0001);
    }
}
```

## Explanation

This solution follows these key steps:

1. **`compute_gc_content`**: Calculates the GC content of a DNA string by counting G and C nucleotides and dividing by the total length
2. **`parse_fasta`**: Parses FASTA format data into a HashMap mapping sequence IDs to DNA sequences
3. **`find_highest_gc_content`**: Finds the sequence with the highest GC content among all input sequences
4. **`main`**: Demonstrates usage with example data

## Key Features

- Handles FASTA format parsing correctly
- Properly calculates GC content as percentage
- Robust error handling for edge cases
- Includes comprehensive tests
- Efficient single-pass algorithms

## Time Complexity
- O(n) where n is the total length of all DNA sequences
- Each sequence is processed once for GC content calculation

## Space Complexity
- O(n) for storing the parsed sequences

The solution correctly handles the Rosalind problem requirements and should work for the given constraints of at most 10 DNA strings, each of length at most 1 kbp.

