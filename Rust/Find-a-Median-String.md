# Rosalind Problem: Find_a_Median_String Solution in Rust

## Problem Understanding

The problem asks us to find a median string for a collection of DNA strings. A median string is a k-mer that minimizes the total Hamming distance to all strings in the collection.

## Solution Approach

1. Generate all possible k-mers of length k from the DNA alphabet
2. For each k-mer, calculate the total Hamming distance to all input strings
3. Return the k-mer with the minimum total distance

## Rust Implementation

```rust
use std::collections::HashMap;

fn hamming_distance(s1: &str, s2: &str) -> usize {
    s1.chars()
        .zip(s2.chars())
        .filter(|(c1, c2)| c1 != c2)
        .count()
}

fn get_all_kmers(k: usize) -> Vec<String> {
    let mut kmers = Vec::new();
    let nucleotides = ['A', 'C', 'G', 'T'];
    
    fn generate_kmers(
        current: String,
        k: usize,
        nucleotides: &[char],
        result: &mut Vec<String>,
    ) {
        if current.len() == k {
            result.push(current);
            return;
        }
        
        for &nucleotide in nucleotides {
            let mut new_current = current.clone();
            new_current.push(nucleotide);
            generate_kmers(new_current, k, nucleotides, result);
        }
    }
    
    generate_kmers(String::new(), k, &nucleotides, &mut kmers);
    kmers
}

fn median_string(dna_strings: &[String], k: usize) -> String {
    let kmers = get_all_kmers(k);
    let mut min_distance = usize::MAX;
    let mut median = String::new();
    
    for kmer in kmers {
        let mut total_distance = 0;
        
        for dna in dna_strings {
            let mut min_hamming = usize::MAX;
            
            // Find minimum Hamming distance between kmer and all k-length substrings of dna
            for i in 0..=dna.len().saturating_sub(k) {
                let substring = &dna[i..i + k];
                let distance = hamming_distance(&kmer, substring);
                min_hamming = min_hamming.min(distance);
            }
            
            total_distance += min_hamming;
        }
        
        if total_distance < min_distance {
            min_distance = total_distance;
            median = kmer;
        }
    }
    
    median
}

fn main() {
    // Example usage
    let dna_strings = vec![
        "AAATTGACGCAT".to_string(),
        "GACGACCACGTT".to_string(),
        "CGTCAGCGCCTG".to_string(),
        "GCTGAGCACCGG".to_string(),
        "AGTACGGGACAG".to_string(),
    ];
    
    let k = 3;
    let result = median_string(&dna_strings, k);
    println!("Median string: {}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hamming_distance() {
        assert_eq!(hamming_distance("GATTACA", "GACTATA"), 2);
        assert_eq!(hamming_distance("AAAA", "AAAA"), 0);
        assert_eq!(hamming_distance("AAAA", "TTTT"), 4);
    }

    #[test]
    fn test_get_all_kmers() {
        let kmers = get_all_kmers(2);
        assert_eq!(kmers.len(), 16); // 4^2 = 16
        assert!(kmers.contains(&"AA".to_string()));
        assert!(kmers.contains(&"TT".to_string()));
    }
}
```

## Explanation

### Key Functions:

1. **`hamming_distance`**: Calculates the Hamming distance between two strings of equal length
2. **`get_all_kmers`**: Generates all possible k-mers of length k using recursive backtracking
3. **`median_string`**: Main function that finds the median string by:
   - Generating all possible k-mers
   - For each k-mer, calculating the minimum Hamming distance to all input strings
   - Tracking the k-mer with the minimum total distance

### Algorithm Steps:

1. Generate all possible k-mers of length k (4^k total possibilities)
2. For each k-mer, find the minimum Hamming distance to any k-length substring in each DNA string
3. Sum these minimum distances for each k-mer
4. Return the k-mer with the smallest total distance

### Time Complexity:
- O(4^k × n × m × k) where n is the number of DNA strings, m is the average length of strings, and k is the k-mer length

### Space Complexity:
- O(4^k) for storing all k-mers

This solution handles the core requirements of finding a median string that minimizes the total Hamming distance to all input DNA strings.

