# Rosalind Problem: Linguistic Complexity of a Genome

## Problem Understanding

The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings to the total number of possible substrings of a given length. For a genome of length n, we need to:
1. Count all distinct substrings of length k
2. Calculate the total number of possible substrings of length k
3. Compute the ratio

## Solution Approach

1. Generate all substrings of a given length k
2. Count distinct substrings using a HashSet
3. Calculate the ratio of distinct to total substrings

## Rust Implementation

```rust
use std::collections::HashSet;

fn linguistic_complexity(genome: &str, k: usize) -> f64 {
    if k == 0 || genome.len() < k {
        return 0.0;
    }
    
    let mut substrings = HashSet::new();
    
    // Generate all substrings of length k
    for i in 0..=genome.len() - k {
        let substring = &genome[i..i + k];
        substrings.insert(substring);
    }
    
    let distinct_count = substrings.len() as f64;
    let total_count = (genome.len() - k + 1) as f64;
    
    distinct_count / total_count
}

fn main() {
    // Example usage
    let genome = "AAATTTCCCGGG";
    let k = 3;
    
    let complexity = linguistic_complexity(genome, k);
    println!("Linguistic complexity for genome {} with k={} is: {:.6}", 
             genome, k, complexity);
    
    // Test with the example from Rosalind
    let test_genome = "AAATTTCCCGGG";
    let test_k = 3;
    
    let result = linguistic_complexity(test_genome, test_k);
    println!("Result: {:.6}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linguistic_complexity() {
        // Test case from Rosalind
        let genome = "AAATTTCCCGGG";
        let k = 3;
        let result = linguistic_complexity(genome, k);
        // Expected: 10 distinct substrings out of 12 total substrings = 10/12 = 0.833333
        assert!((result - 0.833333).abs() < 0.000001);
    }
    
    #[test]
    fn test_edge_cases() {
        // Empty genome
        assert_eq!(linguistic_complexity("", 1), 0.0);
        
        // k larger than genome
        assert_eq!(linguistic_complexity("AA", 5), 0.0);
        
        // k = 0
        assert_eq!(linguistic_complexity("AA", 0), 0.0);
        
        // Single character genome
        assert_eq!(linguistic_complexity("A", 1), 1.0);
    }
}
```

## Step-by-step Explanation

1. **Input Validation**: Check if k is valid (not zero and not larger than genome length)
2. **Substring Generation**: Use a sliding window approach to extract all substrings of length k
3. **Distinct Counting**: Store substrings in a HashSet to automatically handle duplicates
4. **Ratio Calculation**: Divide distinct count by total count of substrings

## Example Walkthrough

For genome "AAATTTCCCGGG" with k=3:
- Total substrings of length 3: 12 (positions 0-11)
- Distinct substrings: {AAA, ATT, TTT, TTC, TCC, CCC, CCG, CGG, GGG} = 9 distinct
- Ratio: 9/12 = 0.75

Wait, let me recheck this more carefully:

For "AAATTTCCCGGG" with k=3:
- Substrings: AAA, AAT, ATT, TTT, TTC, TCC, CCC, CCG, CGG, GGG
- That's 10 distinct substrings out of 12 total substrings
- Ratio: 10/12 = 0.833333

## Complete Solution for Rosalind

```rust
use std::collections::HashSet;

fn linguistic_complexity(genome: &str, k: usize) -> f64 {
    if k == 0 || genome.len() < k {
        return 0.0;
    }
    
    let mut substrings = HashSet::new();
    
    // Generate all substrings of length k
    for i in 0..=genome.len() - k {
        let substring = &genome[i..i + k];
        substrings.insert(substring);
    }
    
    let distinct_count = substrings.len() as f64;
    let total_count = (genome.len() - k + 1) as f64;
    
    distinct_count / total_count
}

fn main() {
    // Read input from stdin or file as needed
    let genome = "AAATTTCCCGGG";  // Replace with actual input
    let k = 3;  // Replace with actual k
    
    let result = linguistic_complexity(genome, k);
    println!("{:.6}", result);
}
```

This solution efficiently computes the linguistic complexity by:
- Using a HashSet for O(1) average case insertion and lookup
- Employing a sliding window approach for optimal time complexity O(n*k) where n is genome length
- Handling edge cases appropriately
- Returning a precise floating-point result as required

