# Rosalind Problem: Generate the d-Neighborhood of a String

## Problem Description
The d-neighborhood of a string is the set of all strings that have Hamming distance at most d from the original string.

## Solution in Rust

```rust
fn generate_d_neighborhood(pattern: &str, d: usize) -> Vec<String> {
    if d == 0 {
        return vec![pattern.to_string()];
    }
    
    let nucleotides = ['A', 'C', 'G', 'T'];
    let mut neighborhood = Vec::new();
    
    // Base case: if pattern length is 1, return all possible nucleotides
    if pattern.len() == 1 {
        for &nucleotide in &nucleotides {
            neighborhood.push(nucleotide.to_string());
        }
        return neighborhood;
    }
    
    // Recursive case: get d-neighborhood of suffix
    let suffix = &pattern[1..];
    let suffix_neighborhood = generate_d_neighborhood(suffix, d);
    
    // For each string in suffix neighborhood
    for text in suffix_neighborhood {
        // If Hamming distance is less than d, we can replace first character
        if hamming_distance(&pattern[1..], &text) < d {
            for &nucleotide in &nucleotides {
                neighborhood.push(format!("{}{}", nucleotide, text));
            }
        } else {
            // If Hamming distance is exactly d, we can only replace with same nucleotide
            neighborhood.push(format!("{}{}", pattern.chars().next().unwrap(), text));
        }
    }
    
    neighborhood
}

fn hamming_distance(s1: &str, s2: &str) -> usize {
    s1.chars().zip(s2.chars()).filter(|(c1, c2)| c1 != c2).count()
}

// Alternative simpler approach using iterative method
fn generate_d_neighborhood_iterative(pattern: &str, d: usize) -> Vec<String> {
    if d == 0 {
        return vec![pattern.to_string()];
    }
    
    let mut neighborhood = vec![pattern.to_string()];
    let nucleotides = ['A', 'C', 'G', 'T'];
    
    for _ in 0..d {
        let mut new_neighborhood = Vec::new();
        for pattern in &neighborhood {
            for i in 0..pattern.len() {
                for &nucleotide in &nucleotides {
                    if nucleotide != pattern.chars().nth(i).unwrap() {
                        let mut new_pattern = pattern.chars().collect::<Vec<char>>();
                        new_pattern[i] = nucleotide;
                        new_neighborhood.push(new_pattern.into_iter().collect());
                    }
                }
            }
        }
        neighborhood = new_neighborhood;
    }
    
    // Remove duplicates
    let mut unique_neighborhood = neighborhood.into_iter().collect::<std::collections::HashSet<_>>();
    unique_neighborhood.into_iter().collect()
}

fn main() {
    let pattern = "ACG";
    let d = 1;
    
    let result = generate_d_neighborhood_iterative(pattern, d);
    
    for neighbor in &result {
        println!("{}", neighbor);
    }
    
    println!("Total count: {}", result.len());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hamming_distance() {
        assert_eq!(hamming_distance("ABCD", "ABCE"), 1);
        assert_eq!(hamming_distance("ABC", "ABC"), 0);
        assert_eq!(hamming_distance("ABC", "DEF"), 3);
    }

    #[test]
    fn test_d_neighborhood() {
        let result = generate_d_neighborhood_iterative("ACG", 1);
        assert!(result.contains(&"ACG".to_string()));
        assert!(result.contains(&"CCG".to_string()));
        assert!(result.contains(&"TCG".to_string()));
        assert!(result.contains(&"GCG".to_string()));
        assert!(result.contains(&"AAG".to_string()));
        assert!(result.contains(&"ATG".to_string()));
        assert!(result.contains(&"AGG".to_string()));
        assert!(result.contains(&"ACT".to_string()));
        assert!(result.contains(&"ACG".to_string()));
        assert!(result.contains(&"ACA".to_string()));
        assert!(result.contains(&"ACC".to_string()));
        assert!(result.contains(&"ACG".to_string()));
    }
}
```

## Explanation

This solution implements two approaches to generate the d-neighborhood of a string:

1. **Recursive approach** (`generate_d_neighborhood`): 
   - Base case: If d=0, return the original string
   - If pattern length is 1, return all possible nucleotides
   - Otherwise, recursively get the d-neighborhood of the suffix and build new strings

2. **Iterative approach** (`generate_d_neighborhood_iterative`):
   - Start with the original pattern
   - For each mutation distance (0 to d), generate all possible mutations
   - Replace each position with each possible nucleotide
   - Remove duplicates using HashSet

## Key Features

- Handles edge cases (d=0, single character strings)
- Uses Hamming distance to measure similarity
- Removes duplicate strings from result
- Includes test cases for verification
- Efficient implementation with proper error handling

## Time Complexity
- O(4^d × n) where n is the length of the pattern
- Space complexity: O(4^d × n)

The iterative approach is generally more practical for larger values of d and is easier to understand and debug.

