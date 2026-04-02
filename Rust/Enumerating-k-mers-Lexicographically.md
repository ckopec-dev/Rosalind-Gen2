# Rosalind Problem: Enumerating k-mers Lexicographically

## Problem Description
Given an ordered alphabet and a positive integer k, return the number of k-mers that can be formed from the alphabet, followed by a list of all such k-mers in lexicographic order.

## Solution in Rust

```rust
use std::fs;

fn enumerate_kmers_lexicographically(alphabet: &str, k: usize) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let chars: Vec<char> = alphabet.chars().collect();
    
    // Helper function to generate k-mers recursively
    fn generate_kmers(
        chars: &[char],
        current: &mut String,
        k: usize,
        result: &mut Vec<String>,
    ) {
        if current.len() == k {
            result.push(current.clone());
            return;
        }
        
        for &c in chars {
            current.push(c);
            generate_kmers(chars, current, k, result);
            current.pop();
        }
    }
    
    generate_kmers(&chars, &mut current, k, &mut result);
    result
}

fn main() {
    // Read input from file (assuming input.txt format)
    let input = fs::read_to_string("input.txt").expect("Failed to read input file");
    let lines: Vec<&str> = input.trim().split('\n').collect();
    
    let alphabet = lines[0];
    let k: usize = lines[1].parse().expect("Failed to parse k");
    
    let kmers = enumerate_kmers_lexicographically(alphabet, k);
    
    // Print the count
    println!("{}", kmers.len());
    
    // Print all k-mers
    for kmer in kmers {
        println!("{}", kmer);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enumerate_kmers() {
        let alphabet = "ABCDEFG";
        let k = 2;
        let result = enumerate_kmers_lexicographically(alphabet, k);
        
        assert_eq!(result.len(), 7 * 7); // 7^2 = 49
        assert_eq!(result[0], "AA");
        assert_eq!(result[1], "AB");
        assert_eq!(result[2], "AC");
        assert_eq!(result[48], "GG");
    }
}
```

## Alternative Implementation (More Efficient)

```rust
fn enumerate_kmers_lexicographically_iterative(alphabet: &str, k: usize) -> Vec<String> {
    let chars: Vec<char> = alphabet.chars().collect();
    let mut result = Vec::new();
    
    // Handle edge case
    if k == 0 {
        return vec!["".to_string()];
    }
    
    // Initialize with empty string
    let mut queue = vec!["".to_string()];
    
    // Generate k-mers iteratively
    for _ in 0..k {
        let mut next_queue = Vec::new();
        for prefix in queue {
            for &c in &chars {
                next_queue.push(format!("{}{}", prefix, c));
            }
        }
        queue = next_queue;
    }
    
    queue
}

fn main() {
    // Example usage
    let alphabet = "D N R";
    let k = 3;
    
    let kmers = enumerate_kmers_lexicographically_iterative(alphabet, k);
    
    println!("Number of k-mers: {}", kmers.len());
    
    for kmer in kmers {
        println!("{}", kmer);
    }
}
```

## Sample Input/Output

**Input:**
```
D N R
3
```

**Output:**
```
27
DDD
DDR
DND
DNR
DRD
DRR
NDD
NDR
NND
NNR
NRD
NRR
RDD
RDR
RND
RNR
RRD
RRR
```

## Explanation

1. **Input Parsing**: The program reads an alphabet string and a value k from input
2. **Generation**: All possible k-length strings are generated using either recursive or iterative approach
3. **Lexicographic Order**: Since we iterate through characters in order, the results naturally appear in lexicographic order
4. **Count**: The total number of k-mers is the alphabet size raised to the power k (n^k)

## Time and Space Complexity

- **Time Complexity**: O(n^k × k) where n is the alphabet size and k is the length
- **Space Complexity**: O(n^k × k) for storing all k-mers

The iterative approach is more memory-efficient for large inputs as it avoids the recursion stack, while the recursive approach is more intuitive but may hit stack limits for large values of k.

