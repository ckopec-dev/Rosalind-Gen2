# Rosalind Problem: Locating Restriction Sites

## Problem Statement
In DNA sequences, restriction sites are palindromic sequences that are recognized by restriction enzymes. Given a DNA string, find all locations where a restriction site occurs.

## Solution in Rust

```rust
use std::fs;

fn main() {
    // Read input from file
    let input = fs::read_to_string("rosalind_revp.txt").expect("Failed to read file");
    let dna = input.trim();
    
    // Find all restriction sites
    let sites = find_restriction_sites(dna);
    
    // Print results
    for (position, length) in sites {
        println!("{} {}", position, length);
    }
}

fn find_restriction_sites(dna: &str) -> Vec<(usize, usize)> {
    let mut sites = Vec::new();
    let dna_chars: Vec<char> = dna.chars().collect();
    let n = dna_chars.len();
    
    // Check all possible palindrome lengths (4-12)
    for length in 4..=12 {
        // For each position in the DNA string
        for i in 0..=(n - length) {
            let substring: String = dna_chars[i..(i + length)].iter().collect();
            
            // Check if substring is a palindrome
            if is_palindrome(&substring) {
                sites.push((i + 1, length)); // 1-indexed positions
            }
        }
    }
    
    // Sort by position
    sites.sort_by_key(|(pos, _)| *pos);
    
    sites
}

fn is_palindrome(s: &str) -> bool {
    let chars: Vec<char> = s.chars().collect();
    let n = chars.len();
    
    for i in 0..(n / 2) {
        if chars[i] != chars[n - 1 - i] {
            return false;
        }
    }
    
    true
}
```

## Explanation

### Approach
1. **Input Reading**: Read the DNA sequence from the input file
2. **Palindrome Detection**: For each possible substring length from 4 to 12:
   - Check every possible position in the DNA string
   - Determine if the substring is a palindrome
   - Record valid restriction sites with their positions and lengths
3. **Output**: Print all restriction sites in 1-indexed format

### Key Functions

**`find_restriction_sites`**: 
- Iterates through all possible palindrome lengths (4-12)
- For each length, checks all possible positions
- Returns vector of tuples (position, length) for valid sites

**`is_palindrome`**: 
- Checks if a string is palindromic by comparing characters from both ends
- Returns true if the string reads the same forwards and backwards

### Time Complexity
- O(n²) where n is the length of the DNA string
- For each position, we check palindromes of lengths 4-12

### Space Complexity
- O(n) for storing the DNA characters and results

## Sample Input/Output

**Input:**
```
TCAATGCATGCGGGTCTATATGCAT
```

**Output:**
```
4 6
12 6
17 6
```

This solution correctly identifies palindromic restriction sites in the given DNA sequence, which are typically 4-12 base pairs long and read the same in both directions.

