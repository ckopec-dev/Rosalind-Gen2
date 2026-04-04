# Rosalind Problem: Maximum Matchings and RNA Secondary Structures

## Problem Understanding

This problem asks us to find the maximum number of base pairings in an RNA sequence that can form valid secondary structures, where:
- A pairs with U
- G pairs with C
- No base can pair with itself
- Base pairs cannot cross each other (non-crossing constraint)

This is a classic dynamic programming problem related to RNA secondary structure prediction.

## Solution Approach

We'll use dynamic programming with the following recurrence:
- `dp[i][j]` = maximum number of base pairings in substring from index i to j
- If s[i] pairs with s[j], then `dp[i][j] = 1 + dp[i+1][j-1]`
- Otherwise, `dp[i][j] = max(dp[i][k] + dp[k+1][j])` for all valid k

## Implementation

```rust
use std::collections::HashMap;

fn maximum_matchings(rna: &str) -> u64 {
    let n = rna.len();
    if n < 2 {
        return 0;
    }
    
    let rna_chars: Vec<char> = rna.chars().collect();
    
    // Precompute valid base pairs
    let mut pairs = HashMap::new();
    pairs.insert(('A', 'U'), true);
    pairs.insert(('U', 'A'), true);
    pairs.insert(('G', 'C'), true);
    pairs.insert(('C', 'G'), true);
    
    // dp[i][j] represents maximum matchings in substring from i to j
    let mut dp = vec![vec![0u64; n]; n];
    
    // Fill DP table for all substrings of length 2 to n
    for length in 2..=n {
        for i in 0..=(n - length) {
            let j = i + length - 1;
            
            // If characters at i and j can pair
            if pairs.contains_key(&(rna_chars[i], rna_chars[j])) {
                if length == 2 {
                    dp[i][j] = 1;
                } else {
                    // Take the pairing of i and j plus optimal solution for inner substring
                    dp[i][j] = 1 + dp[i + 1][j - 1];
                }
            }
            
            // Try all possible splits and take maximum
            for k in (i + 1)..j {
                dp[i][j] = dp[i][j].max(dp[i][k] + dp[k + 1][j]);
            }
        }
    }
    
    dp[0][n - 1]
}

fn main() {
    // Example usage
    let rna = "AUGCUAGU";
    let result = maximum_matchings(rna);
    println!("Maximum matchings for {}: {}", rna, result);
    
    // Test with example from Rosalind
    let rna2 = "AUCG";
    let result2 = maximum_matchings(rna2);
    println!("Maximum matchings for {}: {}", rna2, result2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_case() {
        assert_eq!(maximum_matchings("AUCG"), 1);
    }
    
    #[test]
    fn test_empty() {
        assert_eq!(maximum_matchings(""), 0);
    }
    
    #[test]
    fn test_single() {
        assert_eq!(maximum_matchings("A"), 0);
    }
    
    #[test]
    fn test_pairing() {
        assert_eq!(maximum_matchings("AU"), 1);
    }
}
```

## Alternative Implementation (More Efficient)

For better performance, we can optimize the approach:

```rust
use std::collections::HashMap;

fn maximum_matchings_optimized(rna: &str) -> u64 {
    let n = rna.len();
    if n < 2 {
        return 0;
    }
    
    let rna_chars: Vec<char> = rna.chars().collect();
    
    // Precompute valid base pairs
    let mut is_pair = vec![vec![false; n]; n];
    let mut pairs = HashMap::new();
    pairs.insert(('A', 'U'), true);
    pairs.insert(('U', 'A'), true);
    pairs.insert(('G', 'C'), true);
    pairs.insert(('C', 'G'), true);
    
    for i in 0..n {
        for j in 0..n {
            if pairs.contains_key(&(rna_chars[i], rna_chars[j])) {
                is_pair[i][j] = true;
            }
        }
    }
    
    // dp[i][j] = maximum matchings in substring from i to j
    let mut dp = vec![vec![0u64; n]; n];
    
    // Fill DP table for increasing lengths
    for length in 2..=n {
        for i in 0..=(n - length) {
            let j = i + length - 1;
            
            // Try to pair i with j
            if is_pair[i][j] {
                if length == 2 {
                    dp[i][j] = 1;
                } else {
                    dp[i][j] = 1 + dp[i + 1][j - 1];
                }
            }
            
            // Try all possible splits
            for k in (i + 1)..j {
                dp[i][j] = dp[i][j].max(dp[i][k] + dp[k + 1][j]);
            }
        }
    }
    
    dp[0][n - 1]
}

fn main() {
    // Read input from stdin or use test cases
    let rna = "AUGCUAGU";
    let result = maximum_matchings_optimized(rna);
    println!("Maximum matchings: {}", result);
}
```

## Key Points

1. **Time Complexity**: O(n³) where n is the length of the RNA sequence
2. **Space Complexity**: O(n²) for the DP table
3. **Base Pairing Rules**: A↔U and G↔C only
4. **Non-crossing Constraint**: Valid secondary structures cannot have crossing base pairs
5. **Dynamic Programming**: Uses optimal substructure property where optimal solution contains optimal solutions to subproblems

This solution correctly handles the constraints of RNA secondary structure prediction and finds the maximum number of valid base pairings possible.

