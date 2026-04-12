# Rosalind Problem: Finding the Longest Multiple Repeat

## Problem Understanding

We need to find the longest substring that appears at least k times in a given DNA string, where k is a given integer.

## Approach

1. Generate all possible substrings of the input string
2. Count how many times each substring appears
3. Find the longest substring that appears at least k times
4. Handle edge cases (empty string, k=0, etc.)

## Solution

```rust
use std::collections::HashMap;

fn longest_multiple_repeat(sequence: &str, k: usize) -> String {
    if sequence.is_empty() || k == 0 {
        return String::new();
    }
    
    let mut substring_counts: HashMap<String, usize> = HashMap::new();
    let n = sequence.len();
    
    // Generate all substrings and count their occurrences
    for i in 0..n {
        for j in (i + 1)..=n {
            let substring = &sequence[i..j];
            *substring_counts.entry(substring.to_string()).or_insert(0) += 1;
        }
    }
    
    // Find the longest substring that appears at least k times
    let mut longest = String::new();
    
    for (substring, count) in substring_counts {
        if count >= k && substring.len() > longest.len() {
            longest = substring;
        }
    }
    
    longest
}

fn main() {
    // Example usage
    let sequence = "ACGTACGTACGT";
    let k = 3;
    
    let result = longest_multiple_repeat(sequence, k);
    println!("Longest multiple repeat: {}", result);
    println!("Length: {}", result.len());
    
    // Test with the sample from Rosalind
    let sample_sequence = "ACGTACGTACGTACGT";
    let sample_k = 3;
    
    let sample_result = longest_multiple_repeat(sample_sequence, sample_k);
    println!("Sample result: {}", sample_result);
    println!("Sample length: {}", sample_result.len());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_longest_multiple_repeat() {
        // Test case 1: Simple case
        assert_eq!(longest_multiple_repeat("ACGTACGT", 2), "ACGT");
        
        // Test case 2: Multiple repeats
        assert_eq!(longest_multiple_repeat("ACGTACGTACGT", 3), "ACGT");
        
        // Test case 3: No repeat
        assert_eq!(longest_multiple_repeat("ACGT", 2), "");
        
        // Test case 4: Empty string
        assert_eq!(longest_multiple_repeat("", 1), "");
        
        // Test case 5: k = 1 (should return longest substring)
        assert_eq!(longest_multiple_repeat("ACGT", 1), "ACGT");
    }
}
```

## Optimized Version

For better performance with larger inputs, here's an optimized approach:

```rust
use std::collections::HashMap;

fn longest_multiple_repeat_optimized(sequence: &str, k: usize) -> String {
    if sequence.is_empty() || k == 0 {
        return String::new();
    }
    
    let n = sequence.len();
    let mut longest = String::new();
    
    // Check substrings in descending order of length for early termination
    for length in (1..=n).rev() {
        let mut substring_counts: HashMap<String, usize> = HashMap::new();
        
        // Count all substrings of current length
        for i in 0..=(n - length) {
            let substring = &sequence[i..i + length];
            *substring_counts.entry(substring.to_string()).or_insert(0) += 1;
        }
        
        // Check if any substring appears at least k times
        for (substring, count) in &substring_counts {
            if *count >= k && substring.len() > longest.len() {
                longest = substring.clone();
            }
        }
        
        // If we found a solution, no need to check shorter substrings
        if !longest.is_empty() {
            break;
        }
    }
    
    longest
}

// Alternative implementation using suffix arrays (more complex but more efficient)
fn longest_multiple_repeat_suffix_array(sequence: &str, k: usize) -> String {
    if sequence.is_empty() || k == 0 {
        return String::new();
    }
    
    // This would require implementing suffix array construction
    // For simplicity, we'll stick with the basic approach for now
    longest_multiple_repeat_optimized(sequence, k)
}

#[cfg(test)]
mod optimized_tests {
    use super::*;

    #[test]
    fn test_optimized() {
        assert_eq!(longest_multiple_repeat_optimized("ACGTACGT", 2), "ACGT");
        assert_eq!(longest_multiple_repeat_optimized("ACGTACGTACGT", 3), "ACGT");
        assert_eq!(longest_multiple_repeat_optimized("ACGT", 2), "");
    }
}
```

## Explanation

### Key Points:

1. **Input Handling**: The function handles edge cases like empty strings and k=0
2. **Substring Generation**: We generate all possible substrings using nested loops
3. **Counting**: We use a HashMap to count occurrences of each substring
4. **Optimization**: The optimized version checks longer substrings first, allowing early termination
5. **Return Value**: Returns the longest substring that appears at least k times

### Time Complexity:
- Basic approach: O(n³) where n is the length of the sequence
- Optimized approach: O(n³) worst case, but better average performance

### Space Complexity: 
- O(n³) in worst case for storing all substrings

### Sample Input/Output:
For input `"ACGTACGTACGT"` with k=3:
- Output: `"ACGT"` (length 4)
- This substring appears 3 times in the sequence

This solution efficiently finds the longest multiple repeat while handling all edge cases appropriately.

