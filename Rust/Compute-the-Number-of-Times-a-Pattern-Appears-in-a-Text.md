# Rosalind Problem: Compute the Number of Times a Pattern Appears in a Text

## Problem Statement
Given two strings `pattern` and `text`, we need to count how many times `pattern` appears in `text` as a substring, including overlapping occurrences.

## Solution in Rust

```rust
fn pattern_count(text: &str, pattern: &str) -> usize {
    if pattern.is_empty() || text.is_empty() || pattern.len() > text.len() {
        return 0;
    }
    
    let mut count = 0;
    let text_chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();
    
    // Iterate through all possible starting positions
    for i in 0..=(text_chars.len() - pattern_chars.len()) {
        // Check if pattern matches at position i
        let mut matches = true;
        for j in 0..pattern_chars.len() {
            if text_chars[i + j] != pattern_chars[j] {
                matches = false;
                break;
            }
        }
        if matches {
            count += 1;
        }
    }
    
    count
}

fn main() {
    // Example usage
    let text = "GATATATGCATATACTT";
    let pattern = "ATAT";
    let result = pattern_count(text, pattern);
    println!("Pattern '{}' appears {} times in '{}'", pattern, result, text);
    
    // Read from stdin for Rosalind submission
    use std::io;
    
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    
    let lines: Vec<&str> = input.trim().split('\n').collect();
    if lines.len() >= 2 {
        let text = lines[0];
        let pattern = lines[1];
        let result = pattern_count(text, pattern);
        println!("{}", result);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_count() {
        assert_eq!(pattern_count("GATATATGCATATACTT", "ATAT"), 3);
        assert_eq!(pattern_count("AAAAAAAAAAAAAAAAAAAAAA", "AA"), 18);
        assert_eq!(pattern_count("AAA", "AA"), 2);
        assert_eq!(pattern_count("ABCABCABC", "ABC"), 3);
        assert_eq!(pattern_count("ABCABCABC", "XYZ"), 0);
        assert_eq!(pattern_count("", "ABC"), 0);
        assert_eq!(pattern_count("ABC", ""), 0);
    }
}
```

## Explanation

The solution works as follows:

1. **Edge Case Handling**: First, we check if either string is empty or if the pattern is longer than the text. In these cases, we return 0.

2. **Character Conversion**: We convert both strings to vectors of characters for easier indexing and comparison.

3. **Sliding Window Approach**: We iterate through all possible starting positions where the pattern could fit in the text (from index 0 to `text.len() - pattern.len()`).

4. **Pattern Matching**: For each position, we check if the pattern matches the substring starting at that position by comparing character by character.

5. **Counting**: When a match is found, we increment our counter.

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the length of the text and m is the length of the pattern
- **Space Complexity**: O(n + m) for storing the character vectors

## Example Walkthrough

For text = "GATATATGCATATACTT" and pattern = "ATAT":

- Position 0: "GATAT" - No match (G ≠ A)
- Position 1: "ATATA" - No match (T ≠ A)  
- Position 2: "TATAT" - No match (T ≠ A)
- Position 3: "ATATG" - Match found (count = 1)
- Position 4: "TATGC" - No match (T ≠ A)
- Position 5: "ATGCA" - No match (T ≠ A)
- Position 6: "TG CAT" - No match (T ≠ A)
- Position 7: "GCA TA" - No match (G ≠ A)
- Position 8: "CATAT" - Match found (count = 2)
- Position 9: "ATATA" - Match found (count = 3)

Result: 3 occurrences

