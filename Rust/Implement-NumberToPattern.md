# Rosalind Problem: Implement_NumberToPattern

## Problem Description
Implement the NumberToPattern function that converts a number to its corresponding DNA k-mer pattern.

## Solution in Rust

```rust
fn number_to_pattern(index: usize, k: usize) -> String {
    if k == 1 {
        return match index {
            0 => "A".to_string(),
            1 => "C".to_string(),
            2 => "G".to_string(),
            3 => "T".to_string(),
            _ => panic!("Invalid index for k=1"),
        };
    }
    
    let prefix_index = index / 4;
    let remainder = index % 4;
    
    let prefix_pattern = number_to_pattern(prefix_index, k - 1);
    let symbol = match remainder {
        0 => "A",
        1 => "C",
        2 => "G",
        3 => "T",
        _ => panic!("Invalid remainder"),
    };
    
    format!("{}{}", prefix_pattern, symbol)
}

fn main() {
    // Example usage
    let result = number_to_pattern(45, 4);
    println!("Number 45 with k=4: {}", result);
    
    // Test cases
    println!("0,1: {}", number_to_pattern(0, 1)); // Should be "A"
    println!("4,1: {}", number_to_pattern(4, 1)); // Should be "T"
    println!("45,4: {}", number_to_pattern(45, 4)); // Should be "ACGT"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_to_pattern() {
        assert_eq!(number_to_pattern(0, 1), "A");
        assert_eq!(number_to_pattern(1, 1), "C");
        assert_eq!(number_to_pattern(2, 1), "G");
        assert_eq!(number_to_pattern(3, 1), "T");
        assert_eq!(number_to_pattern(45, 4), "ACGT");
        assert_eq!(number_to_pattern(0, 4), "AAAA");
        assert_eq!(number_to_pattern(255, 4), "TTTT");
    }
}
```

## Explanation

This implementation uses a recursive approach to convert a number to its corresponding DNA k-mer pattern:

1. **Base Case**: When k=1, we directly map the index to a DNA symbol:
   - 0 → "A"
   - 1 → "C" 
   - 2 → "G"
   - 3 → "T"

2. **Recursive Case**: For k>1:
   - Calculate the prefix index: `index / 4`
   - Calculate the remainder: `index % 4`
   - Recursively get the pattern for the prefix index with k-1
   - Map the remainder to the corresponding DNA symbol

3. **Algorithm Logic**:
   - DNA patterns are represented in base-4 (A=0, C=1, G=2, T=3)
   - The recursive approach builds the pattern from left to right
   - Each recursive call handles one position in the k-mer

## Time and Space Complexity
- **Time Complexity**: O(k) - we make k recursive calls
- **Space Complexity**: O(k) - due to the recursive call stack

## Example Walkthrough
For `number_to_pattern(45, 4)`:
1. 45 / 4 = 11, remainder = 1 → "C"
2. 11 / 4 = 2, remainder = 3 → "T" 
3. 2 / 4 = 0, remainder = 2 → "G"
4. 0 / 4 = 0, remainder = 0 → "A"
5. Result: "ACGT"

