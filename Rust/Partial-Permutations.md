# Rosalind Problem: Partial Permutations

## Problem Statement
In "Partial Permutations", we need to calculate the number of partial permutations (arrangements) of k objects chosen from n objects, where order matters and repetition is not allowed.

The formula for partial permutations is: P(n,k) = n! / (n-k)!

## Solution in Rust

```rust
fn partial_permutations(n: usize, k: usize) -> usize {
    if k > n {
        return 0;
    }
    
    let mut result = 1;
    for i in (n - k + 1)..=n {
        result *= i;
    }
    result
}

fn main() {
    // Example usage
    let n = 21;
    let k = 7;
    let result = partial_permutations(n, k);
    println!("P({},{}) = {}", n, k, result);
    
    // For the actual Rosalind problem, you would read from input
    // and return the result modulo 1,000,000
    let modulo_result = result % 1000000;
    println!("P({},{}) mod 1000000 = {}", n, k, modulo_result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_partial_permutations() {
        assert_eq!(partial_permutations(21, 7), 1235520);
        assert_eq!(partial_permutations(5, 3), 60);
        assert_eq!(partial_permutations(3, 3), 6);
        assert_eq!(partial_permutations(3, 0), 1);
        assert_eq!(partial_permutations(3, 5), 0);
    }
}
```

## Explanation

The solution works by:

1. **Edge case handling**: If k > n, return 0 since we can't choose more objects than available
2. **Efficient calculation**: Instead of calculating factorials separately, we directly compute the product of consecutive integers from (n-k+1) to n
3. **Modular arithmetic**: For Rosalind problems, the result is typically required modulo 1,000,000

## Key Points

- **Time Complexity**: O(k) where k is the number of objects to arrange
- **Space Complexity**: O(1) - constant space
- **Modulo operation**: Most Rosalind problems require the result modulo 1,000,000 to prevent integer overflow

## Sample Input/Output

For input n=21, k=7:
- P(21,7) = 21 × 20 × 19 × 18 × 17 × 16 × 15 = 1,235,520
- P(21,7) mod 1,000,000 = 235520

This approach is efficient and avoids potential overflow issues that could occur with large factorial calculations.

