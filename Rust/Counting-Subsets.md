# Rosalind Problem: Counting Subsets

## Problem Statement
Given: A positive integer n ≤ 1000.

Return: The total number of subsets of {1,2,...,n} modulo 1000000.

## Solution Approach
The number of subsets of a set with n elements is 2^n. This is because for each element, we have 2 choices: either include it in the subset or don't include it. Since we make this choice independently for each of the n elements, the total number of subsets is 2^n.

## Rust Implementation

```rust
use std::io;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let n: u64 = input.trim().parse().unwrap();
    
    // Calculate 2^n mod 1000000
    let result = modular_pow(2, n, 1000000);
    println!("{}", result);
}

fn modular_pow(base: u64, mut exponent: u64, modulus: u64) -> u64 {
    if modulus == 1 {
        return 0;
    }
    
    let mut result = 1;
    let mut base = base % modulus;
    
    while exponent > 0 {
        if exponent % 2 == 1 {
            result = (result * base) % modulus;
        }
        exponent = exponent >> 1;
        base = (base * base) % modulus;
    }
    
    result
}
```

## Explanation

1. **Input Reading**: Read the integer n from standard input
2. **Mathematical Insight**: The number of subsets of a set with n elements is 2^n
3. **Modular Exponentiation**: Since n can be up to 1000, 2^n can be very large, so we compute 2^n mod 1000000
4. **Efficient Calculation**: Use fast modular exponentiation to avoid overflow and compute the result efficiently

## Key Points

- **Time Complexity**: O(log n) due to modular exponentiation
- **Space Complexity**: O(1)
- **Modular Arithmetic**: Essential for handling large numbers
- **Fast Exponentiation**: Uses the binary exponentiation algorithm for efficiency

## Example
For n = 3:
- Set = {1, 2, 3}
- Subsets = {}, {1}, {2}, {3}, {1,2}, {1,3}, {2,3}, {1,2,3}
- Total = 8 = 2^3

The answer would be 8 mod 1000000 = 8.

