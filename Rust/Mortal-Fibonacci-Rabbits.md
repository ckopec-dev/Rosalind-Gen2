# Rosalind Problem: Mortal Fibonacci Rabbits

## Problem Description
In this problem, we need to compute the number of rabbit pairs after n months, where each pair of rabbits lives for m months.

## Solution in Rust

```rust
fn mortal_fibonacci_rabbits(n: usize, m: usize) -> usize {
    // Handle edge cases
    if n == 0 || m == 0 {
        return 0;
    }
    
    // For n < m, rabbits don't die yet, so it's regular fibonacci
    if n < m {
        return fibonacci(n);
    }
    
    // Create a vector to store the number of pairs for each age group
    // Index 0 represents rabbits that are 1 month old
    // Index 1 represents rabbits that are 2 months old
    // ...
    // Index m-1 represents rabbits that are m months old (about to die)
    let mut ages = vec![0; m];
    
    // Initially, there's 1 pair of newborn rabbits
    ages[0] = 1;
    
    // Simulate each month
    for _ in 1..n {
        let mut new_ages = vec![0; m];
        
        // All rabbits that are at least 2 months old produce offspring
        // The number of new pairs equals the number of rabbits that are 2 or more months old
        let new_pairs = ages[1..].iter().sum::<usize>();
        new_ages[0] = new_pairs;
        
        // All rabbits that are 1 month old become 2 months old
        // All rabbits that are 2 to m-1 months old become 1 month older
        // Rabbits that are m months old die
        for i in 1..m {
            new_ages[i] = ages[i-1];
        }
        
        ages = new_ages;
    }
    
    // Return total number of pairs
    ages.iter().sum()
}

// Helper function for regular fibonacci (when m >= n)
fn fibonacci(n: usize) -> usize {
    if n <= 2 {
        return 1;
    }
    
    let mut a = 1;
    let mut b = 1;
    
    for _ in 2..n {
        let temp = a + b;
        a = b;
        b = temp;
    }
    
    b
}

fn main() {
    // Example usage
    let n = 6;  // months
    let m = 3;  // lifespan in months
    
    let result = mortal_fibonacci_rabbits(n, m);
    println!("Number of rabbit pairs after {} months with {} month lifespan: {}", n, m, result);
    
    // Test with the sample input from Rosalind
    // Input: n=93, m=16
    let result2 = mortal_fibonacci_rabbits(93, 16);
    println!("Rosalind sample result: {}", result2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_small_cases() {
        // Test case from problem description
        assert_eq!(mortal_fibonacci_rabbits(6, 3), 4);
    }
    
    #[test]
    fn test_edge_cases() {
        assert_eq!(mortal_fibonacci_rabbits(0, 3), 0);
        assert_eq!(mortal_fibonacci_rabbits(5, 0), 0);
        assert_eq!(mortal_fibonacci_rabbits(1, 1), 1);
    }
}
```

## Explanation

The solution uses a **age-based approach** where we track rabbits by their age groups:

1. **Initialization**: Create an array where each index represents rabbits of a specific age (0 = newborn, 1 = 1 month old, etc.)

2. **Monthly Simulation**: For each month:
   - New pairs are born equal to the number of rabbits that are 2 or more months old
   - All rabbits age by one month (shift array elements)
   - Rabbits that reach age `m` die

3. **Result**: Sum all rabbits in all age groups

## Key Insights

- For `n < m`: Rabbits don't die yet, so it's a regular Fibonacci sequence
- For `n >= m`: We need to account for rabbits dying at age `m`
- The algorithm efficiently tracks rabbit populations by age rather than individual rabbits

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the number of months and m is the lifespan
- **Space Complexity**: O(m) for storing the age groups

This approach correctly handles the mortality aspect of the Fibonacci rabbit problem and efficiently computes the result for the given constraints.

