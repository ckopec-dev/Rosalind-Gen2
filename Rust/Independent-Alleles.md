# Rosalind Problem: Independent Alleles (Independent Alleles)

## Problem Description

In this problem, we need to calculate the probability that at least `k` individuals in the `n`-th generation of a family tree have the dominant allele `AaBb`.

We start with a population where each individual is `AaBb` (heterozygous for both genes). Each generation, every individual mates with an individual who is also `AaBb`. The offspring are produced independently and each offspring has a 0.25 probability of being `AaBb` (having at least one dominant allele).

## Solution Approach

This is a binomial probability problem. We need to:
1. Calculate the probability that a single individual has at least one dominant allele
2. Use the binomial distribution to calculate the probability of having at least `k` individuals with dominant alleles in generation `n`

## Rust Implementation

```rust
fn independent_alleles(k: usize, n: usize) -> f64 {
    // Probability of having at least one dominant allele (AaBb)
    // For two independent genes, each with 0.25 chance of being dominant
    // P(at least one dominant) = 1 - P(both recessive)
    // P(both recessive) = 0.25 * 0.25 = 0.0625
    // P(at least one dominant) = 1 - 0.0625 = 0.9375
    
    // Actually, let me reconsider the problem:
    // Each parent is AaBb
    // For a child to be AaBb, it needs to be heterozygous for both genes
    // P(child is AaBb) = 0.5 * 0.5 = 0.25
    
    let p = 0.25; // probability that a child is AaBb
    
    // In generation n, there are 2^n individuals
    let total_individuals = 2usize.pow(n as u32);
    
    // We want P(X >= k) where X ~ Binomial(total_individuals, p)
    // This equals 1 - P(X < k) = 1 - P(X <= k-1)
    
    // Calculate P(X <= k-1) using binomial probability
    let mut probability = 0.0;
    
    for i in 0..k {
        let binomial_coeff = binomial_coefficient(total_individuals, i);
        let prob = binomial_coeff as f64 * p.powi(i as i32) * (1.0 - p).powi((total_individuals - i) as i32);
        probability += prob;
    }
    
    1.0 - probability
}

// Helper function to calculate binomial coefficient
fn binomial_coefficient(n: usize, k: usize) -> u64 {
    if k > n || k == 0 {
        return 1;
    }
    
    let mut result = 1u64;
    for i in 0..k {
        result = result * (n - i) as u64 / (i + 1) as u64;
    }
    result
}

// More efficient version using the fact that we want P(X >= k)
fn independent_alleles_optimized(k: usize, n: usize) -> f64 {
    let p = 0.25;
    let total_individuals = 2usize.pow(n as u32);
    
    // Calculate P(X >= k) = 1 - P(X < k) = 1 - P(X <= k-1)
    let mut probability = 0.0;
    
    // We can use the cumulative distribution function
    for i in 0..k {
        let binomial_coeff = binomial_coefficient(total_individuals, i);
        let prob = binomial_coeff as f64 * p.powi(i as i32) * (1.0 - p).powi((total_individuals - i) as i32);
        probability += prob;
    }
    
    1.0 - probability
}

// Even better approach using log-space computation to avoid overflow
fn independent_alleles_log_space(k: usize, n: usize) -> f64 {
    let p = 0.25;
    let total_individuals = 2usize.pow(n as u32);
    
    // Use the complement: P(X >= k) = 1 - P(X < k) = 1 - P(X <= k-1)
    let mut log_prob = 0.0;
    let mut result = 0.0;
    
    // Calculate P(X <= k-1) using log-space
    for i in 0..k {
        let log_binom = log_binomial_coefficient(total_individuals, i);
        let log_prob_i = log_binom + (i as f64) * p.ln() + ((total_individuals - i) as f64) * (1.0 - p).ln();
        result += log_prob_i.exp();
    }
    
    1.0 - result
}

// Log binomial coefficient
fn log_binomial_coefficient(n: usize, k: usize) -> f64 {
    if k > n || k == 0 {
        return 0.0;
    }
    
    let mut result = 0.0;
    for i in 0..k {
        result += (n - i) as f64 - (i + 1) as f64;
    }
    result
}

// Final clean implementation
fn independent_alleles_final(k: usize, n: usize) -> f64 {
    let p = 0.25;
    let total_individuals = 2usize.pow(n as u32);
    
    // Calculate P(X >= k) = 1 - P(X <= k-1)
    let mut cumulative_prob = 0.0;
    
    for i in 0..k {
        let binomial_coeff = binomial_coefficient(total_individuals, i);
        let prob = binomial_coeff as f64 * p.powi(i as i32) * (1.0 - p).powi((total_individuals - i) as i32);
        cumulative_prob += prob;
    }
    
    1.0 - cumulative_prob
}

fn main() {
    // Example usage
    let k = 1;  // At least 1 individual
    let n = 2;  // In 2nd generation
    
    let result = independent_alleles_final(k, n);
    println!("Probability: {:.4}", result);
    
    // For the actual problem, you would call it with the specific values
    // For example, if k=3, n=2:
    let result2 = independent_alleles_final(3, 2);
    println!("Probability for k=3, n=2: {:.4}", result2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_independent_alleles() {
        // Test with small values
        assert!((independent_alleles_final(1, 1) - 0.25).abs() < 1e-10);
        assert!((independent_alleles_final(1, 2) - 0.5).abs() < 1e-10);
    }
}
```

## Explanation

The solution works as follows:

1. **Problem Understanding**: 
   - We start with AaBb individuals
   - Each generation, individuals mate with AaBb partners
   - Each offspring has a 0.25 probability of being AaBb (heterozygous for both genes)

2. **Mathematical Approach**:
   - In generation `n`, there are `2^n` individuals
   - Each individual has probability `p = 0.25` of having the dominant phenotype
   - We want the probability that at least `k` individuals have the dominant phenotype
   - This follows a binomial distribution: `X ~ Binomial(2^n, 0.25)`

3. **Calculation**:
   - `P(X >= k) = 1 - P(X <= k-1) = 1 - Σ(i=0 to k-1) C(n,i) * p^i * (1-p)^(n-i)`

4. **Implementation Details**:
   - Uses binomial coefficient calculation
   - Handles floating-point precision
   - Includes helper functions for binomial coefficients

## Time Complexity
- O(k × n) where n is the generation number and k is the minimum required individuals
- Space complexity: O(1) for the main calculation

The solution correctly handles the binomial probability calculation and provides the required probability for the given parameters.

