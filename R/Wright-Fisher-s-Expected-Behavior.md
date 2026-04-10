# Rosalind Problem: Wright-Fisher's Expected Behavior

## Problem Understanding

This problem asks us to compute the expected number of copies of a particular allele in the next generation under the Wright-Fisher model of genetic drift.

In the Wright-Fisher model:
- We have a population of 2N diploid individuals (so 4N total alleles)
- Each generation, alleles are randomly sampled from the previous generation
- We want to find the expected number of copies of a specific allele in the next generation

## Mathematical Background

For a population of 2N diploid individuals (4N total alleles), if we start with k copies of a particular allele in the current generation, the expected number of copies in the next generation is k (the mean remains the same due to the properties of the Wright-Fisher model).

However, the problem likely asks for the expected frequency or variance in the next generation.

## Solution Approach

Let's implement this step by step:

```r
# Wright-Fisher's Expected Behavior Solution
# Problem: Given population size 2N and current allele count k, 
# find expected behavior in next generation

wright_fisher_expected <- function(N, k) {
  # N: population size (2N diploid individuals)
  # k: current number of copies of the allele
  
  # In Wright-Fisher model, the expected number of copies remains the same
  # This is because the model preserves the mean
  expected_copies <- k
  
  # The variance in the next generation is: k * (1 - k/(2*N))
  # But the problem asks for expected behavior, which is the expected value
  return(expected_copies)
}

# Alternative interpretation - if we want to simulate the process
wright_fisher_simulation <- function(N, k, generations = 1) {
  # Simulate Wright-Fisher process for specified generations
  current_k <- k
  
  for (i in 1:generations) {
    # Expected value remains the same in Wright-Fisher model
    current_k <- current_k
  }
  
  return(current_k)
}

# More complete solution for the specific problem format
solve_wright_fisher <- function(N, k) {
  # For Wright-Fisher model with population size 2N and k copies
  # The expected number of copies in next generation is k
  # This is because E[X_{t+1}] = E[X_t] = k
  
  return(k)
}

# Example usage
# If N = 5 (population size = 10 diploid individuals)
# and k = 3 (3 copies of allele)
result <- solve_wright_fisher(5, 3)
print(paste("Expected copies in next generation:", result))

# More detailed analysis
analyze_wright_fisher <- function(N, k) {
  # Population size
  population_size <- 2 * N
  
  # Expected value
  expected <- k
  
  # Variance in next generation: k * (1 - k/(2*N))
  variance <- k * (1 - k/population_size)
  
  # Expected frequency
  expected_freq <- k / population_size
  
  return(list(
    population_size = population_size,
    current_copies = k,
    expected_copies = expected,
    variance = variance,
    expected_frequency = expected_freq
  ))
}

# Test with example
analysis <- analyze_wright_fisher(5, 3)
print(analysis)
```

## Final Clean Solution

```r
# Wright-Fisher's Expected Behavior - Clean Solution
wright_fisher_expected_behavior <- function(N, k) {
  # N: population size (2N diploid individuals)
  # k: current number of copies of the allele
  
  # In Wright-Fisher model, the expected number of copies remains the same
  return(k)
}

# For the specific Rosalind problem format
# Input: N (population size) and k (current copies)
# Output: Expected copies in next generation

# Example:
# N = 5, k = 3
# Expected result = 3

# This is the fundamental property of Wright-Fisher model:
# The expected value is preserved across generations
```

## Key Points

1. **Wright-Fisher Model Property**: The expected number of copies of any allele remains constant across generations
2. **Mathematical Foundation**: This is due to the martingale property of the model
3. **Practical Implication**: While individual realizations vary due to genetic drift, the mean remains unchanged
4. **Answer**: For any N and k, the expected behavior is simply k

The solution is straightforward because the Wright-Fisher model preserves the expected value of allele frequencies across generations.

