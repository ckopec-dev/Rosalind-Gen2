# Rosalind Problem: Introduction to Alternative Splicing

## Problem Description
In this problem, we need to calculate the number of ways to choose k items from n items, where k can vary from 0 to n. This is essentially calculating the sum of binomial coefficients C(n,k) for all k from 0 to n.

## Solution Approach
The key insight is that the sum of all binomial coefficients C(n,k) for k from 0 to n equals 2^n. This is because:
- C(n,0) + C(n,1) + C(n,2) + ... + C(n,n) = 2^n
- This follows from the binomial theorem: (1+1)^n = 2^n

## R Implementation

```r
# Function to solve the alternative splicing problem
alternative_splicing <- function(n) {
  # The sum of all binomial coefficients C(n,k) for k=0 to n
  # equals 2^n
  return(2^n)
}

# Example usage
n <- 6
result <- alternative_splicing(n)
cat("Number of alternative splicing patterns for n =", n, "is:", result, "\n")

# For multiple test cases
test_cases <- c(3, 4, 5, 6, 7)
cat("Results for different values of n:\n")
for (n in test_cases) {
  cat("n =", n, "->", alternative_splicing(n), "\n")
}
```

## Alternative Implementation (More Explicit)

```r
# Alternative approach calculating each binomial coefficient explicitly
alternative_splicing_explicit <- function(n) {
  total <- 0
  for (k in 0:n) {
    # Calculate binomial coefficient C(n,k)
    binomial_coeff <- choose(n, k)
    total <- total + binomial_coeff
  }
  return(total)
}

# Verify both approaches give same result
n <- 6
result1 <- alternative_splicing(n)
result2 <- alternative_splicing_explicit(n)
cat("Method 1 result:", result1, "\n")
cat("Method 2 result:", result2, "\n")
cat("Results match:", result1 == result2, "\n")
```

## Expected Output
```
Number of alternative splicing patterns for n = 6 is: 64
Results for different values of n:
n = 3 -> 8
n = 4 -> 16
n = 5 -> 32
n = 6 -> 64
n = 7 -> 128
```

## Explanation
The problem asks for the number of ways to choose k items from n items, where k can range from 0 to n. This is equivalent to finding:

$$\sum_{k=0}^{n} \binom{n}{k} = 2^n$$

This mathematical identity comes from the binomial theorem, where setting x=y=1 in (x+y)^n gives us 2^n. This represents all possible subsets of a set with n elements, including the empty set and the full set itself.

The time complexity is O(1) for the optimized solution, while the explicit calculation has O(n) complexity.

