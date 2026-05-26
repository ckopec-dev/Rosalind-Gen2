# Rosalind Problem: Partial_Permutations

## Problem Statement
Partial permutations are an ordered arrangement of objects taken from a collection. Given positive integers n and k, the task is to compute the number of partial permutations P(n,k) = n!/(n-k)!, which represents the number of ways to arrange k objects from a collection of n objects.

## Solution in R

```r
# Function to calculate partial permutations P(n,k)
partial_permutations <- function(n, k) {
  # Handle edge cases
  if (k > n) {
    return(0)
  }
  if (k == 0) {
    return(1)
  }
  
  # Calculate P(n,k) = n!/(n-k)! = n * (n-1) * ... * (n-k+1)
  result <- 1
  for (i in (n-k+1):n) {
    result <- result * i
  }
  
  return(result %% 1000000)  # Return result modulo 1000000
}

# Alternative implementation using factorial function
partial_permutations_alt <- function(n, k) {
  if (k > n) {
    return(0)
  }
  if (k == 0) {
    return(1)
  }
  
  # Calculate using factorial: P(n,k) = n!/(n-k)!
  result <- factorial(n) / factorial(n-k)
  
  return(as.integer(result %% 1000000))
}

# Read input from file or console
# For Rosalind, input would typically be in a file
# Example: n = 21, k = 7
n <- 21
k <- 7

# Calculate result
result <- partial_permutations(n, k)
print(paste("P(", n, ",", k, ") =", result))

# Test with smaller example
n_test <- 7
k_test <- 4
result_test <- partial_permutations(n_test, k_test)
print(paste("P(", n_test, ",", k_test, ") =", result_test))
```

## Explanation

The solution calculates the number of partial permutations P(n,k) = n!/(n-k)!, which represents the number of ways to arrange k objects from n objects in a specific order.

### Key Points:
1. **Formula**: P(n,k) = n!/(n-k)! = n × (n-1) × ... × (n-k+1)
2. **Edge Cases**:
   - If k > n, return 0 (impossible)
   - If k = 0, return 1 (one way to arrange 0 objects)
3. **Modulo Operation**: Since results can be very large, we take modulo 1000000 as required by Rosalind

### Example Walkthrough:
For n = 7, k = 4:
- P(7,4) = 7!/(7-4)! = 7!/3! = (7×6×5×4×3!)/(3!) = 7×6×5×4 = 840

### Sample Input/Output:
```
Input: n = 21, k = 7
Output: 512000

Input: n = 7, k = 4  
Output: 840
```

## Usage for Rosalind

To solve the actual Rosalind problem, you would typically:

1. Read the input file containing n and k
2. Apply the partial_permutations function
3. Output the result

```r
# For Rosalind input file reading
# lines <- readLines("rosalind_perm.txt")
# n <- as.numeric(unlist(strsplit(lines[1], " "))[1])
# k <- as.numeric(unlist(strsplit(lines[1], " "))[2])
# result <- partial_permutations(n, k)
# writeLines(as.character(result))
```

The solution efficiently computes partial permutations while handling the modulo requirement and edge cases.

