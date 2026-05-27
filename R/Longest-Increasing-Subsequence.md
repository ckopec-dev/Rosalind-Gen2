# Rosalind Problem: Longest Increasing Subsequence (LIS) in R

## Problem Description
Given a permutation of n positive integers, find the length of the longest increasing subsequence.

## Solution

```r
# Function to find the length of longest increasing subsequence
longest_increasing_subsequence <- function(nums) {
  if (length(nums) == 0) return(0)
  
  # Dynamic programming approach
  # dp[i] represents the length of LIS ending at position i
  dp <- rep(1, length(nums))
  
  for (i in 2:length(nums)) {
    for (j in 1:(i-1)) {
      if (nums[i] > nums[j] && dp[i] < dp[j] + 1) {
        dp[i] <- dp[j] + 1
      }
    }
  }
  
  return(max(dp))
}

# Alternative optimized solution using binary search
longest_increasing_subsequence_optimized <- function(nums) {
  if (length(nums) == 0) return(0)
  
  # tails[i] stores the smallest ending element of all increasing subsequences of length i+1
  tails <- numeric(length(nums))
  size <- 0
  
  for (num in nums) {
    # Binary search for the position to insert/replace
    left <- 1
    right <- size + 1
    
    while (left < right) {
      mid <- (left + right) %/% 2
      if (tails[mid] < num) {
        left <- mid + 1
      } else {
        right <- mid
      }
    }
    
    tails[left] <- num
    if (left == size + 1) {
      size <- size + 1
    }
  }
  
  return(size)
}

# Read input from file (assuming file contains space-separated numbers)
read_permutation <- function(filename) {
  content <- readLines(filename)
  nums <- as.numeric(unlist(strsplit(content, "\\s+")))
  return(nums)
}

# Example usage:
# For a given permutation
example_permutation <- c(5, 1, 4, 2, 3, 6)

# Method 1: Dynamic Programming (O(n²))
result1 <- longest_increasing_subsequence(example_permutation)
cat("Length of LIS (DP):", result1, "\n")

# Method 2: Optimized with binary search (O(n log n))
result2 <- longest_increasing_subsequence_optimized(example_permutation)
cat("Length of LIS (Optimized):", result2, "\n")

# If reading from file:
# nums <- read_permutation("rosalind_lgis.txt")
# result <- longest_increasing_subsequence_optimized(nums)
# cat("Result:", result, "\n")
```

## Explanation

### Approach 1: Dynamic Programming (O(n²))
1. Create a `dp` array where `dp[i]` represents the length of the longest increasing subsequence ending at position `i`
2. For each element, check all previous elements that are smaller
3. Update `dp[i]` if we can extend a previous subsequence
4. Return the maximum value in `dp`

### Approach 2: Optimized with Binary Search (O(n log n))
1. Maintain an array `tails` where `tails[i]` is the smallest ending element of all increasing subsequences of length `i+1`
2. For each number, use binary search to find the correct position in `tails`
3. Replace the element at that position or append if it's larger than all elements
4. The length of `tails` gives us the LIS length

## Sample Input/Output

**Input:** `5 1 4 2 3 6`
**Output:** `4` (one possible LIS is `1 2 3 6`)

## Usage for Rosalind

To solve the actual Rosalind problem, you would:

1. Read the input file containing the permutation
2. Apply the optimized solution
3. Output the result

```r
# Complete solution for Rosalind
# Assuming input is in a file called "rosalind_lgis.txt"
nums <- read_permutation("rosalind_lgis.txt")
result <- longest_increasing_subsequence_optimized(nums)
cat(result, "\n")
```

The optimized solution is preferred for larger inputs as it has better time complexity.

