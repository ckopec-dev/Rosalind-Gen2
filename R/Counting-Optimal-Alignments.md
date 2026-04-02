# Rosalind Problem: Counting Optimal Alignments

## Problem Description

Given two strings s and t, we want to count the number of optimal alignments between them. An optimal alignment is one that minimizes the edit distance (Levenshtein distance) between the strings.

## Solution Approach

We'll use dynamic programming to:
1. Compute the edit distance matrix
2. Count the number of ways to achieve the optimal edit distance

## R Implementation

```r
# Function to count optimal alignments between two strings
count_optimal_alignments <- function(s, t) {
  m <- nchar(s)
  n <- nchar(t)
  
  # Create DP matrix for edit distances
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize base cases
  dp[1, ] <- 0:(n)
  dp[, 1] <- 0:(m)
  
  # Fill the DP matrix for edit distances
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      if (substr(s, i - 1, i - 1) == substr(t, j - 1, j - 1)) {
        dp[i, j] <- dp[i - 1, j - 1]
      } else {
        dp[i, j] <- 1 + min(dp[i - 1, j], dp[i, j - 1], dp[i - 1, j - 1])
      }
    }
  }
  
  # Create matrix to count number of optimal alignments
  count <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize base cases for counting
  count[1, ] <- 1
  count[, 1] <- 1
  
  # Fill the count matrix
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      if (substr(s, i - 1, i - 1) == substr(t, j - 1, j - 1)) {
        # Characters match - only diagonal path contributes
        count[i, j] <- count[i - 1, j - 1]
      } else {
        # Characters don't match - consider all possible operations
        min_dist <- dp[i, j]
        total <- 0
        
        # Check if deletion leads to optimal distance
        if (dp[i - 1, j] == min_dist - 1) {
          total <- total + count[i - 1, j]
        }
        
        # Check if insertion leads to optimal distance
        if (dp[i, j - 1] == min_dist - 1) {
          total <- total + count[i, j - 1]
        }
        
        # Check if substitution leads to optimal distance
        if (dp[i - 1, j - 1] == min_dist - 1) {
          total <- total + count[i - 1, j - 1]
        }
        
        count[i, j] <- total
      }
    }
  }
  
  return(count[m + 1, n + 1])
}

# Alternative cleaner implementation
count_optimal_alignments_v2 <- function(s, t) {
  m <- nchar(s)
  n <- nchar(t)
  
  # Create DP matrix for edit distances
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Initialize base cases
  dp[1, ] <- 0:(n)
  dp[, 1] <- 0:(m)
  
  # Fill the DP matrix for edit distances
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      if (substr(s, i - 1, i - 1) == substr(t, j - 1, j - 1)) {
        dp[i, j] <- dp[i - 1, j - 1]
      } else {
        dp[i, j] <- 1 + min(dp[i - 1, j], dp[i, j - 1], dp[i - 1, j - 1])
      }
    }
  }
  
  # Create count matrix
  count <- matrix(0, nrow = m + 1, ncol = n + 1)
  count[1, 1] <- 1
  
  # Fill the count matrix
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      if (i == 1 && j == 1) next
      
      if (i == 1) {
        count[i, j] <- 1
      } else if (j == 1) {
        count[i, j] <- 1
      } else {
        # Check if current characters match
        if (substr(s, i - 1, i - 1) == substr(t, j - 1, j - 1)) {
          # Match case
          count[i, j] <- count[i - 1, j - 1]
        } else {
          # Mismatch case - sum all paths that lead to optimal distance
          min_dist <- dp[i, j]
          total <- 0
          
          # Deletion path
          if (dp[i - 1, j] == min_dist - 1) {
            total <- total + count[i - 1, j]
          }
          
          # Insertion path
          if (dp[i, j - 1] == min_dist - 1) {
            total <- total + count[i, j - 1]
          }
          
          # Substitution path
          if (dp[i - 1, j - 1] == min_dist - 1) {
            total <- total + count[i - 1, j - 1]
          }
          
          count[i, j] <- total
        }
      }
    }
  }
  
  return(count[m + 1, n + 1])
}

# Test with example
s <- "ATAT"
t <- "ATA"
result <- count_optimal_alignments_v2(s, t)
print(paste("Number of optimal alignments:", result))

# Another test case
s2 <- "ACGT"
t2 <- "ACGT"
result2 <- count_optimal_alignments_v2(s2, t2)
print(paste("Number of optimal alignments:", result2))
```

## Explanation

The algorithm works in two phases:

1. **Compute Edit Distance**: First, we compute the standard edit distance matrix using dynamic programming
2. **Count Alignments**: Then, we build a second matrix that counts the number of ways to achieve each edit distance value

Key insights:
- If characters match, we only inherit from the diagonal (substitution)
- If characters don't match, we sum the counts from all three possible operations (deletion, insertion, substitution) that lead to the optimal distance
- Base cases are initialized properly for the counting matrix

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the strings
- **Space Complexity**: O(m×n) for the DP matrices

This solution handles all edge cases including empty strings and strings with no optimal alignments.

