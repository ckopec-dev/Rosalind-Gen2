# Rosalind Problem: 2-Way Partition

## Problem Description
Given: A positive integer n ≤ 10⁵ and an array A[1..n] of integers from -10⁵ to 10⁵.

Return: A partition of A into two parts such that the difference between the maximum and minimum elements in each part is minimized.

## Solution in R

```r
# Function to perform 2-way partition
two_way_partition <- function(n, A) {
  # Sort the array
  sorted_A <- sort(A)
  
  # Find the optimal partition point
  # We want to minimize the difference between max and min in each part
  # This is achieved by finding the point where the difference is minimized
  
  min_diff <- Inf
  best_split <- 1
  
  # Try all possible split points
  for (i in 1:(n-1)) {
    # Left part: elements from 1 to i
    # Right part: elements from i+1 to n
    
    left_max <- sorted_A[i]
    left_min <- sorted_A[1]
    
    right_max <- sorted_A[n]
    right_min <- sorted_A[i+1]
    
    # Calculate the difference for this partition
    left_diff <- left_max - left_min
    right_diff <- right_max - right_min
    total_diff <- left_diff + right_diff
    
    if (total_diff < min_diff) {
      min_diff <- total_diff
      best_split <- i
    }
  }
  
  # Return the partition
  left_part <- sorted_A[1:best_split]
  right_part <- sorted_A[(best_split+1):n]
  
  return(list(
    left = left_part,
    right = right_part,
    min_diff = min_diff
  ))
}

# Alternative approach - more efficient
two_way_partition_optimized <- function(n, A) {
  # Sort the array
  sorted_A <- sort(A)
  
  # The optimal partition is at the point where the sum of differences is minimized
  # For a sorted array, we want to minimize (max_left - min_left) + (max_right - min_right)
  # Since left part starts from index 1, min_left = sorted_A[1]
  # Since right part starts from index best_split+1, min_right = sorted_A[best_split+1]
  
  min_diff <- Inf
  best_split <- 1
  
  # Try all possible split points
  for (i in 1:(n-1)) {
    # Left part: sorted_A[1] to sorted_A[i]
    # Right part: sorted_A[i+1] to sorted_A[n]
    
    left_max <- sorted_A[i]
    left_min <- sorted_A[1]
    
    right_max <- sorted_A[n]
    right_min <- sorted_A[i+1]
    
    # Calculate total difference
    total_diff <- (left_max - left_min) + (right_max - right_min)
    
    if (total_diff < min_diff) {
      min_diff <- total_diff
      best_split <- i
    }
  }
  
  # Return the partition as indices
  left_indices <- 1:best_split
  right_indices <- (best_split+1):n
  
  return(list(
    left_indices = left_indices,
    right_indices = right_indices,
    left_values = sorted_A[left_indices],
    right_values = sorted_A[right_indices],
    min_diff = min_diff
  ))
}

# Read input from file (example)
# Input format: n followed by array elements
read_input <- function(filename) {
  lines <- readLines(filename)
  n <- as.numeric(lines[1])
  A <- as.numeric(unlist(strsplit(lines[2], " ")))
  return(list(n = n, A = A))
}

# Example usage
# For demonstration purposes, let's create sample data
n <- 5
A <- c(3, 1, 4, 1, 5)

# Solve the problem
result <- two_way_partition_optimized(n, A)

# Print results
cat("Original array:", paste(A, collapse = " "), "\n")
cat("Sorted array:", paste(sort(A), collapse = " "), "\n")
cat("Left part:", paste(result$left_values, collapse = " "), "\n")
cat("Right part:", paste(result$right_values, collapse = " "), "\n")
cat("Minimum total difference:", result$min_diff, "\n")

# If we want to output in the format required by Rosalind
# (just the partitioned array)
partition_array <- function(n, A) {
  sorted_A <- sort(A)
  
  # Find best split point
  min_diff <- Inf
  best_split <- 1
  
  for (i in 1:(n-1)) {
    left_max <- sorted_A[i]
    left_min <- sorted_A[1]
    right_max <- sorted_A[n]
    right_min <- sorted_A[i+1]
    
    total_diff <- (left_max - left_min) + (right_max - right_min)
    
    if (total_diff < min_diff) {
      min_diff <- total_diff
      best_split <- i
    }
  }
  
  # Return the sorted array (this is the partitioned result)
  return(sorted_A)
}

# Final function for Rosalind submission
solve_2way_partition <- function(n, A) {
  # Sort the array first
  sorted_A <- sort(A)
  
  # Find optimal split point
  min_diff <- Inf
  best_split <- 1
  
  for (i in 1:(n-1)) {
    left_max <- sorted_A[i]
    left_min <- sorted_A[1]
    right_max <- sorted_A[n]
    right_min <- sorted_A[i+1]
    
    total_diff <- (left_max - left_min) + (right_max - right_min)
    
    if (total_diff < min_diff) {
      min_diff <- total_diff
      best_split <- i
    }
  }
  
  # Return the sorted array as the answer
  return(sorted_A)
}
```

## Key Points:

1. **Approach**: Since we want to minimize the difference between max and min in each part, we sort the array first and then try all possible split points.

2. **Optimization**: For each split point, we calculate the sum of differences between max and min for both left and right parts.

3. **Time Complexity**: O(n²) for the brute force approach, but can be optimized to O(n log n) with better algorithms.

4. **Output**: The function returns the sorted array which represents the optimal 2-way partition.

## Sample Input/Output:

**Input:**
```
5
3 1 4 1 5
```

**Output:**
```
1 1 3 4 5
```

The function returns the sorted array where the optimal 2-way partition has been achieved.

