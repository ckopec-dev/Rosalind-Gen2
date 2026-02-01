# Rosalind Problem 2SUM Solution in R

## Problem Understanding
The 2SUM problem asks us to find two indices in an array where the sum of elements at those indices equals zero. Given an array of integers, we need to return the 1-based indices of two elements that sum to zero.

## Solution Approach
1. Read the input array
2. For each element, check if its complement (negative value) exists in the array
3. Return the 1-based indices of the pair

## R Implementation

```r
# Read input from file
read_input <- function(filename) {
  lines <- readLines(filename)
  # First line contains array size and number of arrays
  # For this problem, we'll assume we're given one array per file
  numbers <- as.numeric(unlist(strsplit(lines[1], " ")))
  return(numbers)
}

# Solve 2SUM problem
solve_2sum <- function(arr) {
  n <- length(arr)
  
  # Check all pairs
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (arr[i] + arr[j] == 0) {
        return(c(i, j))  # Return 1-based indices
      }
    }
  }
  
  return(c(-1))  # No solution found
}

# Alternative more efficient approach using hash map
solve_2sum_efficient <- function(arr) {
  # Create a hash map to store value -> index mapping
  value_to_index <- list()
  
  for (i in 1:length(arr)) {
    complement <- -arr[i]
    
    # Check if complement exists in our hash map
    if (complement %in% names(value_to_index)) {
      # Found the pair
      j <- as.numeric(value_to_index[[as.character(complement)]])
      return(c(j, i))  # Return 1-based indices
    }
    
    # Store current value and its index
    value_to_index[[as.character(arr[i])]] <- i
  }
  
  return(c(-1))  # No solution found
}

# Example usage:
# Assuming input file "rosalind_2sum.txt" contains:
# 4 2
# 2 -3 1 5
# -1 2 3 4

# Read the array from input
# arr <- read_input("rosalind_2sum.txt")

# For demonstration, let's use a sample array
sample_array <- c(2, -3, 1, 5, -1, 2, 3, 4)

# Solve using brute force approach
result1 <- solve_2sum(sample_array)
print(paste("Brute force result:", paste(result1, collapse = " ")))

# Solve using efficient approach
result2 <- solve_2sum_efficient(sample_array)
print(paste("Efficient result:", paste(result2, collapse = " ")))

# For Rosalind format, we need to process multiple test cases
process_multiple_cases <- function(filename) {
  lines <- readLines(filename)
  results <- c()
  
  i <- 1
  while (i <= length(lines)) {
    # Parse array size and number of arrays if needed
    if (length(strsplit(lines[i], " ")[[1]]) > 1) {
      # This is a header line
      header <- as.numeric(unlist(strsplit(lines[i], " ")))
      n_arrays <- header[2]
      i <- i + 1
    }
    
    # Read the array
    if (i <= length(lines)) {
      arr <- as.numeric(unlist(strsplit(lines[i], " ")))
      result <- solve_2sum_efficient(arr)
      results <- c(results, paste(result, collapse = " "))
      i <- i + 1
    }
  }
  
  return(results)
}
```

## Complete Rosalind Solution

```r
# Complete solution for Rosalind 2SUM
solve_2sum_complete <- function(input_file) {
  # Read all lines from file
  lines <- readLines(input_file)
  
  # Process each test case
  results <- c()
  
  # Skip first line if it's header
  start_line <- 1
  
  # Process each array
  i <- start_line
  while (i <= length(lines)) {
    if (nchar(lines[i]) > 0) {
      # Parse the array
      arr <- as.numeric(unlist(strsplit(lines[i], " ")))
      
      # Find 2SUM solution
      found <- FALSE
      for (j in 1:(length(arr)-1)) {
        for (k in (j+1):length(arr)) {
          if (arr[j] + arr[k] == 0) {
            # Return 1-based indices
            results <- c(results, paste(j, k, sep = " "))
            found <- TRUE
            break
          }
        }
        if (found) break
      }
      
      if (!found) {
        results <- c(results, "-1")
      }
    }
    i <- i + 1
  }
  
  return(results)
}

# Usage for Rosalind problem:
# results <- solve_2sum_complete("rosalind_2sum.txt")
# writeLines(results, "output.txt")
```

## Key Points

1. **Time Complexity**: 
   - Brute force: O(n²)
   - Hash map approach: O(n)

2. **Space Complexity**: O(n) for the hash map approach

3. **Output Format**: Return 1-based indices, or -1 if no solution exists

4. **Edge Cases**: 
   - Array with no valid pair
   - Array with duplicate values
   - Empty arrays

This solution handles the standard 2SUM problem as specified in Rosalind's requirements.

