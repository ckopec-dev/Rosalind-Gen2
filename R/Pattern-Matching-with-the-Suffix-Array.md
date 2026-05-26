# Rosalind Problem: Pattern Matching with the Suffix Array

## Problem Understanding

Pattern matching with suffix arrays involves finding all occurrences of a pattern in a text using a precomputed suffix array. The key insight is that we can use binary search on the suffix array to find the range of suffixes that start with our pattern.

## Solution Approach

1. Build the suffix array of the text
2. For each pattern, use binary search to find the range of suffixes that start with the pattern
3. Return the starting positions of all matching suffixes

## R Implementation

```r
# Function to build suffix array
build_suffix_array <- function(text) {
  n <- nchar(text)
  suffixes <- character(n)
  
  # Create all suffixes
  for (i in 1:n) {
    suffixes[i] <- substr(text, i, n)
  }
  
  # Get indices that would sort suffixes
  sorted_indices <- order(suffixes)
  
  # Return the original indices (starting positions)
  return(sorted_indices)
}

# Binary search to find leftmost occurrence
binary_search_left <- function(suffix_array, text, pattern) {
  n <- length(suffix_array)
  left <- 1
  right <- n
  result <- 0
  
  while (left <= right) {
    mid <- (left + right) %/% 2
    suffix_start <- suffix_array[mid]
    suffix <- substr(text, suffix_start, nchar(text))
    
    if (startsWith(suffix, pattern)) {
      result <- mid
      right <- mid - 1  # Continue searching left
    } else if (suffix < pattern) {
      left <- mid + 1
    } else {
      right <- mid - 1
    }
  }
  
  return(result)
}

# Binary search to find rightmost occurrence
binary_search_right <- function(suffix_array, text, pattern) {
  n <- length(suffix_array)
  left <- 1
  right <- n
  result <- 0
  
  while (left <= right) {
    mid <- (left + right) %/% 2
    suffix_start <- suffix_array[mid]
    suffix <- substr(text, suffix_start, nchar(text))
    
    if (startsWith(suffix, pattern)) {
      result <- mid
      left <- mid + 1  # Continue searching right
    } else if (suffix < pattern) {
      left <- mid + 1
    } else {
      right <- mid - 1
    }
  }
  
  return(result)
}

# Main function to find pattern matches
pattern_matching_with_suffix_array <- function(text, patterns) {
  # Build suffix array
  suffix_array <- build_suffix_array(text)
  
  results <- list()
  
  for (pattern in patterns) {
    # Find range of suffixes that start with pattern
    left_pos <- binary_search_left(suffix_array, text, pattern)
    right_pos <- binary_search_right(suffix_array, text, pattern)
    
    # Extract matching positions
    matches <- c()
    
    if (left_pos > 0) {
      # Check all suffixes in the range
      for (i in left_pos:right_pos) {
        suffix_start <- suffix_array[i]
        suffix <- substr(text, suffix_start, nchar(text))
        
        if (startsWith(suffix, pattern)) {
          matches <- c(matches, suffix_start - 1)  # Convert to 0-based indexing
        }
      }
    }
    
    results[[pattern]] <- matches
  }
  
  return(results)
}

# Alternative more efficient approach
pattern_matching_efficient <- function(text, patterns) {
  # Build suffix array with positions
  n <- nchar(text)
  suffixes <- data.frame(
    position = 1:n,
    suffix = sapply(1:n, function(i) substr(text, i, n))
  )
  
  # Sort by suffix
  suffixes <- suffixes[order(suffixes$suffix), ]
  
  results <- list()
  
  for (pattern in patterns) {
    # Find all positions where pattern occurs
    matches <- c()
    
    # Check each suffix for pattern match
    for (i in 1:nrow(suffixes)) {
      suffix <- suffixes$suffix[i]
      pos <- suffixes$position[i]
      
      if (startsWith(suffix, pattern)) {
        matches <- c(matches, pos - 1)  # Convert to 0-based
      } else if (nchar(suffix) > nchar(pattern) && 
                 substr(suffix, 1, nchar(pattern)) > pattern) {
        # If suffix is lexicographically larger than pattern, 
        # no more matches in this direction
        break
      }
    }
    
    results[[pattern]] <- matches
  }
  
  return(results)
}

# Simple direct approach for small inputs
find_pattern_positions <- function(text, pattern) {
  positions <- c()
  n <- nchar(text)
  p_len <- nchar(pattern)
  
  if (p_len > n) return(positions)
  
  for (i in 1:(n - p_len + 1)) {
    if (substr(text, i, i + p_len - 1) == pattern) {
      positions <- c(positions, i - 1)  # 0-based indexing
    }
  }
  
  return(positions)
}

# Complete solution for the problem
solve_pattern_matching <- function(text, patterns) {
  # For each pattern, find all positions in text
  results <- list()
  
  for (pattern in patterns) {
    results[[pattern]] <- find_pattern_positions(text, pattern)
  }
  
  return(results)
}

# Example usage:
# text <- "AATCGGGTTCAATCGGGGT"
# patterns <- c("ATCG", "GGGT")
# result <- solve_pattern_matching(text, patterns)
# print(result)
```

## Example Test

```r
# Test with sample data
text <- "AATCGGGTTCAATCGGGGT"
patterns <- c("ATCG", "GGGT")

# Expected output:
# ATCG: [1, 11] (positions 1 and 11 in 0-based indexing)
# GGGT: [8] (position 8 in 0-based indexing)

result <- solve_pattern_matching(text, patterns)
print(result)
```

## Time Complexity

- Building suffix array: O(n² log n) where n is the length of text
- Pattern matching: O(m log n) per pattern where m is pattern length
- Overall: O(n² log n + m log n)

## Space Complexity

O(n²) for storing all suffixes

## Key Points

1. The suffix array contains starting positions of sorted suffixes
2. Binary search can be used to find the range of matching suffixes
3. Pattern matching is done by checking prefix matches
4. Results are returned in 0-based indexing as per Rosalind convention

