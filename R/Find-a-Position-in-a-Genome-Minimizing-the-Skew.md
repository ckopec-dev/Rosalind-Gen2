# Finding a Position in a Genome Minimizing the Skew

## Problem Understanding

The skew of a genome is defined as the difference between the number of occurrences of 'G' and the number of occurrences of 'C' at each position in the genome. We need to find the position(s) where the skew reaches its minimum value.

## Solution Approach

1. Calculate the skew at each position by tracking the cumulative difference between G and C counts
2. Find the minimum skew value
3. Return all positions where this minimum occurs

## R Implementation

```r
# Function to find positions minimizing the skew
find_min_skew_positions <- function(genome) {
  # Initialize variables
  skew <- 0
  skew_values <- c(0)  # Start with 0 at position 0
  min_skew <- 0
  
  # Calculate skew at each position
  for (i in 1:length(genome)) {
    # Update skew based on current nucleotide
    if (genome[i] == 'G') {
      skew <- skew + 1
    } else if (genome[i] == 'C') {
      skew <- skew - 1
    }
    
    # Store skew value
    skew_values <- c(skew_values, skew)
    
    # Update minimum skew if needed
    if (skew < min_skew) {
      min_skew <- skew
    }
  }
  
  # Find all positions where minimum skew occurs
  min_positions <- which(skew_values == min_skew)
  
  return(min_positions)
}

# Alternative more concise implementation
find_min_skew_positions_v2 <- function(genome) {
  # Calculate cumulative skew
  skew <- cumsum(ifelse(genome == 'G', 1, ifelse(genome == 'C', -1, 0)))
  
  # Add initial position (0)
  skew <- c(0, skew)
  
  # Find minimum skew value
  min_skew <- min(skew)
  
  # Find all positions with minimum skew
  positions <- which(skew == min_skew)
  
  return(positions)
}

# Example usage:
# genome <- "CCTATCGGTATTCACCTCCAT"
# result <- find_min_skew_positions_v2(genome)
# print(result)
```

## Example Walkthrough

Let's trace through an example with genome "CCTATCGGTATTCACCTCCAT":

```
Position:  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
Genome:    C C T A T C G G T A T T C A C C T C C A T

Skew calculation:
Position 0: 0 (initial)
Position 1: 0-1 = -1 (C)
Position 2: -1-1 = -2 (C)
Position 3: -2+0 = -2 (T)
Position 4: -2+0 = -2 (A)
Position 5: -2-1 = -3 (C)
Position 6: -3+1 = -2 (G)
Position 7: -2+1 = -1 (G)
Position 8: -1+0 = -1 (T)
Position 9: -1+0 = -1 (A)
Position 10: -1+0 = -1 (T)
Position 11: -1-1 = -2 (C)
Position 12: -2+1 = -1 (A)
Position 13: -1-1 = -2 (C)
Position 14: -2+1 = -1 (C)
Position 15: -1+1 = 0 (T)
Position 16: 0+1 = 1 (C)
Position 17: 1+1 = 2 (C)
Position 18: 2-1 = 1 (A)
Position 19: 1-1 = 0 (T)
Position 20: 0-1 = -1 (C)
```

The minimum skew value is -3, occurring at position 5.

## Final Solution

```r
# Complete solution for Rosalind problem
find_min_skew_positions <- function(genome) {
  # Calculate cumulative skew
  skew <- cumsum(ifelse(genome == 'G', 1, ifelse(genome == 'C', -1, 0)))
  
  # Add initial position (0)
  skew <- c(0, skew)
  
  # Find minimum skew value
  min_skew <- min(skew)
  
  # Find all positions with minimum skew
  positions <- which(skew == min_skew)
  
  return(positions)
}

# Usage:
# genome <- readLines("rosalind_ba1f.txt")  # Read from file
# result <- find_min_skew_positions(genome)
# print(result)
```

This solution has O(n) time complexity and O(n) space complexity, where n is the length of the genome string.

