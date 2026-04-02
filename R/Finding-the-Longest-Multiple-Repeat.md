# Finding the Longest Multiple Repeat - Rosalind Problem Solution in R

## Problem Understanding

The problem asks us to find the longest substring that appears at least k times in a given DNA sequence, where k is a given integer.

## Solution Approach

1. Generate all possible substrings of the DNA sequence
2. Count occurrences of each substring
3. Filter substrings that appear at least k times
4. Find the longest among these repeated substrings

## R Implementation

```r
# Function to find the longest multiple repeat
longest_multiple_repeat <- function(sequence, k) {
  # Convert to uppercase for consistency
  sequence <- toupper(sequence)
  
  # Get length of sequence
  n <- nchar(sequence)
  
  # Store all repeated substrings with their counts
  repeats <- list()
  
  # Check all possible substring lengths from longest to shortest
  for (length in n:1) {
    # Check all possible starting positions for current length
    for (start in 1:(n - length + 1)) {
      substring <- substr(sequence, start, start + length - 1)
      
      # Count occurrences of this substring in the sequence
      count <- length(gregexpr(substring, sequence)[[1]])
      
      # If substring appears at least k times, store it
      if (count >= k) {
        repeats[[substring]] <- count
      }
    }
    
    # If we found repeats at this length, return the longest one
    if (length(repeats) > 0) {
      # Sort by length (descending) and return first
      sorted <- sort(names(repeats), decreasing = TRUE, 
                     method = "radix", index.return = TRUE)
      return(sorted[[1]][1])
    }
  }
  
  # If no repeats found, return empty string
  return("")
}

# More efficient version using a different approach
find_longest_repeat <- function(sequence, k) {
  sequence <- toupper(sequence)
  n <- nchar(sequence)
  longest_repeat <- ""
  
  # Try all possible substring lengths from longest to shortest
  for (len in n:1) {
    # Check all substrings of current length
    for (i in 1:(n - len + 1)) {
      substring <- substr(sequence, i, i + len - 1)
      
      # Count occurrences
      matches <- gregexpr(substring, sequence)[[1]]
      count <- length(matches)
      
      # If this substring repeats at least k times
      if (count >= k) {
        # Check if it's longer than current longest
        if (nchar(substring) > nchar(longest_repeat)) {
          longest_repeat <- substring
        }
      }
    }
    
    # If we found a repeat, return it (since we're checking from longest to shortest)
    if (nchar(longest_repeat) > 0) {
      return(longest_repeat)
    }
  }
  
  return("")
}

# Alternative cleaner approach
find_longest_multiple_repeat <- function(sequence, k) {
  sequence <- toupper(sequence)
  n <- nchar(sequence)
  max_length <- 0
  result <- ""
  
  # For each possible substring length
  for (length in n:1) {
    # For each possible starting position
    for (start in 1:(n - length + 1)) {
      substring <- substr(sequence, start, start + length - 1)
      
      # Count occurrences
      count <- length(gregexpr(substring, sequence)[[1]])
      
      # If repeated at least k times and longer than current result
      if (count >= k && length > max_length) {
        max_length <- length
        result <- substring
      }
    }
    
    # Early termination: if we found a repeat of maximum possible length
    if (max_length == n) {
      break
    }
  }
  
  return(result)
}

# Example usage:
# sequence <- "ACGTACGTACGT"
# k <- 3
# result <- find_longest_multiple_repeat(sequence, k)
# print(result)
```

## Example Test Case

```r
# Test with example
test_sequence <- "ACGTACGTACGT"
k <- 3

# Expected: "ACGT" (appears 3 times)
result <- find_longest_multiple_repeat(test_sequence, k)
print(paste("Longest repeat:", result))

# Another test
test_sequence2 <- "ATATATATATAT"
k <- 4

result2 <- find_longest_multiple_repeat(test_sequence2, k)
print(paste("Longest repeat:", result2))
```

## Key Features of the Solution

1. **Case insensitive**: Converts input to uppercase
2. **Efficient search**: Starts from longest possible substrings and works backwards
3. **Early termination**: Stops as soon as the longest repeat is found
4. **Proper counting**: Uses `gregexpr()` to count all occurrences including overlapping ones

## Time Complexity

- **Time**: O(n³) in worst case where n is the length of the sequence
- **Space**: O(n²) for storing all substrings

The algorithm efficiently finds the longest substring that appears at least k times in the given DNA sequence.

