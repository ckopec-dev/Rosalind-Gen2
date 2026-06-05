# Rosalind Problem: Counting Inversions

## Problem Description
An inversion in a sequence is a pair of positions where the elements are out of order. Given a sequence of integers, count the number of inversions.

## Solution in R

```r
# Function to count inversions using merge sort approach
count_inversions <- function(arr) {
  if (length(arr) <= 1) {
    return(0)
  }
  
  # Split array into two halves
  mid <- length(arr) %/% 2
  left <- arr[1:mid]
  right <- arr[(mid + 1):length(arr)]
  
  # Count inversions in left and right halves
  left_inv <- count_inversions(left)
  right_inv <- count_inversions(right)
  
  # Count split inversions
  split_inv <- merge_and_count(left, right)
  
  return(left_inv + right_inv + split_inv)
}

# Helper function to merge two sorted arrays and count split inversions
merge_and_count <- function(left, right) {
  i <- j <- k <- 0
  inv_count <- 0
  result <- numeric(length(left) + length(right))
  
  # Merge the two sorted arrays
  while (i < length(left) && j < length(right)) {
    if (left[i + 1] <= right[j + 1]) {
      result[k + 1] <- left[i + 1]
      i <- i + 1
    } else {
      result[k + 1] <- right[j + 1]
      j <- j + 1
      # All elements from i to end of left array form inversions with current right element
      inv_count <- inv_count + (length(left) - i)
    }
    k <- k + 1
  }
  
  # Copy remaining elements
  while (i < length(left)) {
    result[k + 1] <- left[i + 1]
    i <- i + 1
    k <- k + 1
  }
  
  while (j < length(right)) {
    result[k + 1] <- right[j + 1]
    j <- j + 1
    k <- k + 1
  }
  
  return(inv_count)
}

# Alternative simpler approach for small arrays (O(n^2))
count_inversions_simple <- function(arr) {
  n <- length(arr)
  count <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (arr[i] > arr[j]) {
        count <- count + 1
      }
    }
  }
  
  return(count)
}

# Read input from file or console
# For Rosalind, you would typically read from a file
# Example input: "1 3 5 2 4 6"
read_input <- function(filename = NULL) {
  if (!is.null(filename)) {
    # Read from file
    lines <- readLines(filename)
    arr <- as.numeric(unlist(strsplit(lines[2], " ")))
  } else {
    # Read from console for testing
    cat("Enter sequence (space-separated): ")
    input <- readline()
    arr <- as.numeric(unlist(strsplit(input, " ")))
  }
  return(arr)
}

# Main execution
# Example usage:
# arr <- c(1, 3, 5, 2, 4, 6)
# result <- count_inversions(arr)
# print(result)

# For Rosalind problem specifically:
# Read the input file (assuming it contains the array in the second line)
# arr <- read_input("rosalind_inv.txt")
# result <- count_inversions(arr)
# print(result)
```

## Example Usage

```r
# Test with example array
test_array <- c(1, 3, 5, 2, 4, 6)
print(paste("Number of inversions:", count_inversions(test_array)))

# Expected output: 3
# Inversions: (3,2), (5,2), (5,4)
```

## Time Complexity
- **Merge Sort Approach**: O(n log n) - more efficient for large arrays
- **Simple Approach**: O(n²) - simpler but slower for large inputs

## Space Complexity
- O(n) for the merge sort approach due to temporary arrays

## How it works:
1. **Divide**: Split the array into two halves
2. **Conquer**: Recursively count inversions in each half
3. **Combine**: Count inversions between the two halves during merge
4. **Return**: Sum of all inversions

The merge sort approach is preferred for larger datasets as it's more efficient than the brute force O(n²) method.

