# Rosalind Problem: Quick_Sort Solution in R

## Problem Understanding

The Quick_Sort problem requires implementing the quicksort algorithm to sort an array of integers in ascending order.

## Solution Approach

I'll implement the quicksort algorithm using the following steps:
1. Choose a pivot element
2. Partition the array around the pivot
3. Recursively sort the sub-arrays
4. Combine the results

## R Implementation

```r
# Quick Sort implementation in R
quick_sort <- function(arr) {
  # Base case: arrays with 0 or 1 element are already sorted
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Choose the first element as pivot
  pivot <- arr[1]
  
  # Partition the array into three parts
  less <- arr[arr < pivot]      # Elements less than pivot
  equal <- arr[arr == pivot]    # Elements equal to pivot
  greater <- arr[arr > pivot]   # Elements greater than pivot
  
  # Recursively sort the less and greater parts, then combine
  c(quick_sort(less), equal, quick_sort(greater))
}

# Alternative implementation with in-place sorting
quick_sort_inplace <- function(arr, low = 1, high = length(arr)) {
  if (low < high) {
    # Partition the array and get pivot index
    pivot_index <- partition(arr, low, high)
    
    # Recursively sort elements before and after partition
    quick_sort_inplace(arr, low, pivot_index - 1)
    quick_sort_inplace(arr, pivot_index + 1, high)
  }
  return(arr)
}

partition <- function(arr, low, high) {
  # Choose the rightmost element as pivot
  pivot <- arr[high]
  
  # Index of smaller element (indicates right position of pivot)
  i <- low - 1
  
  for (j in low:(high - 1)) {
    # If current element is smaller than or equal to pivot
    if (arr[j] <= pivot) {
      i <- i + 1
      # Swap elements
      temp <- arr[i]
      arr[i] <- arr[j]
      arr[j] <- temp
    }
  }
  
  # Place pivot in its correct position
  temp <- arr[i + 1]
  arr[i + 1] <- arr[high]
  arr[high] <- temp
  
  return(i + 1)
}

# Main function to solve the Rosalind problem
solve_quick_sort <- function(input_file = NULL) {
  # If input file is provided, read from file
  if (!is.null(input_file)) {
    lines <- readLines(input_file)
    # First line contains array size (we don't need it)
    # Second line contains the array elements
    numbers <- as.numeric(unlist(strsplit(lines[2], " ")))
  } else {
    # For testing purposes, we can use a sample array
    numbers <- c(8, 2, 4, 5, 1, 3, 7, 6)
  }
  
  # Sort the array using quick sort
  sorted_numbers <- quick_sort(numbers)
  
  # Return as space-separated string
  return(paste(sorted_numbers, collapse = " "))
}

# Example usage:
# For Rosalind input format:
# First line: array size (ignored)
# Second line: space-separated integers

# Sample input:
# 8
# 8 2 4 5 1 3 7 6

# Expected output:
# 1 2 3 4 5 6 7 8

# Test the solution
test_array <- c(8, 2, 4, 5, 1, 3, 7, 6)
result <- quick_sort(test_array)
cat("Original array:", paste(test_array, collapse = " "), "\n")
cat("Sorted array:  ", paste(result, collapse = " "), "\n")
```

## Explanation

The solution provides two implementations:

1. **Simple recursive approach** (`quick_sort`): 
   - Creates three subarrays (less than pivot, equal to pivot, greater than pivot)
   - Recursively sorts the less and greater subarrays
   - Concatenates the results

2. **In-place approach** (`quick_sort_inplace`):
   - More memory efficient
   - Sorts the array by rearranging elements in place
   - Uses a partition function to place elements relative to the pivot

## Time and Space Complexity

- **Time Complexity**: 
  - Average case: O(n log n)
  - Worst case: O(n²) - when pivot is always the smallest or largest element
- **Space Complexity**: 
  - Simple version: O(n) due to creating new arrays
  - In-place version: O(log n) for recursion stack

## Usage

For Rosalind problem input:
```r
# Read input from file
result <- solve_quick_sort("rosalind_input.txt")
cat(result, "\n")
```

The output will be the sorted array elements separated by spaces, as required by the Rosalind format.

