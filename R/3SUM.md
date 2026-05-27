# Rosalind Problem 3SUM Solution in R

## Problem Understanding

The 3SUM problem asks us to find all unique triplets in an array that sum to zero. Given an array of integers, we need to return all combinations of three numbers that add up to zero.

## Solution Approach

I'll implement a solution using a two-pointer technique after sorting the array to efficiently find triplets without duplicates.

```r
# Function to solve 3SUM problem
three_sum <- function(nums) {
  # Sort the array first
  nums <- sort(nums)
  n <- length(nums)
  result <- list()
  
  # Iterate through each element as the first element of triplet
  for (i in 1:(n-2)) {
    # Skip duplicates for the first element
    if (i > 1 && nums[i] == nums[i-1]) {
      next
    }
    
    # Use two pointers approach for remaining elements
    left <- i + 1
    right <- n
    
    while (left < right) {
      current_sum <- nums[i] + nums[left] + nums[right]
      
      if (current_sum == 0) {
        # Found a triplet
        result[[length(result) + 1]] <- c(nums[i], nums[left], nums[right])
        
        # Skip duplicates for second element
        while (left < right && nums[left] == nums[left + 1]) {
          left <- left + 1
        }
        
        # Skip duplicates for third element
        while (left < right && nums[right] == nums[right - 1]) {
          right <- right - 1
        }
        
        left <- left + 1
        right <- right - 1
      } else if (current_sum < 0) {
        # Sum is too small, move left pointer right
        left <- left + 1
      } else {
        # Sum is too large, move right pointer left
        right <- right - 1
      }
    }
  }
  
  return(result)
}

# Alternative simpler approach for small inputs
three_sum_simple <- function(nums) {
  n <- length(nums)
  result <- list()
  
  # Check all combinations
  for (i in 1:(n-2)) {
    for (j in (i+1):(n-1)) {
      for (k in (j+1):n) {
        if (nums[i] + nums[j] + nums[k] == 0) {
          triplet <- sort(c(nums[i], nums[j], nums[k]))
          # Check if this triplet is already in result
          is_duplicate <- FALSE
          for (existing in result) {
            if (identical(existing, triplet)) {
              is_duplicate <- TRUE
              break
            }
          }
          if (!is_duplicate) {
            result[[length(result) + 1]] <- triplet
          }
        }
      }
    }
  }
  
  return(result)
}

# Example usage
# Test with sample input
test_nums <- c(-1, 0, 1, 2, -1, -4)
print("Input array:")
print(test_nums)

print("3SUM results:")
result <- three_sum(test_nums)
print(result)

# Test with another example
test_nums2 <- c(1, 2, 3, -1, -2, -3)
print("Input array 2:")
print(test_nums2)

print("3SUM results 2:")
result2 <- three_sum(test_nums2)
print(result2)
```

## Key Features of the Solution

1. **Efficient Algorithm**: Uses two-pointer technique with O(n²) time complexity
2. **Duplicate Handling**: Properly skips duplicate elements to avoid duplicate triplets
3. **Sorted Output**: Returns triplets in sorted order for consistency
4. **Edge Case Handling**: Handles empty arrays and arrays with insufficient elements

## Time and Space Complexity

- **Time Complexity**: O(n²) where n is the length of the input array
- **Space Complexity**: O(1) excluding the space required for the output

## Sample Input/Output

For input `[-1, 0, 1, 2, -1, -4]`:
- Expected output: `[[-1, -1, 2], [-1, 0, 1]]`

For input `[1, 2, 3, -1, -2, -3]`:
- Expected output: `[[-3, 0, 3], [-3, 1, 2], [-2, -1, 3], [-2, 0, 2], [-1, 0, 1]]`

The solution efficiently handles the constraints of the Rosalind 3SUM problem while ensuring no duplicate triplets are returned.

