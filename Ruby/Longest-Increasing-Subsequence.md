# Rosalind Problem: Longest Increasing Subsequence

## Problem Statement
Given a permutation of integers, find the longest increasing subsequence.

## Solution

```ruby
def longest_increasing_subsequence(arr)
  return [] if arr.empty?
  
  n = arr.length
  # dp[i] stores the length of LIS ending at index i
  dp = Array.new(n, 1)
  # parent[i] stores the previous index in the LIS ending at i
  parent = Array.new(n, -1)
  
  # Fill dp array
  (1...n).each do |i|
    (0...i).each do |j|
      if arr[i] > arr[j] && dp[i] < dp[j] + 1
        dp[i] = dp[j] + 1
        parent[i] = j
      end
    end
  end
  
  # Find the index with maximum LIS length
  max_length = dp.max
  max_index = dp.index(max_length)
  
  # Reconstruct the LIS
  lis = []
  current = max_index
  
  while current != -1
    lis.unshift(arr[current])
    current = parent[current]
  end
  
  lis
end

# Read input from stdin
input = gets.chomp
arr = input.split.map(&:to_i)

# Solve and output result
result = longest_increasing_subsequence(arr)
puts result.join(" ")
```

## Explanation

This solution uses dynamic programming to find the longest increasing subsequence:

1. **Initialization**: 
   - `dp[i]` represents the length of the longest increasing subsequence ending at index `i`
   - `parent[i]` stores the previous index in the subsequence ending at `i`

2. **DP Transition**:
   - For each element at index `i`, check all previous elements at index `j`
   - If `arr[i] > arr[j]` and extending the subsequence ending at `j` gives a longer subsequence at `i`, update `dp[i]` and `parent[i]`

3. **Reconstruction**:
   - Find the index with maximum LIS length
   - Trace back using the parent array to reconstruct the actual subsequence

## Time and Space Complexity
- **Time Complexity**: O(n²) where n is the length of the input array
- **Space Complexity**: O(n) for the dp and parent arrays

## Example
For input `5 1 4 2 3`:
- The longest increasing subsequence is `1 2 3`
- Output: `1 2 3`

