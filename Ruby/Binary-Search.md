# Rosalind Problem: Binary Search - Ruby Solution

## Problem Understanding

The binary search problem requires finding the position of a target value in a sorted array. If the target exists, return its 1-based index; otherwise, return -1.

## Solution

```ruby
def binary_search(sorted_array, target)
  left = 0
  right = sorted_array.length - 1
  
  while left <= right
    mid = (left + right) / 2
    
    if sorted_array[mid] == target
      return mid + 1  # Convert to 1-based indexing
    elsif sorted_array[mid] < target
      left = mid + 1
    else
      right = mid - 1
    end
  end
  
  return -1
end

# Read input from file or stdin
def solve_binary_search(input_file = nil)
  if input_file
    lines = File.readlines(input_file).map(&:chomp)
  else
    lines = []
    begin
      while line = gets
        lines << line.chomp
      end
    rescue EOFError
      # End of input
    end
  end
  
  # Parse the input
  n = lines[0].to_i
  array = lines[1].split.map(&:to_i)
  k = lines[2].to_i
  targets = lines[3].split.map(&:to_i)
  
  # Find positions for each target
  results = []
  targets.each do |target|
    position = binary_search(array, target)
    results << position
  end
  
  # Return results as space-separated string
  results.join(" ")
end

# Alternative: Direct solution for stdin input
def direct_binary_search
  n = gets.to_i
  array = gets.split.map(&:to_i)
  k = gets.to_i
  targets = gets.split.map(&:to_i)
  
  results = []
  targets.each do |target|
    left = 0
    right = array.length - 1
    found = false
    position = -1
    
    while left <= right && !found
      mid = (left + right) / 2
      
      if array[mid] == target
        position = mid + 1  # 1-based indexing
        found = true
      elsif array[mid] < target
        left = mid + 1
      else
        right = mid - 1
      end
    end
    
    results << position
  end
  
  puts results.join(" ")
end

# Example usage:
# direct_binary_search
```

## Explanation

1. **Binary Search Algorithm**: The function implements classic binary search with:
   - Two pointers (`left` and `right`) defining the search range
   - Midpoint calculation to divide the search space
   - Comparison logic to narrow down the search

2. **Index Conversion**: Since the problem requires 1-based indexing, we add 1 to the zero-based array index

3. **Edge Cases Handled**:
   - Target not found: returns -1
   - Empty array: handled by the while loop condition
   - Single element array: works correctly

4. **Time Complexity**: O(log n) where n is the size of the array
5. **Space Complexity**: O(1) for the iterative implementation

## Sample Input/Output

**Input:**
```
5
1 2 3 4 5
2
3 1
```

**Output:**
```
3 1
```

The algorithm correctly finds that:
- Target 3 is at position 3 (1-based)
- Target 1 is at position 1 (1-based)

