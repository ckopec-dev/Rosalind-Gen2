# Rosalind Problem 2-Way Partition Solution in Ruby

## Problem Understanding

The 2-Way Partition problem requires us to partition an array around a pivot element such that:
- All elements less than the pivot come before all elements greater than or equal to the pivot
- The pivot element ends up in its correct sorted position

## Solution Approach

We'll use a two-pointer technique to partition the array in-place:
1. Use a left pointer starting from the beginning
2. Use a right pointer starting from the end
3. Move left pointer forward while elements are less than pivot
4. Move right pointer backward while elements are greater than or equal to pivot
5. Swap elements when both pointers find elements that need to be swapped
6. Continue until pointers meet

## Ruby Implementation

```ruby
def two_way_partition(array, pivot_index)
  pivot = array[pivot_index]
  left = 0
  right = array.length - 1
  
  while left <= right
    # Move left pointer while element is less than pivot
    while left <= right && array[left] < pivot
      left += 1
    end
    
    # Move right pointer while element is greater than or equal to pivot
    while left <= right && array[right] >= pivot
      right -= 1
    end
    
    # Swap elements if pointers haven't crossed
    if left < right
      array[left], array[right] = array[right], array[left]
      left += 1
      right -= 1
    end
  end
  
  array
end

# Alternative cleaner implementation using partition method
def two_way_partition_clean(array, pivot_index)
  pivot = array[pivot_index]
  
  # Partition the array using Ruby's built-in partition method
  less_than_pivot = array.select { |x| x < pivot }
  greater_or_equal = array.select { |x| x >= pivot }
  
  # Combine the partitions
  result = less_than_pivot + greater_or_equal
  
  # Copy back to original array
  array.replace(result)
  
  array
end

# More efficient in-place version
def two_way_partition_efficient(array, pivot_index)
  pivot = array[pivot_index]
  i = 0
  
  # Move all elements less than pivot to the left
  (0...array.length).each do |j|
    if array[j] < pivot
      array[i], array[j] = array[j], array[i]
      i += 1
    end
  end
  
  array
end

# Main solution function
def solve_two_way_partition(input_array, pivot_index)
  # For the given problem, we want to partition around the first element
  # as that's typically how it's defined in Rosalind problems
  if pivot_index == 0
    two_way_partition_efficient(input_array, pivot_index)
  else
    two_way_partition(input_array, pivot_index)
  end
end

# Example usage:
# Given array and pivot index
array = [5, 8, 1, 3, 9, 2, 7, 4, 6]
pivot_index = 0  # Partition around first element (5)

puts "Original array: #{array}"
puts "Pivot index: #{pivot_index}"
puts "Pivot value: #{array[pivot_index]}"

result = solve_two_way_partition(array, pivot_index)
puts "Partitioned array: #{result}"

# Test with sample data
def test_two_way_partition
  # Test case 1
  test_array1 = [5, 8, 1, 3, 9, 2, 7, 4, 6]
  result1 = two_way_partition_efficient(test_array1, 0)
  puts "Test 1 - Expected: elements < 5 on left, >= 5 on right"
  puts "Result: #{result1}"
  
  # Test case 2  
  test_array2 = [3, 8, 2, 5, 1, 4, 7, 6]
  result2 = two_way_partition_efficient(test_array2, 3)  # Pivot at index 3 (value 5)
  puts "Test 2 - Expected: elements < 5 on left, >= 5 on right"
  puts "Result: #{result2}"
end

# Run tests
test_two_way_partition
```

## Sample Input/Output

**Input:**
```
Array: [5, 8, 1, 3, 9, 2, 7, 4, 6]
Pivot Index: 0
```

**Output:**
```
[1, 3, 2, 4, 9, 5, 7, 8, 6]
```

## Key Features of the Solution

1. **In-place partitioning**: Modifies the original array without using extra space
2. **Time complexity**: O(n) where n is the length of the array
3. **Space complexity**: O(1) for the in-place version
4. **Handles edge cases**: Works with empty arrays, single elements, and arrays with duplicate elements
5. **Flexible pivot selection**: Can partition around any specified index

## Rosalind-Specific Considerations

For Rosalind problems, make sure to:
- Read input from standard input or file
- Output the result in the required format
- Handle multiple test cases if specified
- Follow the exact problem requirements for pivot selection

The solution above provides a clean, efficient implementation that can be easily adapted for specific Rosalind input/output requirements.

