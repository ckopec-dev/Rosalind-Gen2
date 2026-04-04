# Rosalind Problem: Implement_ChromosomeToCycle

## Problem Description
Convert a chromosome (a sequence of signed integers) into a cycle representation.

## Solution

```ruby
def chromosome_to_cycle(chromosome)
  # Initialize the cycle array with double the length of chromosome
  cycle = Array.new(chromosome.length * 2)
  
  # For each element in the chromosome
  (0...chromosome.length).each do |j|
    # Get the current element
    element = chromosome[j]
    
    # If element is positive
    if element > 0
      # Set the cycle positions
      cycle[2 * j] = 2 * element - 1   # First position in the pair
      cycle[2 * j + 1] = 2 * element   # Second position in the pair
    else
      # If element is negative
      cycle[2 * j] = -2 * element      # First position in the pair
      cycle[2 * j + 1] = -2 * element - 1 # Second position in the pair
    end
  end
  
  cycle
end

# Alternative more concise implementation
def chromosome_to_cycle_concise(chromosome)
  cycle = []
  
  chromosome.each_with_index do |element, j|
    if element > 0
      cycle << (2 * element - 1) << (2 * element)
    else
      cycle << (-2 * element) << (-2 * element - 1)
    end
  end
  
  cycle
end

# Example usage:
# chromosome = [1, -2, 3, 4, -5]
# result = chromosome_to_cycle(chromosome)
# puts result.inspect
# Output: [1, 2, 4, 3, 6, 5, 8, 7, 10, 9]
```

## Explanation

The algorithm converts a chromosome (sequence of signed integers) into a cycle representation:

1. **Input**: A chromosome represented as an array of signed integers
2. **Output**: A cycle represented as an array of integers

**Key Steps**:
1. For each element in the chromosome, we create a pair of consecutive integers
2. If the element is positive (e.g., 3), we map it to positions (5, 6) 
3. If the element is negative (e.g., -3), we map it to positions (6, 5)
4. The mapping follows the pattern where positive element `i` becomes `(2i-1, 2i)` and negative element `-i` becomes `(2i, 2i-1)`

**Example**:
- Input: `[1, -2, 3, 4, -5]`
- Process each element:
  - 1 → (1, 2)
  - -2 → (4, 3) 
  - 3 → (5, 6)
  - 4 → (7, 8)
  - -5 → (10, 9)
- Output: `[1, 2, 4, 3, 5, 6, 7, 8, 10, 9]`

The cycle representation is useful for representing genome rearrangements in computational biology problems.

