# Rosalind Problem: Rabbits and Recurrence Relations

## Problem Statement
A recurrence relation is a way of defining a sequence of numbers recursively. The Fibonacci sequence is a classic example where each number is the sum of the two preceding numbers.

In this problem, we need to compute the number of rabbit pairs after n months, where:
- Each pair of rabbits produces a new pair every month
- Each pair becomes fertile after 1 month
- Rabbits live forever
- We start with 1 pair of rabbits

This is essentially the Fibonacci sequence.

## Solution

```ruby
def fibonacci_rabbits(n, k)
  # Base cases
  if n == 1 || n == 2
    return 1
  end
  
  # For each month, the number of pairs is:
  # (previous month pairs) + (pairs from previous month * k offspring)
  # But since each pair produces k offspring, and only pairs from previous-1 month 
  # can reproduce, we have:
  # F(n) = F(n-1) + k * F(n-2)
  
  prev2 = 1  # F(1)
  prev1 = 1  # F(2)
  
  (3..n).each do |i|
    current = prev1 + k * prev2
    prev2 = prev1
    prev1 = current
  end
  
  prev1
end

# Alternative implementation using array for clarity
def fibonacci_rabbits_array(n, k)
  if n == 1 || n == 2
    return 1
  end
  
  # Create array to store results
  fib = Array.new(n + 1)
  fib[1] = 1
  fib[2] = 1
  
  (3..n).each do |i|
    fib[i] = fib[i-1] + k * fib[i-2]
  end
  
  fib[n]
end

# Read input from file or stdin
def solve_rabbits(input_file = nil)
  if input_file
    input = File.read(input_file).strip
  else
    input = gets.strip
  end
  
  n, k = input.split.map(&:to_i)
  result = fibonacci_rabbits(n, k)
  puts result
  result
end

# Example usage:
# solve_rabbits("rosalind_fib.txt")  # For file input
# solve_rabbits()  # For stdin input

# Test with example values
puts "Test case 1: n=5, k=3"
puts fibonacci_rabbits(5, 3)  # Expected: 19

puts "Test case 2: n=1, k=1"
puts fibonacci_rabbits(1, 1)  # Expected: 1

puts "Test case 3: n=2, k=1"
puts fibonacci_rabbits(2, 1)  # Expected: 1
```

## Explanation

The problem asks us to calculate the number of rabbit pairs after `n` months, where each pair produces `k` offspring pairs every month (starting from the second month).

The recurrence relation is:
- F(1) = 1 (initial pair)
- F(2) = 1 (initial pair, now fertile)
- F(n) = F(n-1) + k × F(n-2) for n > 2

Where:
- F(n) represents the number of rabbit pairs in month n
- k represents the number of offspring pairs produced by each fertile pair per month

## Key Points

1. **Base cases**: We start with 1 pair in month 1 and 1 pair in month 2
2. **Recurrence relation**: Each month's population equals the previous month plus k times the population from two months ago
3. **Space optimization**: The first implementation uses O(1) space instead of O(n) by only keeping track of the last two values
4. **Time complexity**: O(n) for the iterative approach

## Sample Input/Output

**Input**: `5 3`
**Output**: `19`

This represents 5 months with 3 offspring pairs per fertile pair.

The sequence would be: 1, 1, 4, 7, 19 (where each number is the previous plus 3 times the number before that).

