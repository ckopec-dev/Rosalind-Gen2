# Rosalind Problem: Compute the Number of Times a Pattern Appears in a Text

## Problem Statement
Given two strings `pattern` and `text`, we need to count how many times `pattern` appears in `text` as a substring, including overlapping occurrences.

## Solution

```ruby
def pattern_count(text, pattern)
  count = 0
  pattern_length = pattern.length
  text_length = text.length
  
  # Iterate through each possible starting position
  (0..text_length - pattern_length).each do |i|
    # Check if pattern matches at position i
    if text[i, pattern_length] == pattern
      count += 1
    end
  end
  
  count
end

# Read input from stdin
input = gets.chomp
pattern = gets.chomp

# Solve and print result
result = pattern_count(input, pattern)
puts result
```

## Alternative Implementation Using String Methods

```ruby
def pattern_count(text, pattern)
  count = 0
  start = 0
  
  # Find all occurrences using String#index
  while start <= text.length - pattern.length
    pos = text.index(pattern, start)
    break if pos.nil?
    count += 1
    start = pos + 1  # Move by 1 to find overlapping matches
  end
  
  count
end

# Read input
text = gets.chomp
pattern = gets.chomp

# Solve and print result
result = pattern_count(text, pattern)
puts result
```

## Example Usage

**Input:**
```
GATATATGCATATACTT
ATAT
```

**Output:**
```
3
```

**Explanation:**
The pattern "ATAT" appears at positions 1, 3, and 7 in the text "GATATATGCATATACTT", giving us a count of 3.

## Key Points

1. **Overlapping matches**: The solution counts overlapping occurrences (e.g., "AA" in "AAA" appears 2 times)
2. **Edge cases**: Handles empty strings and patterns longer than text
3. **Time complexity**: O(n×m) where n is text length and m is pattern length
4. **Space complexity**: O(1) additional space

The first implementation is more straightforward and readable, while the second uses Ruby's built-in string methods which might be more efficient for certain cases.

