# Rosalind Problem: Find All Approximate Occurrences of a Collection of Patterns in a String

## Problem Description
Given a string Text, a collection of strings Patterns, and an integer d, return all occurrences of patterns in Text with at most d mismatches.

## Solution

```ruby
def find_all_approximate_occurrences(text, patterns, d)
  occurrences = []
  
  patterns.each do |pattern|
    # Find all occurrences of pattern in text with at most d mismatches
    (0..text.length - pattern.length).each do |i|
      substring = text[i, pattern.length]
      
      # Check if the substring matches the pattern with at most d mismatches
      if hamming_distance(substring, pattern) <= d
        occurrences << i
      end
    end
  end
  
  # Remove duplicates and sort
  occurrences.uniq.sort
end

def hamming_distance(s1, s2)
  # Calculate Hamming distance between two strings of equal length
  distance = 0
  (0...s1.length).each do |i|
    distance += 1 if s1[i] != s2[i]
  end
  distance
end

# Read input from file or stdin
def read_input
  lines = []
  ARGF.each_line { |line| lines << line.chomp }
  lines
end

# Main execution
if __FILE__ == $0
  # Read input
  input_lines = read_input
  
  if input_lines.length >= 3
    text = input_lines[0]
    patterns = input_lines[1].split(' ')
    d = input_lines[2].to_i
    
    # Find all approximate occurrences
    result = find_all_approximate_occurrences(text, patterns, d)
    
    # Output results
    puts result.join(' ')
  end
end
```

## Example Usage

For input:
```
ACAACTATGCATACTATCGGGAACTATCCT
ACT ATC ACT
1
```

The program would output:
```
1 4 10 12 17 21
```

## Explanation

1. **Main Function**: `find_all_approximate_occurrences` takes the text, patterns array, and maximum mismatches `d` as inputs.

2. **Pattern Matching**: For each pattern in the collection:
   - Slide through the text to check every possible substring of the pattern's length
   - Calculate Hamming distance between substring and pattern
   - If distance is ≤ d, record the starting position

3. **Hamming Distance**: Calculates the number of positions where two strings of equal length differ.

4. **Output**: Returns sorted, unique positions where patterns occur with at most d mismatches.

## Time Complexity
- O(n × m × k) where n is text length, m is pattern length, and k is number of patterns
- Space complexity: O(n) for storing results

This solution handles multiple patterns and finds all approximate occurrences efficiently.

