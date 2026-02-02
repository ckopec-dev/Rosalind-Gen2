# Rosalind Problem: Overlap Alignment in Ruby

## Problem Description
Given two strings `v` and `w` of equal length, find a maximum overlap alignment between them. An overlap alignment is a local alignment where the first string starts at position 1 and the second string ends at the last position.

## Solution

```ruby
def overlap_alignment(v, w)
  m = v.length
  n = w.length
  
  # Create scoring matrix
  score = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Initialize first row
  (0..n).each { |j| score[0][j] = -j }
  
  # Fill the scoring matrix
  (1..m).each do |i|
    (1..n).each do |j|
      match = score[i-1][j-1] + (v[i-1] == w[j-1] ? 1 : -1)
      delete = score[i-1][j] - 1
      insert = score[i][j-1] - 1
      
      score[i][j] = [match, delete, insert].max
    end
  end
  
  # Find maximum score in last row
  max_score = score[m].max
  max_col = score[m].index(max_score)
  
  # Traceback
  alignment_v = ""
  alignment_w = ""
  
  i = m
  j = max_col
  
  while i > 0 && j > 0
    current = score[i][j]
    diagonal = score[i-1][j-1]
    up = score[i-1][j]
    left = score[i][j-1]
    
    if current == diagonal + (v[i-1] == w[j-1] ? 1 : -1)
      alignment_v = v[i-1] + alignment_v
      alignment_w = w[j-1] + alignment_w
      i -= 1
      j -= 1
    elsif current == up - 1
      alignment_v = v[i-1] + alignment_v
      alignment_w = "-" + alignment_w
      i -= 1
    else
      alignment_v = "-" + alignment_v
      alignment_w = w[j-1] + alignment_w
      j -= 1
    end
  end
  
  # Handle remaining characters
  while i > 0
    alignment_v = v[i-1] + alignment_v
    alignment_w = "-" + alignment_w
    i -= 1
  end
  
  while j > 0
    alignment_v = "-" + alignment_v
    alignment_w = w[j-1] + alignment_w
    j -= 1
  end
  
  return max_score, alignment_v, alignment_w
end

# Read input from file or stdin
def read_input
  input = []
  while line = gets
    input << line.chomp
  end
  input
end

# Main execution
if __FILE__ == $0
  # Read input
  input = read_input
  
  if input.length >= 2
    v = input[0]
    w = input[1]
    
    score, alignment_v, alignment_w = overlap_alignment(v, w)
    
    puts score
    puts alignment_v
    puts alignment_w
  end
end
```

## Explanation

This solution implements the overlap alignment algorithm using dynamic programming:

1. **Matrix Initialization**: Create a scoring matrix of size (m+1) × (n+1) where m and n are the lengths of the two strings.

2. **Base Case**: Initialize the first row with negative values representing penalties for gaps at the beginning.

3. **Matrix Filling**: For each cell (i,j), calculate the maximum score from three possible operations:
   - Match/Mismatch: score[i-1][j-1] + (1 if characters match, -1 otherwise)
   - Deletion: score[i-1][j] - 1
   - Insertion: score[i][j-1] - 1

4. **Finding Maximum**: Look for the maximum score in the last row since we want the alignment to end at the end of the second string.

5. **Traceback**: Reconstruct the alignment by backtracking through the matrix from the maximum score position.

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the scoring matrix

## Example Usage
For input strings "PAWHEAE" and "HEAGAWGHEV":
```
Input:
PAWHEAE
HEAGAWGHEV

Output:
2
PAWHEAE
-HEAGAWGHEV
```

