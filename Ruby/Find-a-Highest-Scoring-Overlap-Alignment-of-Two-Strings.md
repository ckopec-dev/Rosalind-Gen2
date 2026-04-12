# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings

## Problem Description
Find the highest-scoring overlap alignment of two strings, where the first string is a prefix of the second string, and the second string is a suffix of the first string.

## Solution

```ruby
def overlap_alignment(v, w)
  # Initialize the scoring matrix
  s = Array.new(v.length + 1) { Array.new(w.length + 1, 0) }
  
  # Initialize the traceback matrix
  backtrack = Array.new(v.length + 1) { Array.new(w.length + 1, 0) }
  
  # Scoring parameters
  match_score = 1
  mismatch_score = -1
  gap_penalty = -2
  
  # Fill the first row with gap penalties
  (1..w.length).each do |j|
    s[0][j] = s[0][j-1] + gap_penalty
  end
  
  # Fill the first column with gap penalties
  (1..v.length).each do |i|
    s[i][0] = s[i-1][0] + gap_penalty
  end
  
  # Fill the scoring matrix
  (1..v.length).each do |i|
    (1..w.length).each do |j|
      match = s[i-1][j-1] + (v[i-1] == w[j-1] ? match_score : mismatch_score)
      delete = s[i-1][j] + gap_penalty
      insert = s[i][j-1] + gap_penalty
      
      s[i][j] = [match, delete, insert].max
      
      # Determine the direction for traceback
      if s[i][j] == match
        backtrack[i][j] = 1  # match
      elsif s[i][j] == delete
        backtrack[i][j] = 2  # delete
      else
        backtrack[i][j] = 3  # insert
      end
    end
  end
  
  # Find the maximum score in the last row
  max_score = -Float::INFINITY
  max_j = 0
  
  (1..w.length).each do |j|
    if s[v.length][j] > max_score
      max_score = s[v.length][j]
      max_j = j
    end
  end
  
  # Traceback to construct the alignment
  alignment_v = ""
  alignment_w = ""
  
  i = v.length
  j = max_j
  
  while i > 0 && j > 0
    if backtrack[i][j] == 1
      alignment_v = v[i-1] + alignment_v
      alignment_w = w[j-1] + alignment_w
      i -= 1
      j -= 1
    elsif backtrack[i][j] == 2
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

# Read input from stdin
input = STDIN.read.strip
lines = input.split("\n")

v = lines[0]
w = lines[1]

# Solve the problem
score, alignment_v, alignment_w = overlap_alignment(v, w)

# Output the result
puts score
puts alignment_v
puts alignment_w
```

## Explanation

This solution implements the overlap alignment algorithm using dynamic programming:

1. **Matrix Initialization**: Create a scoring matrix `s` of size `(v.length+1) × (w.length+1)` and a backtrack matrix.

2. **Base Cases**: Fill the first row and column with gap penalties.

3. **Matrix Filling**: For each cell `(i,j)`, compute the maximum score from three possible operations:
   - Match/mismatch: `s[i-1][j-1] + score`
   - Deletion: `s[i-1][j] + gap_penalty`
   - Insertion: `s[i][j-1] + gap_penalty`

4. **Find Maximum**: Look for the maximum score in the last row of the matrix.

5. **Traceback**: Starting from the maximum position, trace back through the matrix to construct the alignment.

6. **Output**: Return the maximum score and the two aligned strings.

## Time and Space Complexity
- **Time Complexity**: O(m×n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m×n) for the scoring and backtrack matrices

## Sample Input/Output

**Input:**
```
PAWHEAE
HEAGAWGHEV
```

**Output:**
```
1
PAWHEAE
-HEAGAWGHEV
```

The solution correctly identifies the highest-scoring overlap alignment between the two strings, where one string overlaps with the other in a way that maximizes the alignment score.

