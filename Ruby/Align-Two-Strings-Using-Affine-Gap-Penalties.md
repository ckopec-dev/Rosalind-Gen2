# Rosalind Problem: Align Two Strings Using Affine Gap Penalties

I'll solve this step-by-step using Ruby to implement sequence alignment with affine gap penalties.

## Problem Understanding

We need to find the optimal global alignment between two strings using affine gap penalties, where:
- Gap opening cost: `gap_open`
- Gap extension cost: `gap_extend`
- We'll use dynamic programming with three matrices (like the classic Hirschberg algorithm)

## Solution

```ruby
def align_two_strings_affine_gap(str1, str2, gap_open, gap_extend)
  m = str1.length
  n = str2.length
  
  # Create three matrices for affine gap penalties
  # F[i][j] - score for aligning str1[0..i-1] with str2[0..j-1]
  # Ix[i][j] - score for aligning str1[0..i-1] with str2[0..j-1] where str1[i] is in gap
  # Iy[i][j] - score for aligning str1[0..i-1] with str2[0..j-1] where str2[j] is in gap
  
  F = Array.new(m + 1) { Array.new(n + 1, 0) }
  Ix = Array.new(m + 1) { Array.new(n + 1, 0) }
  Iy = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Initialize base cases
  # First row
  for j in 1..n
    Ix[0][j] = -gap_open - (j - 1) * gap_extend
    F[0][j] = Ix[0][j]
    Iy[0][j] = -Float::INFINITY
  end
  
  # First column
  for i in 1..m
    Iy[i][0] = -gap_open - (i - 1) * gap_extend
    F[i][0] = Iy[i][0]
    Ix[i][0] = -Float::INFINITY
  end
  
  # Fill the matrices
  for i in 1..m
    for j in 1..n
      # Match/mismatch score
      match_score = (str1[i-1] == str2[j-1]) ? 1 : -1
      
      # Calculate scores for different operations
      # From F[i-1][j-1] - match/mismatch
      f_score = F[i-1][j-1] + match_score
      
      # From Ix[i-1][j-1] - gap in sequence 1 (extension)
      ix_score = Ix[i-1][j-1] + match_score
      
      # From Iy[i-1][j-1] - gap in sequence 2 (extension)
      iy_score = Iy[i-1][j-1] + match_score
      
      # Choose the maximum from match/mismatch operations
      F[i][j] = [f_score, ix_score, iy_score].max
      
      # Calculate gap scores
      # Gap opening in sequence 1
      i1_open = F[i-1][j] - gap_open
      i1_extend = Ix[i-1][j] - gap_extend
      Ix[i][j] = [i1_open, i1_extend].max
      
      # Gap opening in sequence 2
      i2_open = F[i][j-1] - gap_open
      i2_extend = Iy[i][j-1] - gap_extend
      Iy[i][j] = [i2_open, i2_extend].max
    end
  end
  
  # Backtrack to construct alignment
  alignment1 = ""
  alignment2 = ""
  i = m
  j = n
  
  while i > 0 || j > 0
    if i > 0 && j > 0
      # Check which matrix gave us the current score
      current_score = F[i][j]
      prev_match = F[i-1][j-1]
      prev_ix = Ix[i-1][j-1]
      prev_iy = Iy[i-1][j-1]
      
      # Check if we came from match/mismatch
      if current_score == prev_match + ((str1[i-1] == str2[j-1]) ? 1 : -1)
        alignment1 = str1[i-1] + alignment1
        alignment2 = str2[j-1] + alignment2
        i -= 1
        j -= 1
        next
      end
      
      # Check if we came from gap in sequence 1 (Ix)
      if current_score == Ix[i][j]
        if Ix[i][j] == Ix[i-1][j] - gap_extend
          # Extension
          alignment1 = str1[i-1] + alignment1
          alignment2 = "-" + alignment2
          i -= 1
        else
          # Opening
          alignment1 = str1[i-1] + alignment1
          alignment2 = "-" + alignment2
          i -= 1
        end
        next
      end
      
      # Check if we came from gap in sequence 2 (Iy)
      if current_score == Iy[i][j]
        if Iy[i][j] == Iy[i][j-1] - gap_extend
          # Extension
          alignment1 = "-" + alignment1
          alignment2 = str2[j-1] + alignment2
          j -= 1
        else
          # Opening
          alignment1 = "-" + alignment1
          alignment2 = str2[j-1] + alignment2
          j -= 1
        end
        next
      end
    elsif i > 0
      # Gap in sequence 2
      alignment1 = str1[i-1] + alignment1
      alignment2 = "-" + alignment2
      i -= 1
    elsif j > 0
      # Gap in sequence 1
      alignment1 = "-" + alignment1
      alignment2 = str2[j-1] + alignment2
      j -= 1
    end
  end
  
  # Return the score and alignment
  score = F[m][n]
  [score, alignment1, alignment2]
end

# Alternative cleaner implementation
def affine_gap_alignment(str1, str2, gap_open, gap_extend)
  m = str1.length
  n = str2.length
  
  # Create matrices
  F = Array.new(m + 1) { Array.new(n + 1, -Float::INFINITY) }
  X = Array.new(m + 1) { Array.new(n + 1, -Float::INFINITY) }
  Y = Array.new(m + 1) { Array.new(n + 1, -Float::INFINITY) }
  
  # Initialize
  F[0][0] = 0
  X[0][0] = -gap_open
  Y[0][0] = -gap_open
  
  # Fill first row and column
  for j in 1..n
    X[0][j] = -gap_open - j * gap_extend
    Y[0][j] = -Float::INFINITY
    F[0][j] = X[0][j]
  end
  
  for i in 1..m
    X[i][0] = -Float::INFINITY
    Y[i][0] = -gap_open - i * gap_extend
    F[i][0] = Y[i][0]
  end
  
  # Fill matrices using dynamic programming
  for i in 1..m
    for j in 1..n
      match_score = (str1[i-1] == str2[j-1]) ? 1 : -1
      
      # F[i][j] - match/mismatch
      F[i][j] = [F[i-1][j-1] + match_score, 
                 X[i-1][j-1] + match_score, 
                 Y[i-1][j-1] + match_score].max
      
      # X[i][j] - gap in sequence 1
      X[i][j] = [F[i-1][j] - gap_open, X[i-1][j] - gap_extend].max
      
      # Y[i][j] - gap in sequence 2
      Y[i][j] = [F[i][j-1] - gap_open, Y[i][j-1] - gap_extend].max
    end
  end
  
  # Backtrack to get alignment
  alignment1 = ""
  alignment2 = ""
  i = m
  j = n
  
  while i > 0 || j > 0
    if i > 0 && j > 0
      match_score = (str1[i-1] == str2[j-1]) ? 1 : -1
      
      # Check which path we took
      current = F[i][j]
      from_match = F[i-1][j-1] + match_score
      from_x = X[i-1][j-1] + match_score
      from_y = Y[i-1][j-1] + match_score
      
      if current == from_match
        alignment1 = str1[i-1] + alignment1
        alignment2 = str2[j-1] + alignment2
        i -= 1
        j -= 1
      elsif current == from_x
        alignment1 = str1[i-1] + alignment1
        alignment2 = "-" + alignment2
        i -= 1
      else
        alignment1 = "-" + alignment1
        alignment2 = str2[j-1] + alignment2
        j -= 1
      end
    elsif i > 0
      alignment1 = str1[i-1] + alignment1
      alignment2 = "-" + alignment2
      i -= 1
    else
      alignment1 = "-" + alignment1
      alignment2 = str2[j-1] + alignment2
      j -= 1
    end
  end
  
  [F[m][n], alignment1, alignment2]
end

# Example usage:
# str1 = "ACGTACGT"
# str2 = "ACGTACGT"
# gap_open = 5
# gap_extend = 2
# score, align1, align2 = affine_gap_alignment(str1, str2, gap_open, gap_extend)
# puts "Score: #{score}"
# puts "Alignment 1: #{align1}"
# puts "Alignment 2: #{align2}"
```

## Explanation

This solution implements the standard affine gap penalty alignment algorithm using three matrices:

1. **Matrix F**: Scores for match/mismatch operations
2. **Matrix X**: Scores for gaps in sequence 1 (with affine penalties)
3. **Matrix Y**: Scores for gaps in sequence 2 (with affine penalties)

### Key Components:

- **Initialization**: Base cases for first row/column with gap penalties
- **Dynamic Programming**: Fill matrices using recurrence relations
- **Backtracking**: Reconstruct optimal alignment by following the path

### Time Complexity:
- Time: O(m×n) where m and n are string lengths
- Space: O(m×n) for the three matrices

### Space Optimization:
The algorithm can be optimized to O(m+n) space using the Hirschberg algorithm, but this implementation focuses on clarity and correctness.

This solution handles the affine gap penalty model where gaps have an opening cost and an extension cost, making it suitable for biological sequence alignment problems.

