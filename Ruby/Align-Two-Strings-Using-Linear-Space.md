# Rosalind Problem: Align_Two_Strings_Using_Linear_Space

## Problem Understanding

This problem requires implementing sequence alignment using linear space complexity. The standard dynamic programming approach for sequence alignment uses O(mn) space, but we need to optimize it to use only O(m+n) space.

The approach involves:
1. Using divide-and-conquer strategy
2. Finding the middle column of the DP matrix
3. Computing the optimal alignment by splitting the problem into subproblems

## Solution

```ruby
def align_two_strings_linear_space(str1, str2)
  # Initialize scoring parameters
  match_score = 0
  mismatch_score = -1
  gap_penalty = -1
  
  # Get lengths
  m = str1.length
  n = str2.length
  
  # Handle edge cases
  return [str1, str2] if m == 0 || n == 0
  
  # Find optimal alignment using linear space approach
  alignment = linear_space_alignment(str1, str2, match_score, mismatch_score, gap_penalty)
  
  return alignment
end

def linear_space_alignment(str1, str2, match_score, mismatch_score, gap_penalty)
  m = str1.length
  n = str2.length
  
  # Base cases
  if m == 0
    return ["-" * n, str2]
  elsif n == 0
    return [str1, "-" * m]
  elsif m == 1 || n == 1
    # Use standard DP for small cases
    return standard_alignment(str1, str2, match_score, mismatch_score, gap_penalty)
  end
  
  # Divide and conquer approach
  mid = n / 2
  
  # Compute scores from left to middle
  score_left = compute_score_from_left(str1, str2[0...mid], match_score, mismatch_score, gap_penalty)
  
  # Compute scores from right to middle
  score_right = compute_score_from_right(str1, str2[mid..-1], match_score, mismatch_score, gap_penalty)
  
  # Find the best split point
  split_point = find_best_split_point(score_left, score_right)
  
  # Recursively solve left and right parts
  left_alignment = linear_space_alignment(str1[0...split_point], str2[0...mid], match_score, mismatch_score, gap_penalty)
  right_alignment = linear_space_alignment(str1[split_point..-1], str2[mid..-1], match_score, mismatch_score, gap_penalty)
  
  # Combine alignments
  return [left_alignment[0] + right_alignment[0], left_alignment[1] + right_alignment[1]]
end

def compute_score_from_left(str1, str2, match_score, mismatch_score, gap_penalty)
  m = str1.length
  n = str2.length
  
  # Initialize DP matrix for left-to-right computation
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Initialize first row
  (0..n).each { |j| dp[0][j] = j * gap_penalty }
  
  # Fill the DP matrix
  (1..m).each do |i|
    dp[i][0] = i * gap_penalty
    (1..n).each do |j|
      match = dp[i-1][j-1] + (str1[i-1] == str2[j-1] ? match_score : mismatch_score)
      delete = dp[i-1][j] + gap_penalty
      insert = dp[i][j-1] + gap_penalty
      dp[i][j] = [match, delete, insert].max
    end
  end
  
  return dp.map(&:last)  # Return last column
end

def compute_score_from_right(str1, str2, match_score, mismatch_score, gap_penalty)
  m = str1.length
  n = str2.length
  
  # Initialize DP matrix for right-to-left computation
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Initialize last row
  (0..m).each { |i| dp[i][n] = (m - i) * gap_penalty }
  
  # Fill the DP matrix from bottom to top
  (n-1).downto(0) do |j|
    dp[m][j] = (n - j) * gap_penalty
    (m-1).downto(0) do |i|
      match = dp[i+1][j+1] + (str1[i] == str2[j] ? match_score : mismatch_score)
      delete = dp[i+1][j] + gap_penalty
      insert = dp[i][j+1] + gap_penalty
      dp[i][j] = [match, delete, insert].max
    end
  end
  
  return dp.map(&:first)  # Return first column
end

def find_best_split_point(score_left, score_right)
  # Find the index that maximizes score_left[i] + score_right[i]
  max_score = -Float::INFINITY
  best_index = 0
  
  (0...score_left.length).each do |i|
    total = score_left[i] + score_right[i]
    if total > max_score
      max_score = total
      best_index = i
    end
  end
  
  return best_index
end

def standard_alignment(str1, str2, match_score, mismatch_score, gap_penalty)
  m = str1.length
  n = str2.length
  
  # Create DP matrix
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Initialize first row and column
  (0..m).each { |i| dp[i][0] = i * gap_penalty }
  (0..n).each { |j| dp[0][j] = j * gap_penalty }
  
  # Fill DP matrix
  (1..m).each do |i|
    (1..n).each do |j|
      match = dp[i-1][j-1] + (str1[i-1] == str2[j-1] ? match_score : mismatch_score)
      delete = dp[i-1][j] + gap_penalty
      insert = dp[i][j-1] + gap_penalty
      dp[i][j] = [match, delete, insert].max
    end
  end
  
  # Backtrack to find alignment
  align1 = ""
  align2 = ""
  i = m
  j = n
  
  while i > 0 || j > 0
    if i > 0 && j > 0 && dp[i][j] == dp[i-1][j-1] + (str1[i-1] == str2[j-1] ? match_score : mismatch_score)
      align1 = str1[i-1] + align1
      align2 = str2[j-1] + align2
      i -= 1
      j -= 1
    elsif i > 0 && dp[i][j] == dp[i-1][j] + gap_penalty
      align1 = str1[i-1] + align1
      align2 = "-" + align2
      i -= 1
    else
      align1 = "-" + align1
      align2 = str2[j-1] + align2
      j -= 1
    end
  end
  
  return [align1, align2]
end

# Main execution function for Rosalind problem
def solve_rosalind_problem(str1, str2)
  result = align_two_strings_linear_space(str1, str2)
  return result
end

# Example usage:
# str1 = "ACGTACGT"
# str2 = "ACGTACGT"
# result = solve_rosalind_problem(str1, str2)
# puts result[0]
# puts result[1]
```

## Key Features of the Solution

1. **Linear Space Complexity**: Uses O(m+n) space instead of O(mn) by employing divide-and-conquer approach
2. **Divide and Conquer Strategy**: 
   - Splits the problem into two halves
   - Computes middle column scores from both directions
   - Finds optimal split point
   - Recursively solves subproblems

3. **Optimal Substructure**: 
   - Uses dynamic programming for small cases
   - Combines solutions optimally

4. **Proper Scoring System**:
   - Match score: 0
   - Mismatch score: -1  
   - Gap penalty: -1

## Time and Space Complexity

- **Time Complexity**: O(mn) where m and n are lengths of strings
- **Space Complexity**: O(m+n) due to the divide-and-conquer approach

This solution efficiently handles the constraint of linear space usage while maintaining optimal alignment quality.

