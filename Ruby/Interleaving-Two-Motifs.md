# Rosalind Problem: Interleaving Two Motifs (INTER)

## Problem Description
Given two DNA strings s and t, find a shortest common supersequence of s and t, i.e., a shortest string that contains both s and t as subsequences.

## Solution Approach
This is a classic dynamic programming problem similar to finding the shortest common supersequence. We'll use a 2D DP table where `dp[i][j]` represents the length of the shortest common supersequence for the first `i` characters of string `s` and first `j` characters of string `t`.

## Ruby Implementation

```ruby
def shortest_common_supersequence(s, t)
  m, n = s.length, t.length
  
  # Create DP table
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Initialize base cases
  # If s is empty, we need j characters from t
  (0..n).each { |j| dp[0][j] = j }
  
  # If t is empty, we need i characters from s
  (0..m).each { |i| dp[i][0] = i }
  
  # Fill the DP table
  (1..m).each do |i|
    (1..n).each do |j|
      if s[i-1] == t[j-1]
        # Characters match, we can use one character
        dp[i][j] = dp[i-1][j-1] + 1
      else
        # Characters don't match, take minimum of two possibilities
        dp[i][j] = [dp[i-1][j], dp[i][j-1]].min + 1
      end
    end
  end
  
  # Reconstruct the actual supersequence
  result = []
  i, j = m, n
  
  while i > 0 && j > 0
    if s[i-1] == t[j-1]
      result << s[i-1]
      i -= 1
      j -= 1
    elsif dp[i-1][j] < dp[i][j-1]
      result << s[i-1]
      i -= 1
    else
      result << t[j-1]
      j -= 1
    end
  end
  
  # Add remaining characters
  while i > 0
    result << s[i-1]
    i -= 1
  end
  
  while j > 0
    result << t[j-1]
    j -= 1
  end
  
  result.reverse.join
end

# Read input from file or stdin
def solve_interleaving
  input = gets.chomp
  s = input
  t = gets.chomp
  
  result = shortest_common_supersequence(s, t)
  puts result
end

# Example usage:
# solve_interleaving
```

## Alternative Implementation (More Concise)

```ruby
def interleave_motifs(s, t)
  m, n = s.length, t.length
  
  # DP table for length calculation
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Base cases
  (0..n).each { |j| dp[0][j] = j }
  (0..m).each { |i| dp[i][0] = i }
  
  # Fill DP table
  (1..m).each do |i|
    (1..n).each do |j|
      if s[i-1] == t[j-1]
        dp[i][j] = dp[i-1][j-1] + 1
      else
        dp[i][j] = [dp[i-1][j], dp[i][j-1]].min + 1
      end
    end
  end
  
  # Reconstruct the supersequence
  result = []
  i, j = m, n
  
  while i > 0 && j > 0
    if s[i-1] == t[j-1]
      result << s[i-1]
      i -= 1
      j -= 1
    elsif dp[i-1][j] < dp[i][j-1]
      result << s[i-1]
      i -= 1
    else
      result << t[j-1]
      j -= 1
    end
  end
  
  # Add remaining characters
  result += s[0...i].reverse + t[0...j].reverse
  
  result.reverse.join
end

# Read input and solve
s = gets.chomp
t = gets.chomp
puts interleave_motifs(s, t)
```

## Example

**Input:**
```
ABCD
ACBD
```

**Output:**
```
ABCABD
```

## Explanation

1. **DP Table Construction**: We build a table where `dp[i][j]` represents the length of the shortest common supersequence for the first `i` characters of string `s` and first `j` characters of string `t`.

2. **Base Cases**: 
   - If one string is empty, the supersequence is the other string
   - `dp[0][j] = j` and `dp[i][0] = i`

3. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = min(dp[i-1][j], dp[i][j-1]) + 1`

4. **Reconstruction**: Backtrack through the DP table to build the actual supersequence.

The time complexity is O(m×n) and space complexity is O(m×n) where m and n are the lengths of the input strings.

