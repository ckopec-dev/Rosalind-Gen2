# Rosalind Problem: Find a Longest Common Subsequence of Two Strings

## Problem Understanding

We need to find the longest common subsequence (LCS) between two strings. A subsequence is a sequence that can be derived from another sequence by deleting some or no elements without changing the order of the remaining elements.

## Approach

I'll use dynamic programming to solve this problem:
1. Create a 2D table where `dp[i][j]` represents the length of LCS for the first `i` characters of string1 and first `j` characters of string2
2. Fill the table using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. Backtrack through the table to construct the actual LCS

## Solution

```ruby
def longest_common_subsequence(str1, str2)
  m = str1.length
  n = str2.length
  
  # Create DP table
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  # Fill the DP table
  (1..m).each do |i|
    (1..n).each do |j|
      if str1[i-1] == str2[j-1]
        dp[i][j] = dp[i-1][j-1] + 1
      else
        dp[i][j] = [dp[i-1][j], dp[i][j-1]].max
      end
    end
  end
  
  # Backtrack to find the actual LCS
  lcs = []
  i = m
  j = n
  
  while i > 0 && j > 0
    if str1[i-1] == str2[j-1]
      lcs << str1[i-1]
      i -= 1
      j -= 1
    elsif dp[i-1][j] > dp[i][j-1]
      i -= 1
    else
      j -= 1
    end
  end
  
  # Reverse since we built it backwards
  lcs.reverse.join
end

# Read input from stdin
input = gets.chomp
str1 = gets.chomp
str2 = gets.chomp

# Solve and output
result = longest_common_subsequence(str1, str2)
puts result
```

## Example

For input:
```
AACCTTGG
ACACTGTGA
```

The LCS would be: `AACTTG`

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Alternative Compact Version

```ruby
def lcs(s1, s2)
  m, n = s1.length, s2.length
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  
  (1..m).each { |i| (1..n).each { |j| 
    dp[i][j] = s1[i-1] == s2[j-1] ? dp[i-1][j-1] + 1 : [dp[i-1][j], dp[i][j-1]].max 
  }}
  
  # Backtrack
  result = []
  i, j = m, n
  while i > 0 && j > 0
    if s1[i-1] == s2[j-1]
      result << s1[i-1]
      i -= 1
      j -= 1
    elsif dp[i-1][j] > dp[i][j-1]
      i -= 1
    else
      j -= 1
    end
  end
  
  result.reverse.join
end

# Read input and solve
str1 = gets.chomp
str2 = gets.chomp
puts lcs(str1, str2)
```

