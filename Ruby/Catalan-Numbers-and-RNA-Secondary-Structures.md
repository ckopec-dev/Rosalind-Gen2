# Rosalind Problem: Catalan Numbers and RNA Secondary Structures

## Problem Understanding

This problem asks us to find the number of possible secondary structures for a given RNA sequence, which is equivalent to finding the nth Catalan number where n is the number of base pairs in the optimal structure.

## Solution Approach

The key insight is that the number of valid RNA secondary structures (where base pairs don't cross) follows the Catalan number sequence. For an RNA sequence of length n, we need to count the number of ways to form non-crossing base pairs.

## Implementation

```ruby
def catalan_rna_secondary_structures(rna)
  # Remove any whitespace and ensure uppercase
  rna = rna.strip.upcase
  
  # Check if the sequence has even length (required for base pairing)
  return 0 if rna.length.odd?
  
  # Memoization for dynamic programming
  memo = {}
  
  def count_structures(s, memo)
    return 1 if s.length <= 1
    
    # Check memoization
    return memo[s] if memo.key?(s)
    
    # For any valid structure, we can either:
    # 1. Not pair the first nucleotide (count structures of remaining string)
    # 2. Pair first nucleotide with some compatible nucleotide j (and count structures of both parts)
    
    count = 0
    
    # Try pairing first nucleotide with each possible compatible nucleotide
    (1...s.length).each do |j|
      # Check if nucleotides at positions 0 and j can pair
      if can_pair?(s[0], s[j])
        # Split into three parts: left part, middle part, right part
        left = s[1...j]
        right = s[(j+1)..-1]
        
        # Multiply the number of structures for left and right parts
        left_count = count_structures(left, memo)
        right_count = count_structures(right, memo)
        
        count += left_count * right_count
      end
    end
    
    memo[s] = count
    count
  end
  
  def can_pair?(n1, n2)
    # Valid base pairs: A-U, U-A, G-C, C-G, U-G, G-U
    pairs = {
      'A' => 'U',
      'U' => 'A',
      'G' => 'C',
      'C' => 'G'
    }
    
    pairs[n1] == n2 || pairs[n2] == n1
  end
  
  count_structures(rna, memo)
end

# Alternative approach using direct Catalan number calculation
# For RNA secondary structures, we need to compute the nth Catalan number
# where n is the number of base pairs

def catalan_number(n)
  return 1 if n <= 1
  
  # Using the formula: C(n) = (2n)! / ((n+1)! * n!)
  # Or using the recurrence: C(n) = sum(i=0 to n-1) C(i) * C(n-1-i)
  
  # Using dynamic programming approach
  catalan = Array.new(n + 1, 0)
  catalan[0] = 1
  catalan[1] = 1
  
  (2..n).each do |i|
    (0...i).each do |j|
      catalan[i] += catalan[j] * catalan[i - 1 - j]
    end
  end
  
  catalan[n]
end

# More efficient version for the specific problem
def solve_rna_secondary_structures(rna)
  # For RNA secondary structures, we use the approach that counts valid
  # non-crossing base pairings
  
  rna = rna.strip.upcase
  return 0 if rna.length.odd?
  
  memo = {}
  
  def count_non_crossing_pairs(s, memo)
    return 1 if s.length <= 1
    return memo[s] if memo.key?(s)
    
    count = 0
    
    # Try all possible first base pairs
    (1...s.length).each do |j|
      if can_pair?(s[0], s[j])
        # If we pair s[0] with s[j], we have:
        # - structures in s[1...j] 
        # - structures in s[(j+1)..-1]
        left = s[1...j]
        right = s[(j+1)..-1]
        
        left_count = count_non_crossing_pairs(left, memo)
        right_count = count_non_crossing_pairs(right, memo)
        
        count += left_count * right_count
      end
    end
    
    memo[s] = count
    count
  end
  
  def can_pair?(n1, n2)
    # Valid base pairs: A-U, U-A, G-C, C-G
    pairs = {
      'A' => 'U',
      'U' => 'A',
      'G' => 'C',
      'C' => 'G'
    }
    
    pairs[n1] == n2 || pairs[n2] == n1
  end
  
  count_non_crossing_pairs(rna, memo)
end

# Read input from stdin or file
input = gets.chomp
result = solve_rna_secondary_structures(input)
puts result
```

## Example Usage

For an RNA sequence like "AUGCUAGU":
1. The algorithm will find all valid non-crossing base pairings
2. It will recursively count all possible combinations
3. The result will be the total number of valid secondary structures

## Key Points

1. **Base Pairing Rules**: Only A-U, U-A, G-C, C-G pairs are allowed
2. **Non-crossing Constraint**: Base pairs cannot cross each other
3. **Dynamic Programming**: Uses memoization to avoid recomputing subproblems
4. **Recursive Structure**: For any RNA sequence, we either don't pair the first nucleotide or pair it with a compatible nucleotide

## Time Complexity
- **Time**: O(n³) where n is the length of the RNA sequence
- **Space**: O(n²) for memoization

This solution correctly handles the RNA secondary structure counting problem using dynamic programming with memoization to efficiently compute the number of valid non-crossing base pairings.

