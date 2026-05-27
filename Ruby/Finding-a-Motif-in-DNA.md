# Rosalind Problem: Finding a Motif in DNA (Ruby Solution)

## Problem Description
Given two DNA strings s and t, find all locations of t as a substring of s.

## Solution

```ruby
def find_motif_locations(s, t)
  locations = []
  start = 0
  
  while start <= s.length - t.length
    pos = s.index(t, start)
    break if pos.nil?
    locations << pos + 1  # Convert to 1-based indexing
    start = pos + 1
  end
  
  locations
end

# Read input from file or stdin
input = gets.chomp
s = input
t = gets.chomp

# Find and print locations
locations = find_motif_locations(s, t)
puts locations.join(' ')
```

## Alternative Implementation Using Regular Expressions

```ruby
def find_motif_locations_regex(s, t)
  locations = []
  # Escape special regex characters in the motif
  escaped_t = Regexp.escape(t)
  
  # Find all matches with their positions
  s.enum_for(:scan, /(?=#{escaped_t})/).map do |match|
    locations << $~.begin(0) + 1
  end
  
  locations
end

# Read input
s = gets.chomp
t = gets.chomp

# Find and print locations
locations = find_motif_locations_regex(s, t)
puts locations.join(' ')
```

## Simple One-liner Approach

```ruby
s = gets.chomp
t = gets.chomp

# Find all positions where motif occurs
positions = (0..s.length - t.length).select { |i| s[i, t.length] == t }
puts positions.map { |i| i + 1 }.join(' ')
```

## Example Usage

**Input:**
```
GATATATGCATATACTT
ATAT
```

**Output:**
```
2 4 10
```

## Explanation

The problem asks us to find all starting positions (1-indexed) where a motif (substring) appears in a DNA sequence. 

Key points:
1. **1-based indexing**: Positions are counted from 1, not 0
2. **Overlapping matches**: We need to find all occurrences, including overlapping ones
3. **Substring matching**: We're looking for exact matches of the motif

The first solution uses `String#index` method with a starting position to find all occurrences, while the third solution uses a simple loop to check each possible position in the DNA string.

All solutions handle the case where the motif might overlap with itself and return positions in the correct 1-based format required by the problem.

