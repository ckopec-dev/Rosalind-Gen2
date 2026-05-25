# Rosalind Problem: Construct the Partial Suffix Array of a String

## Problem Description
Given a string `s` of length `n` and an integer `k`, construct the partial suffix array of `s` with respect to `k`. The partial suffix array contains entries `(i, s[i:])` where `s[i:]` is a suffix of `s` and `i` is the index of the suffix in the suffix array.

## Solution

```ruby
def partial_suffix_array(s, k)
  # Create array of suffixes with their original indices
  suffixes = []
  (0...s.length).each do |i|
    suffixes << [s[i..-1], i]
  end
  
  # Sort suffixes lexicographically
  suffixes.sort!
  
  # Create partial suffix array with entries where index % k == 0
  partial = []
  suffixes.each_with_index do |(suffix, original_index), i|
    if original_index % k == 0
      partial << [i, original_index]
    end
  end
  
  partial
end

# Alternative more concise implementation
def partial_suffix_array_concise(s, k)
  # Generate all suffixes with original indices
  suffixes = (0...s.length).map { |i| [s[i..-1], i] }
  
  # Sort suffixes lexicographically
  sorted_suffixes = suffixes.sort
  
  # Create partial suffix array for indices where original_index % k == 0
  sorted_suffixes.each_with_index
    .select { |(suffix, original_index), i| original_index % k == 0 }
    .map { |(suffix, original_index), i| [i, original_index] }
end

# Example usage:
# s = "panamabananas$"
# k = 3
# result = partial_suffix_array(s, k)
# puts result.inspect

# Test with sample data
s = "panamabananas$"
k = 3
puts "Input string: #{s}"
puts "k value: #{k}"
puts "Partial suffix array:"
result = partial_suffix_array(s, k)
result.each do |entry|
  puts "  [#{entry[0]}, #{entry[1]}]"
end
```

## Explanation

The solution works in the following steps:

1. **Generate suffixes**: Create an array containing all suffixes of the string along with their original indices
2. **Sort suffixes**: Sort the suffixes lexicographically to create the suffix array
3. **Filter by k**: Select only those entries where the original index is divisible by k
4. **Return result**: The result is an array of pairs `[rank, original_index]` where rank is the position in the sorted suffix array

## Time Complexity
- O(n² log n) for sorting suffixes (where n is the length of the string)
- O(n) for filtering by k
- Overall: O(n² log n)

## Space Complexity
- O(n²) for storing all suffixes

## Example
For string "panamabananas$" with k=3:
- Suffixes: ["panamabananas$", "anamabananas$", "namabananas$", ...]
- After sorting: The suffixes are ordered lexicographically
- Partial array: Only entries where original index % 3 == 0 are included

The partial suffix array is useful in bioinformatics applications, particularly for constructing suffix arrays and finding repeated sequences in DNA strings.

