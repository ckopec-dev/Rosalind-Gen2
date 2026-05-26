# Rosalind Problem: Pattern Matching with the Suffix Array

## Problem Understanding

Pattern matching with suffix arrays involves finding all occurrences of a pattern in a text using a precomputed suffix array. The key insight is that suffixes are sorted lexicographically, so we can use binary search to efficiently locate pattern occurrences.

## Solution Approach

1. **Build the suffix array** of the text
2. **Use binary search** to find the range of suffixes that start with the pattern
3. **Extract positions** from the suffix array that match the pattern

## Implementation

```python
def build_suffix_array(text):
    """
    Build suffix array for given text
    Returns list of indices representing sorted suffixes
    """
    suffixes = [(text[i:], i) for i in range(len(text))]
    suffixes.sort()
    return [suffix[1] for suffix in suffixes]

def pattern_matching_with_suffix_array(text, pattern):
    """
    Find all occurrences of pattern in text using suffix array
    Returns list of starting positions (0-indexed)
    """
    # Build suffix array
    suffix_array = build_suffix_array(text)
    
    # Binary search to find leftmost occurrence
    def find_leftmost():
        left, right = 0, len(suffix_array) - 1
        result = -1
        
        while left <= right:
            mid = (left + right) // 2
            suffix_start = suffix_array[mid]
            
            # Compare pattern with suffix
            suffix = text[suffix_start:]
            if suffix.startswith(pattern):
                result = mid
                right = mid - 1  # Look for leftmost occurrence
            elif suffix < pattern:
                left = mid + 1
            else:
                right = mid - 1
                
        return result
    
    # Binary search to find rightmost occurrence
    def find_rightmost():
        left, right = 0, len(suffix_array) - 1
        result = -1
        
        while left <= right:
            mid = (left + right) // 2
            suffix_start = suffix_array[mid]
            
            # Compare pattern with suffix
            suffix = text[suffix_start:]
            if suffix.startswith(pattern):
                result = mid
                left = mid + 1  # Look for rightmost occurrence
            elif suffix < pattern:
                left = mid + 1
            else:
                right = mid - 1
                
        return result
    
    # Find range of occurrences
    left_pos = find_leftmost()
    right_pos = find_rightmost()
    
    if left_pos == -1:
        return []
    
    # Collect all positions
    positions = []
    for i in range(left_pos, right_pos + 1):
        positions.append(suffix_array[i])
    
    return sorted(positions)

def solve_pattern_matching(text, pattern):
    """
    Main function to solve the pattern matching problem
    """
    positions = pattern_matching_with_suffix_array(text, pattern)
    return positions

# Alternative optimized approach using binary search on suffix array
def pattern_matching_optimized(text, pattern):
    """
    Optimized version using binary search functions
    """
    # Build suffix array
    suffix_array = build_suffix_array(text)
    
    # Find left boundary
    def lower_bound(pattern):
        left, right = 0, len(suffix_array)
        while left < right:
            mid = (left + right) // 2
            suffix_start = suffix_array[mid]
            suffix = text[suffix_start:]
            if suffix < pattern:
                left = mid + 1
            else:
                right = mid
        return left
    
    # Find right boundary
    def upper_bound(pattern):
        left, right = 0, len(suffix_array)
        while left < right:
            mid = (left + right) // 2
            suffix_start = suffix_array[mid]
            suffix = text[suffix_start:]
            if suffix <= pattern:
                left = mid + 1
            else:
                right = mid
        return left
    
    # Get the range of matching suffixes
    left_idx = lower_bound(pattern)
    right_idx = upper_bound(pattern)
    
    # Extract positions
    positions = []
    for i in range(left_idx, right_idx):
        positions.append(suffix_array[i])
    
    return sorted(positions)

# Example usage and test
if __name__ == "__main__":
    # Test case from Rosalind
    text = "AAATCGGGTTCAATCGGGGT"
    pattern = "ATCGGG"
    
    result = solve_pattern_matching(text, pattern)
    print(f"Text: {text}")
    print(f"Pattern: {pattern}")
    print(f"Positions: {result}")
    
    # Another test case
    text2 = "AATCGGGTTCAATCGGGGT"
    pattern2 = "ATCGGG"
    
    result2 = solve_pattern_matching(text2, pattern2)
    print(f"\nText: {text2}")
    print(f"Pattern: {pattern2}")
    print(f"Positions: {result2}")
    
    # Using optimized version
    result3 = pattern_matching_optimized(text, pattern)
    print(f"\nOptimized result: {result3}")
```

## Explanation

### Key Components:

1. **Suffix Array Construction**: Creates an array of indices representing suffixes sorted lexicographically
2. **Binary Search**: Efficiently finds the range of suffixes that start with the pattern
3. **Pattern Matching**: Extracts positions from the suffix array where pattern occurs

### Time Complexity:
- Building suffix array: O(n² log n) where n is text length
- Pattern matching: O(m log n) where m is pattern length
- Overall: O(n² log n + m log n)

### Space Complexity:
- O(n²) for storing suffixes and O(n) for suffix array

### Usage:
```python
# For Rosalind problem input
text = "your_text_here"
pattern = "your_pattern_here"
positions = pattern_matching_optimized(text, pattern)
print(" ".join(map(str, positions)))
```

This solution efficiently handles pattern matching using suffix arrays and works well for the constraints typically found in Rosalind problems.

