# Rosalind Problem: Finding All Similar Motifs

## Problem Description
Given two DNA strings s and t, find all locations where t occurs as a substring of s.

## Solution

```python
def find_all_motifs(s, t):
    """
    Find all locations where motif t occurs as a substring of string s.
    
    Args:
        s (str): The DNA string to search in
        t (str): The motif to search for
    
    Returns:
        list: List of 1-based positions where t occurs in s
    """
    positions = []
    start = 0
    
    while True:
        # Find the next occurrence of t in s starting from 'start'
        pos = s.find(t, start)
        if pos == -1:  # No more occurrences found
            break
        positions.append(pos + 1)  # Convert to 1-based indexing
        start = pos + 1  # Move start position to find overlapping matches
    
    return positions

def main():
    # Read input from file (assuming input.txt format)
    with open('input.txt', 'r') as file:
        s = file.readline().strip()  # First line: DNA string
        t = file.readline().strip()  # Second line: motif
    
    # Find all positions
    positions = find_all_motifs(s, t)
    
    # Print results
    print(' '.join(map(str, positions)))

# Alternative implementation using list comprehension
def find_all_motifs_compact(s, t):
    """
    Compact version using list comprehension.
    """
    return [i + 1 for i in range(len(s)) if s[i:].startswith(t)]

# Example usage
if __name__ == "__main__":
    # Example from Rosalind
    s = "GATATATGCATATACTT"
    t = "ATAT"
    
    result = find_all_motifs(s, t)
    print(f"Positions: {' '.join(map(str, result))}")
    # Expected output: 2 4 10
    
    # For actual Rosalind submission, use the file reading approach
    # main()
```

## Explanation

### Approach
1. **String Search**: Use Python's built-in `find()` method to locate occurrences of the motif
2. **1-based Indexing**: Convert from 0-based Python indexing to 1-based indexing as required
3. **Overlapping Motifs**: Continue searching from the next position to find overlapping occurrences

### Key Points
- The `find()` method returns the index of the first occurrence or -1 if not found
- We increment the search position by 1 each time to catch overlapping matches
- Convert from 0-based to 1-based indexing by adding 1 to the result

### Time Complexity
- **Time**: O(n × m) where n is the length of string s and m is the length of motif t
- **Space**: O(k) where k is the number of matches found

### Sample Input/Output
```
Input:
GATATATGCATATACTT
ATAT

Output:
2 4 10
```

This solution handles the specific requirements of the Rosalind problem including finding all occurrences (including overlapping ones) and returning positions in 1-based indexing format.

