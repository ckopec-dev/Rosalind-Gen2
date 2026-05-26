# Rosalind Problem: Compute the Hamming Distance Between Two Strings

## Problem Description
The Hamming distance between two strings of equal length is the number of positions at which the corresponding symbols are different. Given two strings of equal length, we need to compute their Hamming distance.

## Solution

```python
def hamming_distance(str1, str2):
    """
    Compute the Hamming distance between two strings of equal length.
    
    Args:
        str1 (str): First string
        str2 (str): Second string
    
    Returns:
        int: Hamming distance between the two strings
    """
    # Check if strings have equal length
    if len(str1) != len(str2):
        raise ValueError("Strings must have equal length")
    
    # Count positions where characters differ
    distance = 0
    for i in range(len(str1)):
        if str1[i] != str2[i]:
            distance += 1
    
    return distance

# Alternative implementation using sum and zip
def hamming_distance_alt(str1, str2):
    """
    Alternative implementation using sum and zip.
    """
    if len(str1) != len(str2):
        raise ValueError("Strings must have equal length")
    
    return sum(1 for a, b in zip(str1, str2) if a != b)

# Read input from file (for Rosalind submission)
def solve_rosalind():
    """
    Solve the Rosalind problem by reading from stdin.
    """
    try:
        # Read the two strings from input
        str1 = input().strip()
        str2 = input().strip()
        
        # Compute and print the Hamming distance
        result = hamming_distance(str1, str2)
        print(result)
        
    except EOFError:
        print("Error: Could not read input")

# Example usage
if __name__ == "__main__":
    # Example from Rosalind
    example_str1 = "GAGCCTACTAACGGGAT"
    example_str2 = "CATCGGTATCGTTATAGCC"
    
    distance = hamming_distance(example_str1, example_str2)
    print(f"Hamming distance: {distance}")
    
    # Test with alternative implementation
    distance_alt = hamming_distance_alt(example_str1, example_str2)
    print(f"Hamming distance (alternative): {distance_alt}")
```

## Explanation

### Approach
1. **Input Validation**: Check that both strings have equal length
2. **Character Comparison**: Iterate through each position and compare characters
3. **Count Differences**: Increment counter when characters don't match

### Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the strings
- **Space Complexity**: O(1) - only using a constant amount of extra space

### Example Walkthrough
For strings "GAGCCTACTAACGGGAT" and "CATCGGTATCGTTATAGCC":

```
Position: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
String1:  G A G C C T A C T A A C G G G A T
String2:  C A T C G G T A T C G T T A T A G C C
Differences: X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
Count: 13 differences
```

## Rosalind Submission Format

```python
# For direct Rosalind submission
def hamming_distance(str1, str2):
    return sum(1 for a, b in zip(str1, str2) if a != b)

# Read input and solve
str1 = input().strip()
str2 = input().strip()
print(hamming_distance(str1, str2))
```

This solution efficiently computes the Hamming distance and handles the input/output format required for Rosalind problems.

