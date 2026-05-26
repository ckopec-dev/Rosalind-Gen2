# Rosalind Problem: k-Mer Composition

## Problem Description
Given a string Text and an integer k, return the k-mer composition of Text as a sorted list of k-mers.

## Solution

```python
def kmer_composition(text, k):
    """
    Generate the k-mer composition of a string.
    
    Args:
        text (str): The input string
        k (int): The length of k-mers
    
    Returns:
        list: Sorted list of k-mers
    """
    # Extract all k-mers from the text
    kmers = []
    for i in range(len(text) - k + 1):
        kmers.append(text[i:i+k])
    
    # Sort and return the k-mers
    return sorted(kmers)

# Read input from file or stdin
def main():
    # Example input
    text = "CAATCCAAC"
    k = 5
    
    # Get k-mer composition
    result = kmer_composition(text, k)
    
    # Print result
    for kmer in result:
        print(kmer)

# Alternative implementation using list comprehension
def kmer_composition_compact(text, k):
    """
    Compact version using list comprehension.
    """
    return sorted([text[i:i+k] for i in range(len(text) - k + 1)])

# If reading from a file
def read_from_file(filename):
    """
    Read input from file.
    """
    with open(filename, 'r') as file:
        lines = file.readlines()
        text = lines[0].strip()
        k = int(lines[1].strip())
        return text, k

# Example usage with sample data
if __name__ == "__main__":
    # Sample input from Rosalind
    text = "CAATCCAAC"
    k = 5
    
    result = kmer_composition(text, k)
    
    print("K-mer composition:")
    for kmer in result:
        print(kmer)
    
    # Expected output:
    # AATCC
    # ATCCA
    # CAATC
    # CCAAC
    # TCCAA
```

## Explanation

1. **Problem Understanding**: We need to find all possible k-mers (substrings of length k) from a given string and return them in sorted order.

2. **Algorithm Steps**:
   - Iterate through the string from index 0 to `len(text) - k`
   - Extract substring of length k at each position
   - Sort the resulting k-mers alphabetically
   - Return the sorted list

3. **Time Complexity**: O(n×k×log(n)) where n is the length of the text
   - O(n×k) to extract all k-mers
   - O(n×log(n)) for sorting

4. **Space Complexity**: O(n×k) to store all k-mers

## Sample Input/Output

**Input:**
```
CAATCCAAC
5
```

**Output:**
```
AATCC
ATCCA
CAATC
CCAAC
TCCAA
```

## Test Case Verification

```python
# Test with the given example
text = "CAATCCAAC"
k = 5
expected = ["AATCC", "ATCCA", "CAATC", "CCAAC", "TCCAA"]
result = kmer_composition(text, k)
assert result == expected, f"Expected {expected}, got {result}"
print("Test passed!")
```

This solution efficiently handles the k-mer composition problem by extracting all possible substrings of length k and sorting them alphabetically.

