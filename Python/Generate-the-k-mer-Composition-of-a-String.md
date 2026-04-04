# Rosalind Problem: Generate the k-mer Composition of a String

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
    # Handle edge cases
    if len(text) < k:
        return []
    
    # Generate all k-mers
    kmers = []
    for i in range(len(text) - k + 1):
        kmer = text[i:i+k]
        kmers.append(kmer)
    
    # Sort and return
    return sorted(kmers)

# Read input from file or stdin
def main():
    # Example usage with sample data
    # In practice, you would read from a file
    text = "CAATCCAAC"
    k = 5
    
    result = kmer_composition(text, k)
    
    # Print result
    for kmer in result:
        print(kmer)

# Alternative implementation using list comprehension
def kmer_composition_compact(text, k):
    """
    Compact version using list comprehension.
    """
    if len(text) < k:
        return []
    
    return sorted([text[i:i+k] for i in range(len(text) - k + 1)])

# If reading from a file (as in Rosalind problems)
def read_rosalind_input(filename):
    """
    Read input from Rosalind-style file.
    """
    with open(filename, 'r') as file:
        lines = file.readlines()
        text = lines[0].strip()
        k = int(lines[1].strip())
        return text, k

# Example with sample input
if __name__ == "__main__":
    # Sample input
    sample_text = "CAATCCAAC"
    sample_k = 5
    
    # Solve
    result = kmer_composition(sample_text, sample_k)
    
    print("Input string:", sample_text)
    print("k value:", sample_k)
    print("K-mer composition:")
    for kmer in result:
        print(kmer)
```

## Example Walkthrough

For the input:
- Text: "CAATCCAAC"
- k: 5

The k-mers are:
1. "CAATC" (positions 0-4)
2. "AATCC" (positions 1-5)
3. "ATCCA" (positions 2-6)
4. "TCCAA" (positions 3-7)
5. "CCAAC" (positions 4-8)

Sorted result: ["AATCC", "CAATC", "CCAAC", "TCCAA", "ATCCA"]

## Key Points

1. **Edge case handling**: If the string length is less than k, return empty list
2. **Sliding window**: Use a loop to extract k-mers from consecutive positions
3. **Sorting**: Return k-mers in lexicographically sorted order
4. **Indexing**: Use `range(len(text) - k + 1)` to ensure we don't go out of bounds

## Time and Space Complexity

- **Time Complexity**: O(n×k×log(n)) where n is the length of text
  - O(n×k) for generating k-mers
  - O(n×log(n)) for sorting
- **Space Complexity**: O(n×k) for storing the k-mers

This solution efficiently generates and sorts the k-mer composition as required by the Rosalind problem.

