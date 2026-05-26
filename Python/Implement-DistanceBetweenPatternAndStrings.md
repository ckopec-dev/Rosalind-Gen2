# Rosalind Problem: Implement DistanceBetweenPatternAndStrings

## Problem Description
The distance between a pattern and a set of strings is the sum of distances between the pattern and each string in the set. The distance between a pattern and a string is the minimum Hamming distance between the pattern and any k-mer in the string.

## Solution

```python
def hamming_distance(p, q):
    """Calculate the Hamming distance between two strings p and q."""
    return sum(a != b for a, b in zip(p, q))

def distance_between_pattern_and_strings(pattern, dna):
    """
    Calculate the distance between a pattern and a set of strings.
    
    Args:
        pattern (str): The pattern string
        dna (list): List of DNA strings
    
    Returns:
        int: The distance between pattern and strings
    """
    k = len(pattern)
    total_distance = 0
    
    for string in dna:
        min_hamming = float('inf')
        # Find the minimum Hamming distance between pattern and all k-mers in string
        for i in range(len(string) - k + 1):
            kmer = string[i:i+k]
            hamming = hamming_distance(pattern, kmer)
            min_hamming = min(min_hamming, hamming)
        total_distance += min_hamming
    
    return total_distance

# Example usage:
if __name__ == "__main__":
    # Example from Rosalind
    pattern = "AAA"
    dna = ["TTACCTTAAC", "GATATCTGAC", "AAGAAGTGAT", "TTTTTGGGAC"]
    
    result = distance_between_pattern_and_strings(pattern, dna)
    print(result)  # Should output 5
```

## Explanation

1. **hamming_distance function**: Calculates the Hamming distance between two strings of equal length by counting the number of positions where the characters differ.

2. **distance_between_pattern_and_strings function**: 
   - Takes a pattern and a list of DNA strings
   - For each DNA string, it finds all possible k-mers (where k is the length of the pattern)
   - Calculates the Hamming distance between the pattern and each k-mer
   - Keeps track of the minimum Hamming distance for each string
   - Sums up all the minimum distances to get the total distance

## Time Complexity
- O(n × m × k) where n is the number of strings, m is the average length of strings, and k is the length of the pattern

## Space Complexity
- O(1) additional space

## Test Case
For pattern "AAA" and DNA strings ["TTACCTTAAC", "GATATCTGAC", "AAGAAGTGAT", "TTTTTGGGAC"]:
- For "TTACCTTAAC": minimum Hamming distance is 1 (with "TTA")
- For "GATATCTGAC": minimum Hamming distance is 1 (with "GAT")  
- For "AAGAAGTGAT": minimum Hamming distance is 0 (with "AAA")
- For "TTTTTGGGAC": minimum Hamming distance is 3 (with "TTT")

Total distance = 1 + 1 + 0 + 3 = 5

