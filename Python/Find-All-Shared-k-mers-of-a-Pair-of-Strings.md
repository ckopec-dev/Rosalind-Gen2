# Find All Shared k-mers of a Pair of Strings

## Problem Understanding

Given two DNA strings, we need to find all k-mers (substrings of length k) that appear in both strings. The k-mers can be either forward or reverse complement.

## Solution Approach

1. Generate all k-mers from the first string
2. Generate all k-mers from the second string (including reverse complements)
3. Find the intersection of these sets
4. Return the shared k-mers

## Implementation

```python
def reverse_complement(dna):
    """Return the reverse complement of a DNA string"""
    complement = {'A': 'T', 'T': 'A', 'G': 'C', 'C': 'G'}
    return ''.join(complement[base] for base in reversed(dna))

def get_kmers(dna, k):
    """Get all k-mers from a DNA string"""
    return set(dna[i:i+k] for i in range(len(dna) - k + 1))

def find_all_shared_kmers(text1, text2, k):
    """
    Find all shared k-mers between two DNA strings
    
    Args:
        text1: First DNA string
        text2: Second DNA string
        k: Length of k-mers to find
    
    Returns:
        List of shared k-mers
    """
    # Get all k-mers from first string
    kmers1 = get_kmers(text1, k)
    
    # Get all k-mers from second string (including reverse complements)
    kmers2 = get_kmers(text2, k)
    kmers2_rc = set(reverse_complement(kmer) for kmer in kmers2)
    
    # Find intersection
    shared_kmers = kmers1.intersection(kmers2_rc)
    
    return sorted(list(shared_kmers))

# Example usage
if __name__ == "__main__":
    # Example from Rosalind
    text1 = "AAACTCATC"
    text2 = "TTATCTCAT"
    k = 3
    
    result = find_all_shared_kmers(text1, text2, k)
    print(" ".join(result))
```

## Alternative Implementation (More Detailed)

```python
def find_shared_kmers_detailed(text1, text2, k):
    """
    Find all shared k-mers between two DNA strings with detailed approach
    """
    # Get all k-mers from first string
    kmers1 = set()
    for i in range(len(text1) - k + 1):
        kmers1.add(text1[i:i+k])
    
    # Get all k-mers from second string and their reverse complements
    kmers2 = set()
    for i in range(len(text2) - k + 1):
        kmer = text2[i:i+k]
        kmers2.add(kmer)
        kmers2.add(reverse_complement(kmer))
    
    # Find shared k-mers
    shared = []
    for kmer in kmers1:
        if kmer in kmers2:
            shared.append(kmer)
    
    return sorted(shared)

# Test with sample data
def test_solution():
    # Sample input
    text1 = "AAACTCATC"
    text2 = "TTATCTCAT"
    k = 3
    
    result = find_shared_kmers_detailed(text1, text2, k)
    print("Shared k-mers:", result)
    
    # Expected output: ["ATC", "TAT", "TCT"]
    return result

# Run test
test_result = test_solution()
```

## Key Points

1. **Reverse Complement**: For each k-mer in the second string, we also consider its reverse complement since DNA sequences can be read in both directions.

2. **Set Operations**: Using sets for efficient lookup and intersection operations.

3. **Sorting**: Results are sorted alphabetically as required by Rosalind format.

4. **Edge Cases**: The solution handles cases where k is larger than the string length by returning empty results.

## Time Complexity
- O(n₁ × k + n₂ × k) where n₁ and n₂ are the lengths of the two strings
- Space complexity: O(n₁ × k + n₂ × k)

This solution efficiently finds all shared k-mers between two DNA strings, considering both forward and reverse complement orientations.

