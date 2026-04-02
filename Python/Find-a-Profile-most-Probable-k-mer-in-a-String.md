# Find a Profile-most Probable k-mer in a String

## Problem Understanding

Given a string, a k-mer, and a profile matrix, we need to find the k-mer in the string that has the highest probability according to the profile.

## Solution Approach

1. Generate all possible k-mers from the input string
2. Calculate the probability of each k-mer using the profile matrix
3. Return the k-mer with the highest probability

## Python Implementation

```python
def profile_most_probable_kmer(text, k, profile):
    """
    Find the profile-most probable k-mer in a string.
    
    Args:
        text (str): The input string
        k (int): Length of k-mer
        profile (dict): Profile matrix with nucleotides as keys and lists of probabilities as values
    
    Returns:
        str: The profile-most probable k-mer
    """
    max_probability = -1
    most_probable_kmer = ""
    
    # Generate all possible k-mers from the text
    for i in range(len(text) - k + 1):
        kmer = text[i:i+k]
        
        # Calculate probability of this k-mer
        probability = 1.0
        for j, nucleotide in enumerate(kmer):
            probability *= profile[nucleotide][j]
        
        # Update maximum probability and k-mer if this is better
        if probability > max_probability:
            max_probability = probability
            most_probable_kmer = kmer
    
    return most_probable_kmer

# Alternative implementation with more explicit probability calculation
def profile_most_probable_kmer_v2(text, k, profile):
    """
    Alternative implementation with clearer probability calculation.
    """
    max_probability = 0
    most_probable_kmer = ""
    
    # Generate all possible k-mers
    for i in range(len(text) - k + 1):
        kmer = text[i:i+k]
        
        # Calculate probability by multiplying individual nucleotide probabilities
        probability = 1
        for j, nucleotide in enumerate(kmer):
            probability *= profile[nucleotide][j]
        
        # Keep track of the k-mer with maximum probability
        if probability > max_probability:
            max_probability = probability
            most_probable_kmer = kmer
    
    return most_probable_kmer

# Example usage and test
def main():
    # Example from Rosalind problem
    text = "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT"
    k = 5
    profile = {
        'A': [0.2, 0.2, 0.3, 0.2, 0.3],
        'C': [0.4, 0.3, 0.1, 0.5, 0.1],
        'G': [0.3, 0.3, 0.5, 0.2, 0.4],
        'T': [0.1, 0.2, 0.1, 0.1, 0.2]
    }
    
    result = profile_most_probable_kmer(text, k, profile)
    print(f"Profile-most probable k-mer: {result}")
    
    # Test with another example
    text2 = "ACGTACGT"
    k2 = 3
    profile2 = {
        'A': [0.1, 0.2, 0.3],
        'C': [0.4, 0.3, 0.2],
        'G': [0.3, 0.3, 0.3],
        'T': [0.2, 0.2, 0.2]
    }
    
    result2 = profile_most_probable_kmer(text2, k2, profile2)
    print(f"Profile-most probable k-mer: {result2}")

if __name__ == "__main__":
    main()
```

## Explanation

The algorithm works as follows:

1. **Generate k-mers**: Iterate through the input string and extract all possible k-mers
2. **Calculate probabilities**: For each k-mer, multiply the probabilities of each nucleotide position according to the profile matrix
3. **Find maximum**: Keep track of the k-mer with the highest probability

## Time and Space Complexity

- **Time Complexity**: O(n × k) where n is the length of the text and k is the k-mer length
- **Space Complexity**: O(1) for the algorithm itself (excluding input storage)

## Key Points

- The profile matrix maps each nucleotide (A, C, G, T) to a list of probabilities for each position
- We multiply probabilities across positions for each k-mer
- The first k-mer with maximum probability is returned (in case of ties)

This solution handles the core requirements of the Rosalind problem efficiently and correctly.

