# Rosalind Problem: Find the Reverse Complement of a String

## Problem Statement
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'. The reverse complement of a DNA string is the complement of the string, reversed. Given a DNA string, return its reverse complement.

## Solution

```python
def reverse_complement(dna_string):
    """
    Find the reverse complement of a DNA string.
    
    Args:
        dna_string (str): A DNA string consisting of nucleotides A, C, G, T
    
    Returns:
        str: The reverse complement of the input DNA string
    """
    # Create a mapping for nucleotide complements
    complement_map = {
        'A': 'T',
        'T': 'A',
        'C': 'G',
        'G': 'C'
    }
    
    # Generate complement and reverse the string
    complement = ''.join(complement_map[nucleotide] for nucleotide in dna_string)
    reverse_complement = complement[::-1]
    
    return reverse_complement

# Read input from file (if needed)
def read_input(filename):
    """Read DNA string from file"""
    with open(filename, 'r') as file:
        return file.read().strip()

# Example usage
if __name__ == "__main__":
    # Example DNA string
    dna = "AAAACCCGGT"
    
    # Find reverse complement
    result = reverse_complement(dna)
    print(f"Original DNA: {dna}")
    print(f"Reverse complement: {result}")
    
    # Test with sample data
    # Expected output for "AAAACCCGGT" should be "ACCGGGTTTT"
```

## Alternative Implementation (More Concise)

```python
def reverse_complement_v2(dna_string):
    """
    More concise version using string translation.
    """
    # Create translation table for complement
    complement_table = str.maketrans('ACGT', 'TGCA')
    
    # Get complement and reverse
    return dna_string.translate(complement_table)[::-1]

# Test the function
test_string = "AAAACCCGGT"
print(f"Input: {test_string}")
print(f"Output: {reverse_complement_v2(test_string)}")
```

## Step-by-Step Explanation

1. **Complement Mapping**: Create a dictionary mapping each nucleotide to its complement:
   - A → T
   - T → A  
   - C → G
   - G → C

2. **Generate Complement**: Iterate through the input string and replace each nucleotide with its complement.

3. **Reverse String**: Reverse the complemented string using slicing (`[::-1]`).

4. **Return Result**: Return the final reverse complement string.

## Example Walkthrough

For input `"AAAACCCGGT"`:
1. Complement: `"TTTTGGGCCCA"`
2. Reverse: `"ACCGGGTTTT"`

## Test Case
```
Input: AAAACCCGGT
Output: ACCGGGTTTT
```

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the DNA string
- **Space Complexity**: O(n) for storing the complement and result strings

The solution handles the standard DNA nucleotides (A, C, G, T) and correctly produces the reverse complement as required by the Rosalind problem.

