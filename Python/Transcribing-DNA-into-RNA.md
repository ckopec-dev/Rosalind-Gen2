# Rosalind Problem: Transcribing DNA into RNA

## Problem Statement
In RNA strings, symbols 'U' and 'T' are used to represent nucleotides. Given a DNA string, we need to transcribe it into RNA by replacing each 'T' with 'U'.

## Solution

```python
def transcribe_dna_to_rna(dna_string):
    """
    Transcribe DNA string to RNA string by replacing 'T' with 'U'
    
    Args:
        dna_string (str): DNA string containing nucleotides A, C, G, T
    
    Returns:
        str: RNA string with T replaced by U
    """
    return dna_string.replace('T', 'U')

# Read input from file or stdin
def main():
    # Read DNA string from input
    dna_input = input().strip()
    
    # Transcribe to RNA
    rna_output = transcribe_dna_to_rna(dna_input)
    
    # Print result
    print(rna_output)

# Alternative approach using string translation
def transcribe_dna_to_rna_v2(dna_string):
    """
    Alternative implementation using string translation
    """
    translation_table = str.maketrans('T', 'U')
    return dna_string.translate(translation_table)

# Example usage
if __name__ == "__main__":
    # Example from Rosalind problem
    example_dna = "GATGGAACTTGACTACGTAAATT"
    result = transcribe_dna_to_rna(example_dna)
    print(f"DNA: {example_dna}")
    print(f"RNA: {result}")
    
    # For actual Rosalind submission
    # main()
```

## Explanation

The solution is straightforward:
1. **Problem Understanding**: DNA contains nucleotides A, C, G, T while RNA contains A, C, G, U
2. **Transformation**: Replace every 'T' in the DNA string with 'U' to get the RNA string
3. **Implementation**: Use Python's built-in `replace()` method or `translate()` method

## Test Case

**Input**: `GATGGAACTTGACTACGTAAATT`
**Output**: `GAUGGAACUUGACUACGUAAAUU`

## Key Points

- The `replace()` method is simple and readable
- The `translate()` method is more efficient for large strings
- Both methods preserve the order of nucleotides
- The solution handles any valid DNA string with characters A, C, G, T

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the DNA string
- **Space Complexity**: O(n) for the resulting RNA string

This solution efficiently handles the transcription process required for the Rosalind problem.

