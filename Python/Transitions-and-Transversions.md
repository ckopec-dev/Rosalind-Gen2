# Rosalind Problem: Transitions and Transversions

## Problem Description
Given two DNA strings of equal length, we need to count the number of transitions and transversions between them.

- **Transition**: A change between purines (A↔G) or pyrimidines (C↔T)
- **Transversion**: A change between purine and pyrimidine (A↔C, A↔T, G↔C, G↔T)

## Solution

```python
def transitions_transversions(seq1, seq2):
    """
    Count transitions and transversions between two DNA sequences.
    
    Args:
        seq1 (str): First DNA sequence
        seq2 (str): Second DNA sequence
    
    Returns:
        tuple: (transitions, transversions)
    """
    # Define transition pairs
    transitions = {
        ('A', 'G'), ('G', 'A'),  # purine to purine
        ('C', 'T'), ('T', 'C')   # pyrimidine to pyrimidine
    }
    
    # Define transversion pairs
    transversions = {
        ('A', 'C'), ('C', 'A'),  # purine to pyrimidine
        ('A', 'T'), ('T', 'A'),  # purine to pyrimidine
        ('G', 'C'), ('C', 'G'),  # purine to pyrimidine
        ('G', 'T'), ('T', 'G')   # purine to pyrimidine
    }
    
    transitions_count = 0
    transversions_count = 0
    
    # Compare each position
    for base1, base2 in zip(seq1, seq2):
        if base1 != base2:
            if (base1, base2) in transitions:
                transitions_count += 1
            elif (base1, base2) in transversions:
                transversions_count += 1
    
    return transitions_count, transversions_count

def main():
    # Example usage with sample data
    # You would typically read from a file for Rosalind problems
    seq1 = "AAAACCCGGT"
    seq2 = "AAAACCCGGT"
    
    transitions, transversions = transitions_transversions(seq1, seq2)
    
    # Calculate the ratio
    ratio = transitions / transversions if transversions > 0 else 0
    
    print(f"Transitions: {transitions}")
    print(f"Transversions: {transversions}")
    print(f"Ratio (transition/transversion): {ratio:.10f}")

# For Rosalind input format
def rosalind_solve():
    """
    Solve the Rosalind problem with proper input handling
    """
    # Read input (this would typically be from a file)
    # For demonstration, using example from Rosalind
    
    # Sample input format for Rosalind
    # First line: sequence 1
    # Second line: sequence 2
    
    # You would typically read from stdin or a file:
    # lines = [line.strip() for line in sys.stdin]
    
    # For example:
    seq1 = "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGAAGTACGGGCATCAACCCAGTT"
    seq2 = "TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGCGGTACGAGTGTTCCTTTGGGT"
    
    transitions, transversions = transitions_transversions(seq1, seq2)
    
    # Output the ratio as required by Rosalind
    ratio = transitions / transversions if transversions > 0 else 0
    print(f"{transitions} {transversions}")
    print(f"{ratio:.10f}")

# Alternative more concise implementation
def transitions_transversions_v2(seq1, seq2):
    """
    More concise version using dictionary lookup
    """
    # Define transition and transversion rules
    is_transition = {
        ('A', 'G'): True, ('G', 'A'): True,
        ('C', 'T'): True, ('T', 'C'): True
    }
    
    is_transversion = {
        ('A', 'C'): True, ('C', 'A'): True,
        ('A', 'T'): True, ('T', 'A'): True,
        ('G', 'C'): True, ('C', 'G'): True,
        ('G', 'T'): True, ('T', 'G'): True
    }
    
    transitions = 0
    transversions = 0
    
    for base1, base2 in zip(seq1, seq2):
        if base1 != base2:
            if (base1, base2) in is_transition:
                transitions += 1
            elif (base1, base2) in is_transversion:
                transversions += 1
    
    return transitions, transversions

# Test with sample data
if __name__ == "__main__":
    # Test case from Rosalind
    test_seq1 = "AAAACCCGGT"
    test_seq2 = "AAAACCCGGT"
    
    # This should be a real Rosalind input
    # For actual Rosalind submission, read from file
    
    # Example with different sequences
    example_seq1 = "ACGTACGTACGT"
    example_seq2 = "ACGTACGTACGT"
    
    print("Testing with example sequences:")
    trans, transv = transitions_transversions(example_seq1, example_seq2)
    print(f"Transitions: {trans}")
    print(f"Transversions: {transv}")
```

## Key Points

1. **Transitions**: Changes between purines (A↔G) or pyrimidines (C↔T)
2. **Transversions**: Changes between purine and pyrimidine (A↔C, A↔T, G↔C, G↔T)
3. **Input**: Two DNA sequences of equal length
4. **Output**: Count of transitions and transversions, and their ratio

## Usage for Rosalind

For actual Rosalind submission, you would typically:

1. Read from a file or stdin
2. Process the input to extract the two DNA sequences
3. Call the function to get transitions and transversions
4. Output the results in the required format

The ratio of transitions to transversions is often the main output required for this problem.

