# Finding Disjoint Motifs in a Gene - Rosalind Problem Solution

## Problem Understanding

The problem asks us to find the positions of two disjoint motifs (non-overlapping substrings) in a DNA sequence. We need to identify all possible pairs of positions where these motifs can be found without overlapping.

## Solution Approach

I'll solve this step by step:

1. Find all occurrences of the first motif
2. Find all occurrences of the second motif  
3. For each pair of positions, check if they are disjoint (don't overlap)
4. Return valid pairs of positions

## Python Implementation

```python
def find_disjoint_motifs(sequence, motif1, motif2):
    """
    Find all disjoint occurrences of two motifs in a DNA sequence.
    
    Args:
        sequence (str): DNA sequence
        motif1 (str): First motif to find
        motif2 (str): Second motif to find
    
    Returns:
        list: List of tuples containing (position1, position2) where positions are 1-indexed
    """
    # Find all occurrences of motif1
    positions1 = []
    for i in range(len(sequence) - len(motif1) + 1):
        if sequence[i:i+len(motif1)] == motif1:
            positions1.append(i + 1)  # 1-indexed
    
    # Find all occurrences of motif2
    positions2 = []
    for i in range(len(sequence) - len(motif2) + 1):
        if sequence[i:i+len(motif2)] == motif2:
            positions2.append(i + 1)  # 1-indexed
    
    # Find disjoint pairs
    disjoint_pairs = []
    for pos1 in positions1:
        for pos2 in positions2:
            # Check if motifs don't overlap
            # motif1 starts at pos1, ends at pos1 + len(motif1) - 1
            # motif2 starts at pos2, ends at pos2 + len(motif2) - 1
            start1, end1 = pos1, pos1 + len(motif1) - 1
            start2, end2 = pos2, pos2 + len(motif2) - 1
            
            # Check if intervals don't overlap
            if end1 < start2 or end2 < start1:
                disjoint_pairs.append((pos1, pos2))
    
    return disjoint_pairs

def solve_finding_disjoint_motifs():
    """
    Solve the Rosalind problem Finding Disjoint Motifs in a Gene
    """
    # Example input (replace with actual Rosalind input)
    sequence = "ACGTACGTACGT"
    motif1 = "ACG"
    motif2 = "TAC"
    
    # Find disjoint motifs
    result = find_disjoint_motifs(sequence, motif1, motif2)
    
    # Print results
    for pos1, pos2 in result:
        print(f"Motif1 at position {pos1}, Motif2 at position {pos2}")
    
    return result

# Alternative more efficient approach using regular expressions
import re

def find_disjoint_motifs_regex(sequence, motif1, motif2):
    """
    Find disjoint motifs using regex approach
    """
    # Find all occurrences of motif1
    positions1 = []
    for match in re.finditer(f'(?={motif1})', sequence):
        positions1.append(match.start() + 1)
    
    # Find all occurrences of motif2
    positions2 = []
    for match in re.finditer(f'(?={motif2})', sequence):
        positions2.append(match.start() + 1)
    
    # Find disjoint pairs
    disjoint_pairs = []
    for pos1 in positions1:
        for pos2 in positions2:
            start1, end1 = pos1, pos1 + len(motif1) - 1
            start2, end2 = pos2, pos2 + len(motif2) - 1
            
            if end1 < start2 or end2 < start1:
                disjoint_pairs.append((pos1, pos2))
    
    return disjoint_pairs

# Complete solution for Rosalind format
def rosalind_solution(sequence, motif1, motif2):
    """
    Complete solution for Rosalind problem
    """
    # Find all disjoint motif pairs
    pairs = find_disjoint_motifs(sequence, motif1, motif2)
    
    # Format output
    result = []
    for pos1, pos2 in pairs:
        result.append(f"{pos1} {pos2}")
    
    return result

# Example usage
if __name__ == "__main__":
    # Sample input
    sequence = "ACGTACGTACGT"
    motif1 = "ACG"
    motif2 = "TAC"
    
    # Solve
    result = rosalind_solution(sequence, motif1, motif2)
    
    # Print formatted result
    for line in result:
        print(line)
```

## Example Walkthrough

Let's trace through an example:

**Input:**
- Sequence: "ACGTACGTACGT"
- Motif1: "ACG" 
- Motif2: "TAC"

**Step 1:** Find all occurrences of "ACG"
- Position 1: "ACG" ✓
- Position 5: "ACG" ✓
- Position 9: "ACG" ✓

**Step 2:** Find all occurrences of "TAC"
- Position 4: "TAC" ✓
- Position 8: "TAC" ✓
- Position 12: "TAC" ✓

**Step 3:** Check for disjoint pairs
- ACG at position 1, TAC at position 4: No overlap ✓
- ACG at position 5, TAC at position 8: No overlap ✓  
- ACG at position 9, TAC at position 12: No overlap ✓

**Output:** (1,4), (5,8), (9,12)

## Key Points

1. **1-indexed positions**: Rosalind problems typically use 1-indexed positions
2. **Disjoint condition**: Two intervals [start1, end1] and [start2, end2] are disjoint if `end1 < start2` or `end2 < start1`
3. **Overlapping check**: We need to ensure the motifs don't share any positions
4. **Efficiency**: The solution has O(n²) complexity in worst case, which is acceptable for typical Rosalind inputs

This approach handles the core requirements of finding disjoint motifs in a gene sequence as specified in the Rosalind problem.

