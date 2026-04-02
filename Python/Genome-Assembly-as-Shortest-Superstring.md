# Rosalind Problem: Genome Assembly as Shortest Superstring

## Problem Description
Given a collection of DNA strings, find the shortest superstring that contains all the strings as substrings.

## Approach
This is a variant of the Shortest Superstring Problem, which can be solved using a greedy approach:
1. Find the maximum overlap between each pair of strings
2. Repeatedly merge the pair with maximum overlap
3. Continue until only one string remains

## Solution

```python
def get_overlap(s1, s2):
    """Find the maximum overlap between the end of s1 and the beginning of s2"""
    max_overlap = 0
    # Try all possible overlaps from longest to shortest
    for i in range(1, min(len(s1), len(s2)) + 1):
        if s1[-i:] == s2[:i]:
            max_overlap = i
    return max_overlap

def find_shortest_superstring(strings):
    """Find the shortest superstring containing all strings as substrings"""
    if not strings:
        return ""
    
    if len(strings) == 1:
        return strings[0]
    
    # Create a matrix of overlaps
    n = len(strings)
    overlap = [[0] * n for _ in range(n)]
    
    # Calculate overlaps between all pairs
    for i in range(n):
        for j in range(n):
            if i != j:
                overlap[i][j] = get_overlap(strings[i], strings[j])
    
    # Greedy approach: repeatedly merge the pair with maximum overlap
    while len(strings) > 1:
        # Find the maximum overlap
        max_overlap = -1
        best_i, best_j = 0, 1
        
        for i in range(len(strings)):
            for j in range(len(strings)):
                if i != j and overlap[i][j] > max_overlap:
                    max_overlap = overlap[i][j]
                    best_i, best_j = i, j
        
        # If no positive overlap found, we're done
        if max_overlap <= 0:
            break
            
        # Merge strings[best_i] and strings[best_j]
        merged = strings[best_i] + strings[best_j][max_overlap:]
        
        # Remove the two merged strings and add the new one
        new_strings = []
        for i in range(len(strings)):
            if i != best_i and i != best_j:
                new_strings.append(strings[i])
        new_strings.append(merged)
        
        # Update overlap matrix
        new_overlap = [[0] * len(new_strings) for _ in range(len(new_strings))]
        for i in range(len(new_strings)):
            for j in range(len(new_strings)):
                if i != j:
                    if i < len(strings) and j < len(strings):
                        # Old overlaps
                        new_overlap[i][j] = get_overlap(new_strings[i], new_strings[j])
                    else:
                        # New overlaps
                        new_overlap[i][j] = get_overlap(new_strings[i], new_strings[j])
        
        strings = new_strings
        overlap = new_overlap
    
    return strings[0]

def solve_rosalind_problem(strings):
    """Solve the Rosalind problem"""
    return find_shortest_superstring(strings)

# Example usage
if __name__ == "__main__":
    # Example from Rosalind
    example_strings = [
        "ATTAGACCTG",
        "CCTGCCGGAA",
        "AGACCTGCCG",
        "GCCGGAATAC"
    ]
    
    result = solve_rosalind_problem(example_strings)
    print(f"Shortest superstring: {result}")
```

## Alternative Implementation (More Efficient)

```python
def shortest_superstring(strings):
    """More efficient implementation using greedy approach"""
    if not strings:
        return ""
    
    # Precompute overlaps
    n = len(strings)
    overlaps = [[0] * n for _ in range(n)]
    
    for i in range(n):
        for j in range(n):
            if i != j:
                # Find overlap of strings[i] with strings[j]
                for k in range(1, len(strings[i]) + 1):
                    if strings[i][-k:] == strings[j][:k]:
                        overlaps[i][j] = k
    
    # Greedy merging
    while len(strings) > 1:
        max_overlap = -1
        merge_i, merge_j = 0, 1
        
        # Find pair with maximum overlap
        for i in range(len(strings)):
            for j in range(len(strings)):
                if i != j and overlaps[i][j] > max_overlap:
                    max_overlap = overlaps[i][j]
                    merge_i, merge_j = i, j
        
        if max_overlap <= 0:
            break
            
        # Merge strings[merge_i] and strings[merge_j]
        merged = strings[merge_i] + strings[merge_j][max_overlap:]
        
        # Remove the two strings and add merged one
        new_strings = []
        for i in range(len(strings)):
            if i != merge_i and i != merge_j:
                new_strings.append(strings[i])
        new_strings.append(merged)
        
        # Update overlaps for new set of strings
        strings = new_strings
        n = len(strings)
        overlaps = [[0] * n for _ in range(n)]
        
        for i in range(n):
            for j in range(n):
                if i != j:
                    for k in range(1, len(strings[i]) + 1):
                        if strings[i][-k:] == strings[j][:k]:
                            overlaps[i][j] = k
    
    return strings[0] if strings else ""

# For processing Rosalind input format
def read_fasta(data):
    """Read FASTA formatted data"""
    sequences = []
    current_seq = ""
    
    for line in data.split('\n'):
        if line.startswith('>'):
            if current_seq:
                sequences.append(current_seq)
            current_seq = ""
        else:
            current_seq += line
    
    if current_seq:
        sequences.append(current_seq)
    
    return sequences

# Complete solution for Rosalind
def main():
    # Read input (this would be from a file in actual Rosalind)
    # For demonstration, using example data
    input_data = """>Rosalind_56
ATTAGACCTG
>Rosalind_57
CCTGCCGGAA
>Rosalind_58
AGACCTGCCG
>Rosalind_59
GCCGGAATAC"""
    
    sequences = read_fasta(input_data)
    result = shortest_superstring(sequences)
    print(result)

if __name__ == "__main__":
    main()
```

## Key Points

1. **Overlap Calculation**: For each pair of strings, we calculate the maximum overlap where the end of one string matches the beginning of another.

2. **Greedy Merging**: We repeatedly merge the pair of strings with the maximum overlap until only one string remains.

3. **Time Complexity**: O(n³) where n is the number of strings, due to overlap calculations and repeated merging.

4. **Space Complexity**: O(n²) for storing the overlap matrix.

This solution handles the core problem of finding the shortest superstring that contains all given DNA sequences as substrings.

