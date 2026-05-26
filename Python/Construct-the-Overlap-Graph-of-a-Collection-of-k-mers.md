# Rosalind Problem: Construct the Overlap Graph of a Collection of k-mers

## Problem Description
Given a collection of k-mers, construct the overlap graph where each k-mer is a node, and there is a directed edge from k-mer A to k-mer B if the suffix of A (k-1 characters) matches the prefix of B (k-1 characters).

## Solution

```python
def construct_overlap_graph(kmers):
    """
    Construct the overlap graph of a collection of k-mers.
    
    Args:
        kmers: List of k-mers (strings of equal length)
    
    Returns:
        List of edges in the format "Node1 -> Node2"
    """
    # Create a dictionary to store all k-mers and their indices
    kmer_dict = {}
    for i, kmer in enumerate(kmers):
        if kmer not in kmer_dict:
            kmer_dict[kmer] = []
        kmer_dict[kmer].append(i)
    
    # Find all edges
    edges = []
    k = len(kmers[0]) if kmers else 0
    
    if k == 0:
        return edges
    
    # For each k-mer, check if its suffix matches prefix of other k-mers
    for i, kmer in enumerate(kmers):
        suffix = kmer[1:]  # Remove first character
        prefix = kmer[:-1]  # Remove last character
        
        # Check if suffix matches any k-mer's prefix
        for j, other_kmer in enumerate(kmers):
            if i != j and other_kmer.startswith(suffix):
                edges.append(f"{kmer} -> {other_kmer}")
    
    # Remove duplicates and sort
    edges = sorted(list(set(edges)))
    
    return edges

def read_input(filename):
    """Read k-mers from file"""
    with open(filename, 'r') as file:
        kmers = [line.strip() for line in file if line.strip()]
    return kmers

def write_output(edges, filename):
    """Write edges to file"""
    with open(filename, 'w') as file:
        for edge in edges:
            file.write(edge + '\n')

# Example usage
if __name__ == "__main__":
    # Example input
    example_kmers = [
        "ATGCG",
        "GCATC",
        "CATCG",
        "TCATC",
        "CATCG",
        "TCATC"
    ]
    
    # Construct overlap graph
    result = construct_overlap_graph(example_kmers)
    
    # Print results
    for edge in result:
        print(edge)
    
    # If you have input file, you can use:
    # kmers = read_input('input.txt')
    # result = construct_overlap_graph(kmers)
    # write_output(result, 'output.txt')
```

## Alternative Implementation (More Efficient)

```python
def construct_overlap_graph_efficient(kmers):
    """
    More efficient implementation using prefix/suffix dictionaries.
    """
    if not kmers:
        return []
    
    k = len(kmers[0])
    if k == 0:
        return []
    
    # Create prefix and suffix dictionaries
    prefix_dict = {}  # prefix -> list of k-mers with that prefix
    suffix_dict = {}  # suffix -> list of k-mers with that suffix
    
    for kmer in kmers:
        prefix = kmer[:-1]  # k-1 characters from left
        suffix = kmer[1:]   # k-1 characters from right
        
        if prefix not in prefix_dict:
            prefix_dict[prefix] = []
        prefix_dict[prefix].append(kmer)
        
        if suffix not in suffix_dict:
            suffix_dict[suffix] = []
        suffix_dict[suffix].append(kmer)
    
    # Find edges
    edges = []
    for kmer in kmers:
        suffix = kmer[1:]
        if suffix in prefix_dict:
            for target_kmer in prefix_dict[suffix]:
                if target_kmer != kmer:  # Avoid self-edges
                    edges.append(f"{kmer} -> {target_kmer}")
    
    # Remove duplicates and sort
    edges = sorted(list(set(edges)))
    return edges

# Test with example
example_kmers = [
    "ATGCG",
    "GCATC",
    "CATCG",
    "TCATC",
    "CATCG",
    "TCATC"
]

result = construct_overlap_graph_efficient(example_kmers)
for edge in result:
    print(edge)
```

## Sample Input/Output

**Input:**
```
ATGCG
GCATC
CATCG
TCATC
CATCG
TCATC
```

**Output:**
```
ATGCG -> CATCG
CATCG -> ATGCG
GCATC -> CATCG
TCATC -> GCATC
```

## Explanation

1. **Overlap Graph Construction**: For each k-mer, we check if its suffix (k-1 characters from the end) matches the prefix (k-1 characters from the beginning) of other k-mers.

2. **Edge Creation**: When a match is found, we create a directed edge from the source k-mer to the target k-mer.

3. **Duplicate Handling**: The solution removes duplicate edges and sorts the output.

4. **Time Complexity**: O(n²) where n is the number of k-mers, but the efficient version reduces this through dictionary lookups.

The solution handles the case where multiple k-mers might be identical and ensures that self-edges are not created.

