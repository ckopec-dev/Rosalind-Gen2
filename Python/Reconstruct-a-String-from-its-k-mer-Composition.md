# Rosalind Problem: Reconstruct a String from its k-mer Composition

## Problem Description
Given a collection of k-mers, reconstruct the original string. This is the reverse of the k-mer composition problem.

## Approach
1. Build a de Bruijn graph from the k-mers
2. Find an Eulerian path in the graph
3. Reconstruct the string by traversing the path

## Solution

```python
def get_kmers_from_string(text, k):
    """Get all k-mers from a string"""
    return [text[i:i+k] for i in range(len(text) - k + 1)]

def build_debruijn_graph(kmers):
    """Build de Bruijn graph from k-mers"""
    graph = {}
    
    for kmer in kmers:
        prefix = kmer[:-1]
        suffix = kmer[1:]
        
        if prefix not in graph:
            graph[prefix] = []
        graph[prefix].append(suffix)
    
    return graph

def find_eulerian_path(graph):
    """Find Eulerian path in the graph"""
    # Find in-degrees and out-degrees
    all_nodes = set()
    in_degree = {}
    out_degree = {}
    
    for node in graph:
        all_nodes.add(node)
        out_degree[node] = len(graph[node])
        for neighbor in graph[node]:
            all_nodes.add(neighbor)
            if neighbor not in in_degree:
                in_degree[neighbor] = 0
            in_degree[neighbor] += 1
    
    # Find start node (node with out_degree - in_degree = 1)
    start = None
    for node in all_nodes:
        if node not in in_degree:
            in_degree[node] = 0
        if out_degree[node] - in_degree[node] == 1:
            start = node
            break
    
    if start is None:
        start = list(all_nodes)[0]
    
    # Hierholzer's algorithm to find Eulerian path
    stack = [start]
    path = []
    
    while stack:
        current = stack[-1]
        if current in graph and graph[current]:
            next_node = graph[current].pop()
            stack.append(next_node)
        else:
            path.append(stack.pop())
    
    return path[::-1]

def reconstruct_string_from_path(path):
    """Reconstruct string from Eulerian path"""
    if not path:
        return ""
    
    result = path[0]
    for i in range(1, len(path)):
        result += path[i][-1]
    
    return result

def reconstruct_string_from_kmers(kmers):
    """Main function to reconstruct string from k-mers"""
    if not kmers:
        return ""
    
    # Build de Bruijn graph
    graph = build_debruijn_graph(kmers)
    
    # Find Eulerian path
    path = find_eulerian_path(graph)
    
    # Reconstruct string
    return reconstruct_string_from_path(path)

# Example usage:
if __name__ == "__main__":
    # Test with example from Rosalind
    kmer_list = ["CTTA", "GCCT", "TACC", "GGCT", "GCTT", "TTAC"]
    result = reconstruct_string_from_kmers(kmer_list)
    print(result)
    
    # Another example
    kmer_list2 = ["ATT", "TTC", "TCA", "CAA", "AAT", "ATC", "TCT", "CTT", "TTT"]
    result2 = reconstruct_string_from_kmers(kmer_list2)
    print(result2)
```

## Alternative Cleaner Implementation

```python
def reconstruct_string_from_kmers(kmers):
    """
    Reconstruct a string from its k-mer composition.
    
    Args:
        kmers: List of k-mers
        
    Returns:
        Reconstructed string
    """
    # Build de Bruijn graph
    graph = {}
    for kmer in kmers:
        prefix = kmer[:-1]
        suffix = kmer[1:]
        if prefix not in graph:
            graph[prefix] = []
        graph[prefix].append(suffix)
    
    # Find Eulerian path using Hierholzer's algorithm
    def find_eulerian_path():
        # Count in-degrees and out-degrees
        in_degree = {}
        out_degree = {}
        
        for node in graph:
            out_degree[node] = len(graph[node])
            for neighbor in graph[node]:
                in_degree[neighbor] = in_degree.get(neighbor, 0) + 1
        
        # Find start node
        start = None
        for node in out_degree:
            if node not in in_degree:
                in_degree[node] = 0
            if out_degree[node] - in_degree[node] == 1:
                start = node
                break
        
        if start is None:
            start = list(out_degree.keys())[0]
        
        # Find path
        stack = [start]
        path = []
        
        while stack:
            current = stack[-1]
            if current in graph and graph[current]:
                stack.append(graph[current].pop())
            else:
                path.append(stack.pop())
        
        return path[::-1]
    
    # Get path and reconstruct string
    path = find_eulerian_path()
    if not path:
        return ""
    
    # Reconstruct string
    result = path[0]
    for i in range(1, len(path)):
        result += path[i][-1]
    
    return result

# Read input from file (for Rosalind submission)
def read_kmers_from_file(filename):
    """Read k-mers from file"""
    with open(filename, 'r') as f:
        kmers = [line.strip() for line in f.readlines()]
    return kmers

# Example for Rosalind submission
# kmers = read_kmers_from_file('rosalind_string_reconstruction.txt')
# print(reconstruct_string_from_kmers(kmers))
```

## Key Points

1. **De Bruijn Graph Construction**: Each k-mer is represented as an edge from its prefix to its suffix
2. **Eulerian Path**: The reconstructed string corresponds to an Eulerian path in the de Bruijn graph
3. **Hierholzer's Algorithm**: Efficient algorithm for finding Eulerian paths
4. **String Reconstruction**: The path is reconstructed by taking the first character of the first k-mer and the last character of each subsequent k-mer

## Time Complexity
- Building the graph: O(n×k) where n is number of k-mers and k is k-mer length
- Finding Eulerian path: O(n)
- Overall: O(n×k)

## Space Complexity
- O(n×k) for storing the graph and path

