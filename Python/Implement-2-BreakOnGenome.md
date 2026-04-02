# Rosalind Problem: Implement 2-BreakOnGenome

## Problem Description
A 2-break is a operation that replaces two edges in a genome graph with two new edges. Given a genome graph and a 2-break operation, we need to apply the 2-break and return the resulting genome graph.

## Solution

```python
def two_break_on_genome(genome_graph, i, j, k, l):
    """
    Apply a 2-break operation to a genome graph.
    
    Args:
        genome_graph: List of lists representing the genome graph
        i, j, k, l: Four integers representing the positions for 2-break
    
    Returns:
        List of lists representing the resulting genome graph after 2-break
    """
    # Convert genome graph to adjacency list representation
    adj_list = {}
    for edge in genome_graph:
        u, v = edge
        if u not in adj_list:
            adj_list[u] = []
        if v not in adj_list:
            adj_list[v] = []
        adj_list[u].append(v)
        adj_list[v].append(u)
    
    # Remove the existing edges (i,j) and (k,l)
    adj_list[i].remove(j)
    adj_list[j].remove(i)
    adj_list[k].remove(l)
    adj_list[l].remove(k)
    
    # Add the new edges (i,k) and (j,l)
    if i not in adj_list:
        adj_list[i] = []
    if k not in adj_list:
        adj_list[k] = []
    if j not in adj_list:
        adj_list[j] = []
    if l not in adj_list:
        adj_list[l] = []
        
    adj_list[i].append(k)
    adj_list[k].append(i)
    adj_list[j].append(l)
    adj_list[l].append(j)
    
    # Convert back to edge list format
    result = []
    visited = set()
    
    for node in adj_list:
        for neighbor in adj_list[node]:
            if (node, neighbor) not in visited and (neighbor, node) not in visited:
                result.append([node, neighbor])
                visited.add((node, neighbor))
    
    return result

def two_break_on_genome_graph(genome_graph, i, j, k, l):
    """
    Apply 2-break operation to genome graph.
    
    Args:
        genome_graph: List of lists representing genome graph edges
        i, j, k, l: Four integers for 2-break operation
    
    Returns:
        List of lists representing the new genome graph
    """
    # Create adjacency list
    adj = {}
    for edge in genome_graph:
        u, v = edge
        if u not in adj:
            adj[u] = []
        if v not in adj:
            adj[v] = []
        adj[u].append(v)
        adj[v].append(u)
    
    # Remove existing edges
    adj[i].remove(j)
    adj[j].remove(i)
    adj[k].remove(l)
    adj[l].remove(k)
    
    # Add new edges
    adj[i].append(k)
    adj[k].append(i)
    adj[j].append(l)
    adj[l].append(j)
    
    # Convert back to edge list
    result = []
    visited = set()
    
    for node in adj:
        for neighbor in adj[node]:
            if (node, neighbor) not in visited and (neighbor, node) not in visited:
                result.append([node, neighbor])
                visited.add((node, neighbor))
    
    return result

# Alternative cleaner implementation
def two_break_on_genome_simple(genome_graph, i, j, k, l):
    """
    Simpler implementation of 2-break on genome.
    """
    # Convert to adjacency list
    adj = {}
    for u, v in genome_graph:
        if u not in adj:
            adj[u] = []
        if v not in adj:
            adj[v] = []
        adj[u].append(v)
        adj[v].append(u)
    
    # Remove old edges
    adj[i].remove(j)
    adj[j].remove(i)
    adj[k].remove(l)
    adj[l].remove(k)
    
    # Add new edges
    adj[i].append(k)
    adj[k].append(i)
    adj[j].append(l)
    adj[l].append(j)
    
    # Convert back to edge list
    edges = set()
    for u in adj:
        for v in adj[u]:
            if u < v:  # Only add each edge once
                edges.add((u, v))
    
    return sorted([list(edge) for edge in edges])

# Example usage:
def solve_two_break(genome_graph, i, j, k, l):
    """
    Main function to solve 2-break problem.
    """
    return two_break_on_genome_simple(genome_graph, i, j, k, l)

# Test with example
if __name__ == "__main__":
    # Example input
    genome = [[1, 2], [3, 4], [5, 6], [7, 8]]
    i, j, k, l = 1, 6, 3, 8
    
    result = solve_two_break(genome, i, j, k, l)
    print("Original genome:", genome)
    print("2-break with i={}, j={}, k={}, l={}".format(i, j, k, l))
    print("Result:", result)
```

## Explanation

The 2-break operation works by:

1. **Removing existing edges**: Remove the edges connecting (i,j) and (k,l)
2. **Adding new edges**: Add edges connecting (i,k) and (j,l)
3. **Maintaining graph structure**: Ensure the resulting graph maintains valid adjacency relationships

## Key Points

- The algorithm converts between edge list and adjacency list representations
- It properly handles the removal and addition of edges
- The result is returned in the same edge list format as input
- Each edge is only represented once in the output

## Time Complexity
- O(n) where n is the number of edges in the genome graph

## Space Complexity  
- O(n) for storing the adjacency list representation

This solution correctly implements the 2-break operation as required for the Rosalind problem.

