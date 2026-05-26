# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Description
Given a directed acyclic graph (DAG), find a topological ordering of its vertices.

## Solution Approach
I'll use Kahn's algorithm for topological sorting:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. Process vertices from queue, reducing in-degrees of neighbors
4. Add neighbors with in-degree 0 to queue
5. Continue until queue is empty

## Python Implementation

```python
from collections import deque, defaultdict

def find_topological_ordering(edges, num_vertices):
    """
    Find a topological ordering of a DAG using Kahn's algorithm.
    
    Args:
        edges: List of tuples representing directed edges (from, to)
        num_vertices: Number of vertices in the graph
    
    Returns:
        List representing topological ordering, or empty list if cycle detected
    """
    # Build adjacency list and in-degree array
    graph = defaultdict(list)
    in_degree = [0] * (num_vertices + 1)  # 1-indexed
    
    for from_node, to_node in edges:
        graph[from_node].append(to_node)
        in_degree[to_node] += 1
    
    # Find all nodes with in-degree 0
    queue = deque()
    for i in range(1, num_vertices + 1):
        if in_degree[i] == 0:
            queue.append(i)
    
    result = []
    
    # Process nodes in topological order
    while queue:
        current = queue.popleft()
        result.append(current)
        
        # Reduce in-degree of neighbors
        for neighbor in graph[current]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)
    
    # Check if all nodes were processed (no cycles)
    if len(result) == num_vertices:
        return result
    else:
        return []  # Cycle detected

# Read input and solve
def solve():
    # Read input from stdin (assuming standard Rosalind format)
    import sys
    
    input_lines = []
    for line in sys.stdin:
        input_lines.append(line.strip())
    
    # Parse the input
    # First line: number of vertices and edges
    n, m = map(int, input_lines[0].split())
    
    # Parse edges
    edges = []
    for i in range(1, m + 1):
        from_node, to_node = map(int, input_lines[i].split())
        edges.append((from_node, to_node))
    
    # Find topological ordering
    ordering = find_topological_ordering(edges, n)
    
    # Output result
    if ordering:
        print(' '.join(map(str, ordering)))
    else:
        print("No valid topological ordering (cycle detected)")

# Alternative: Direct function for testing
def solve_rosalind_problem(edges_input, num_vertices):
    """
    Solve the problem with given input parameters.
    
    Args:
        edges_input: List of tuples (from, to)
        num_vertices: Number of vertices
    
    Returns:
        Topological ordering as list of integers
    """
    return find_topological_ordering(edges_input, num_vertices)

# Example usage:
if __name__ == "__main__":
    # Example from Rosalind problem
    # Sample input:
    # 6 8
    # 1 2
    # 1 3
    # 2 4
    # 3 4
    # 4 5
    # 4 6
    # 5 6
    # 2 5
    
    sample_edges = [
        (1, 2), (1, 3), (2, 4), (3, 4),
        (4, 5), (4, 6), (5, 6), (2, 5)
    ]
    
    result = solve_rosalind_problem(sample_edges, 6)
    print("Topological ordering:", ' '.join(map(str, result)))
```

## How it works:

1. **Build Graph Representation**: Create adjacency list and in-degree array
2. **Initialize Queue**: Add all vertices with in-degree 0 to queue
3. **Process Vertices**: 
   - Remove vertex from queue
   - Add to result list
   - Decrease in-degrees of neighbors
   - Add neighbors with in-degree 0 to queue
4. **Check for Cycles**: If result length != number of vertices, there's a cycle

## Time Complexity: O(V + E)
## Space Complexity: O(V + E)

## Sample Input/Output:
Input:
```
6 8
1 2
1 3
2 4
3 4
4 5
4 6
5 6
2 5
```

Output:
```
1 2 3 4 5 6
```

This solution handles the standard Rosalind input format and returns a valid topological ordering of the DAG.

