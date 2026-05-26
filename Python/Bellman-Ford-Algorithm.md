# Rosalind Problem: Bellman-Ford Algorithm

## Problem Description

The Bellman-Ford algorithm is used to find the shortest paths from a single source vertex to all other vertices in a weighted graph, even when the graph contains negative edge weights.

## Solution

```python
def bellman_ford(edges, num_vertices, source):
    """
    Implementation of Bellman-Ford algorithm to find shortest paths
    
    Args:
        edges: List of tuples (u, v, weight) representing edges
        num_vertices: Number of vertices in the graph
        source: Source vertex index (0-based)
    
    Returns:
        List of shortest distances from source to all vertices, 
        or None if negative cycle is detected
    """
    # Initialize distances
    dist = [float('inf')] * num_vertices
    dist[source] = 0
    
    # Relax edges repeatedly
    for _ in range(num_vertices - 1):
        updated = False
        for u, v, weight in edges:
            if dist[u] != float('inf') and dist[u] + weight < dist[v]:
                dist[v] = dist[u] + weight
                updated = True
        if not updated:
            break
    
    # Check for negative cycles
    for u, v, weight in edges:
        if dist[u] != float('inf') and dist[u] + weight < dist[v]:
            return None  # Negative cycle detected
    
    return dist

def solve_bellman_ford(input_data):
    """
    Solve the Bellman-Ford problem with given input data
    
    Args:
        input_data: String containing the input data
    
    Returns:
        List of distances or "No negative cycle" if negative cycle detected
    """
    lines = input_data.strip().split('\n')
    
    # Parse number of vertices and edges
    n, m = map(int, lines[0].split())
    
    # Parse edges
    edges = []
    for i in range(1, len(lines)):
        u, v, w = map(int, lines[i].split())
        edges.append((u-1, v-1, w))  # Convert to 0-based indexing
    
    # Run Bellman-Ford
    result = bellman_ford(edges, n, 0)  # Source vertex is 0
    
    if result is None:
        return "No negative cycle"
    else:
        return [str(d) if d != float('inf') else 'x' for d in result]

# Example usage:
if __name__ == "__main__":
    # Sample input
    sample_input = """6 10
1 2 8
1 3 5
1 4 2
2 4 1
2 5 3
3 4 3
3 5 2
4 5 1
4 6 1
5 6 1"""
    
    result = solve_bellman_ford(sample_input)
    print(" ".join(result))
```

## Explanation

### Algorithm Steps:

1. **Initialization**: Set distance to source vertex as 0, all other vertices as infinity
2. **Relaxation**: Repeat `V-1` times:
   - For each edge (u,v) with weight w, if `dist[u] + w < dist[v]`, update `dist[v]`
3. **Negative Cycle Detection**: Check if any edge can still be relaxed:
   - If yes, negative cycle exists
4. **Return**: Distances from source to all vertices

### Key Points:

- **Time Complexity**: O(VE) where V is vertices, E is edges
- **Space Complexity**: O(V)
- **Handles**: Negative weights, negative cycles
- **Output**: Returns distances or None if negative cycle detected

### Input Format:
- First line: N (vertices) M (edges)
- Next M lines: u v w (edge from u to v with weight w)

### Output Format:
- Space-separated distances from source to all vertices
- Use 'x' for unreachable vertices
- Return "No negative cycle" if negative cycle detected

The algorithm correctly handles the case where vertices are numbered from 1 in the input, converting them to 0-based indexing internally.

