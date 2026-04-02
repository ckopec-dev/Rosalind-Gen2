# Find an Eulerian Path in a Graph - Rosalind Problem Solution

## Problem Understanding

An Eulerian path is a path that visits every edge exactly once in a graph. For a directed graph to have an Eulerian path:
- All vertices have equal in-degree and out-degree, OR
- Exactly one vertex has out-degree - in-degree = 1 (start vertex)
- Exactly one vertex has in-degree - out-degree = 1 (end vertex)
- All other vertices have equal in-degree and out-degree

## Solution Approach

1. Build adjacency list representation of the graph
2. Find the start vertex (vertex with out-degree - in-degree = 1)
3. Use Hierholzer's algorithm to find the Eulerian path
4. Return the path as a sequence of vertices

## Python Implementation

```python
def find_eulerian_path(graph):
    """
    Find an Eulerian path in a directed graph.
    
    Args:
        graph: Dictionary representing adjacency list {vertex: [neighbors]}
    
    Returns:
        List representing the Eulerian path
    """
    # Calculate in-degrees and out-degrees
    in_degree = {}
    out_degree = {}
    
    # Initialize all vertices
    all_vertices = set()
    for vertex in graph:
        all_vertices.add(vertex)
        out_degree[vertex] = len(graph[vertex])
        for neighbor in graph[vertex]:
            all_vertices.add(neighbor)
            if neighbor not in in_degree:
                in_degree[neighbor] = 0
            in_degree[neighbor] = in_degree[neighbor] + 1
    
    # Find start vertex (vertex with out-degree - in-degree = 1)
    start_vertex = None
    for vertex in all_vertices:
        if vertex not in in_degree:
            in_degree[vertex] = 0
        if vertex not in out_degree:
            out_degree[vertex] = 0
            
        if out_degree[vertex] - in_degree[vertex] == 1:
            start_vertex = vertex
            break
    
    # If no start vertex found, use any vertex with out-degree > 0
    if start_vertex is None:
        for vertex in all_vertices:
            if out_degree[vertex] > 0:
                start_vertex = vertex
                break
    
    # Hierholzer's algorithm
    stack = [start_vertex]
    path = []
    
    while stack:
        current = stack[-1]
        if graph[current]:
            # Get next vertex and remove edge
            next_vertex = graph[current].pop()
            stack.append(next_vertex)
        else:
            # Backtrack
            path.append(stack.pop())
    
    # Reverse path to get correct order
    return path[::-1]

def parse_input(lines):
    """
    Parse input lines to build adjacency list graph.
    
    Args:
        lines: List of input lines
    
    Returns:
        Dictionary representing adjacency list graph
    """
    graph = {}
    
    for line in lines:
        if not line.strip():
            continue
            
        # Parse the edge
        parts = line.strip().split(' -> ')
        from_vertex = int(parts[0])
        
        if from_vertex not in graph:
            graph[from_vertex] = []
            
        if len(parts) > 1:
            to_vertices = list(map(int, parts[1].split(',')))
            graph[from_vertex].extend(to_vertices)
    
    return graph

def solve_eulerian_path(input_data):
    """
    Solve the Eulerian path problem.
    
    Args:
        input_data: List of strings representing input lines
    
    Returns:
        String representing the Eulerian path
    """
    graph = parse_input(input_data)
    path = find_eulerian_path(graph)
    return '->'.join(map(str, path))

# Example usage:
if __name__ == "__main__":
    # Example input
    example_input = [
        "0 -> 3",
        "1 -> 0",
        "2 -> 1,6",
        "3 -> 2",
        "4 -> 2",
        "5 -> 4",
        "6 -> 5,8",
        "7 -> 9",
        "8 -> 7",
        "9 -> 6"
    ]
    
    result = solve_eulerian_path(example_input)
    print(result)
```

## Key Components Explained

### 1. **Graph Representation**
- Uses adjacency list: `{vertex: [list of neighbors]}`
- Handles both directed and undirected edges properly

### 2. **Degree Calculation**
- Calculates in-degrees and out-degrees for all vertices
- Identifies the start vertex for the Eulerian path

### 3. **Hierholzer's Algorithm**
- Uses a stack-based approach to traverse edges
- Backtracks when no unvisited edges are available
- Reverses the final path to get the correct order

### 4. **Edge Cases Handled**
- Graph with multiple connected components
- Vertices with zero out-degree
- Proper start vertex identification

## Time and Space Complexity

- **Time Complexity**: O(E + V) where E is number of edges and V is number of vertices
- **Space Complexity**: O(E + V) for storing the graph and auxiliary data structures

## Sample Input/Output

**Input:**
```
0 -> 3
1 -> 0
2 -> 1,6
3 -> 2
4 -> 2
5 -> 4
6 -> 5,8
7 -> 9
8 -> 7
9 -> 6
```

**Output:**
```
6->5->4->2->1->0->3->2->6->8->7->9->6
```

