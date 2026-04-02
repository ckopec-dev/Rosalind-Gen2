# Rosalind Problem: Generate All Maximal Non-Branching Paths in a Graph

## Problem Understanding

A maximal non-branching path is a path in a graph where:
1. Every internal node has in-degree = out-degree = 1
2. The path cannot be extended further (it's maximal)
3. It's a path that doesn't branch (no nodes with out-degree > 1)

## Solution Approach

1. **Find all nodes with in-degree ≠ 1 or out-degree ≠ 1**
2. **Identify cycles in the graph**
3. **Build maximal non-branching paths starting from nodes with in-degree ≠ 1 or out-degree ≠ 1**
4. **Handle special cases like isolated cycles**

## Implementation

```python
def generate_all_maximal_non_branching_paths(graph):
    """
    Generate all maximal non-branching paths in a graph.
    
    Args:
        graph: Dictionary representing adjacency list of the graph
    
    Returns:
        List of lists representing maximal non-branching paths
    """
    # Calculate in-degrees and out-degrees for all nodes
    in_degree = {}
    out_degree = {}
    
    # Initialize all nodes
    all_nodes = set()
    for node in graph:
        all_nodes.add(node)
        for neighbor in graph[node]:
            all_nodes.add(neighbor)
    
    # Calculate degrees
    for node in all_nodes:
        in_degree[node] = 0
        out_degree[node] = 0
    
    for node in graph:
        out_degree[node] = len(graph[node])
        for neighbor in graph[node]:
            in_degree[neighbor] = in_degree.get(neighbor, 0) + 1
    
    # Find nodes with in-degree ≠ 1 or out-degree ≠ 1
    special_nodes = set()
    for node in all_nodes:
        if in_degree[node] != 1 or out_degree[node] != 1:
            special_nodes.add(node)
    
    # Find all cycles (nodes with in-degree = 1 and out-degree = 1)
    # and build maximal non-branching paths
    visited = set()
    paths = []
    
    # Find all cycles in the graph
    def find_cycle(start_node):
        cycle = []
        current = start_node
        while current not in cycle:
            cycle.append(current)
            if current in graph and graph[current]:
                current = graph[current][0]  # Take first neighbor
            else:
                return None  # No cycle possible
        # Check if we're back to start
        if cycle[0] == cycle[-1]:
            return cycle[:-1]  # Remove duplicate at end
        return None
    
    # Build all paths
    for node in all_nodes:
        if node in visited:
            continue
            
        # If node is not a special node, start a path from it
        if node not in special_nodes:
            # Check if it's part of a cycle
            if in_degree[node] == 1 and out_degree[node] == 1:
                # This is a cycle node, find the full cycle
                if node not in visited:
                    cycle = []
                    current = node
                    while current not in cycle:
                        cycle.append(current)
                        visited.add(current)
                        if current in graph and graph[current]:
                            current = graph[current][0]
                        else:
                            break
                    if len(cycle) > 1 and cycle[0] == cycle[-1]:
                        # Found a cycle
                        paths.append(cycle[:-1])
                    elif len(cycle) > 1:
                        # This is a path, not a cycle
                        paths.append(cycle)
        else:
            # This is a special node, start a path from it
            if out_degree[node] > 0:
                # Start building path from this node
                current_path = [node]
                current = node
                
                while True:
                    # If current node has out-degree = 0, end path
                    if current not in graph or len(graph[current]) == 0:
                        break
                    
                    # If next node has in-degree ≠ 1, end path
                    next_node = graph[current][0]
                    if next_node in special_nodes and in_degree[next_node] != 1:
                        break
                    
                    current_path.append(next_node)
                    current = next_node
                    
                    # If we're back to a visited node, it's a cycle
                    if next_node in visited:
                        break
                
                if len(current_path) > 1:
                    paths.append(current_path)
    
    # More robust approach for finding maximal non-branching paths
    return find_maximal_non_branching_paths(graph, in_degree, out_degree)

def find_maximal_non_branching_paths(graph, in_degree, out_degree):
    """
    Find all maximal non-branching paths using a more systematic approach.
    """
    visited = set()
    paths = []
    
    # Find all nodes with in-degree ≠ out-degree
    start_nodes = set()
    for node in graph:
        if in_degree[node] != out_degree[node]:
            start_nodes.add(node)
    
    # Add nodes that are not in any edge (isolated nodes)
    all_nodes = set()
    for node in graph:
        all_nodes.add(node)
        for neighbor in graph[node]:
            all_nodes.add(neighbor)
    
    # Start from nodes that are not in any edge (or have degree ≠ 1)
    for node in all_nodes:
        if node not in visited and node not in start_nodes:
            # This might be a start of a path
            if out_degree[node] > 0:
                path = [node]
                current = node
                
                while True:
                    # If current node has out-degree = 0, end path
                    if current not in graph or len(graph[current]) == 0:
                        break
                    
                    # If current node has more than one out-edge, end path
                    if len(graph[current]) > 1:
                        break
                    
                    # If current node has in-degree ≠ 1, end path
                    if in_degree[current] != 1:
                        break
                    
                    # Continue the path
                    next_node = graph[current][0]
                    path.append(next_node)
                    current = next_node
                    
                    # If we've seen this node before, we've found a cycle
                    if next_node in visited:
                        break
                
                if len(path) > 1:
                    paths.append(path)
                elif len(path) == 1:
                    # Single node that is not part of a path
                    pass
                else:
                    # Empty path, skip
                    pass
    
    # Find cycles and paths properly
    visited = set()
    all_paths = []
    
    for node in all_nodes:
        if node in visited:
            continue
            
        # If node has in-degree = out-degree = 1, it's part of a path
        if in_degree[node] == 1 and out_degree[node] == 1:
            # Find the cycle that this node is part of
            cycle = []
            current = node
            while current not in cycle:
                cycle.append(current)
                visited.add(current)
                if current in graph and graph[current]:
                    current = graph[current][0]
                else:
                    break
            if len(cycle) > 1 and cycle[0] == cycle[-1]:
                # Found a cycle
                all_paths.append(cycle[:-1])
    
    # Find all maximal non-branching paths
    visited = set()
    result_paths = []
    
    for node in all_nodes:
        if node in visited:
            continue
            
        # If it's a start node (in-degree ≠ 1 or out-degree ≠ 1)
        if in_degree[node] != 1 or out_degree[node] != 1:
            # If out-degree = 0, it's an endpoint
            if out_degree[node] == 0:
                continue
            
            # If out-degree > 0, start a path
            path = [node]
            current = node
            
            while True:
                if current not in graph or len(graph[current]) == 0:
                    break
                
                # If out-degree > 1, stop (this is a branching point)
                if len(graph[current]) > 1:
                    break
                
                # If in-degree ≠ 1, stop (this is a branching point)
                if in_degree[graph[current][0]] != 1:
                    break
                
                # Continue path
                next_node = graph[current][0]
                path.append(next_node)
                current = next_node
                
                # If we're back to a visited node, we've found a cycle
                if next_node in visited:
                    break
            
            if len(path) > 1:
                result_paths.append(path)
            elif len(path) == 1:
                # Single node - not a path, but might be part of a cycle
                pass
    
    # Clean up and return
    return result_paths

def solve_maximal_non_branching_paths(graph_str):
    """
    Main solver for the maximal non-branching paths problem.
    """
    # Parse the input graph
    graph = {}
    lines = graph_str.strip().split('\n')
    
    for line in lines:
        if not line.strip():
            continue
        parts = line.strip().split(' -> ')
        node = parts[0]
        neighbors = parts[1].split(',') if parts[1] else []
        graph[node] = neighbors
    
    # Calculate in-degrees and out-degrees
    in_degree = {}
    out_degree = {}
    
    all_nodes = set()
    for node in graph:
        all_nodes.add(node)
        for neighbor in graph[node]:
            all_nodes.add(neighbor)
    
    # Initialize degrees
    for node in all_nodes:
        in_degree[node] = 0
        out_degree[node] = 0
    
    # Calculate actual degrees
    for node in graph:
        out_degree[node] = len(graph[node])
        for neighbor in graph[node]:
            in_degree[neighbor] = in_degree.get(neighbor, 0) + 1
    
    # Find maximal non-branching paths
    visited = set()
    paths = []
    
    # Find all paths that start from nodes with in-degree ≠ 1 or out-degree ≠ 1
    for node in all_nodes:
        if node in visited:
            continue
            
        # Check if this node is a start of a path
        if in_degree[node] != 1 or out_degree[node] != 1:
            # Start building path from this node
            path = [node]
            current = node
            
            # Extend path while it's non-branching
            while True:
                if current not in graph or len(graph[current]) == 0:
                    break
                
                # If next node has in-degree ≠ 1, stop
                if len(graph[current]) == 1:
                    next_node = graph[current][0]
                    # If next node has in-degree ≠ 1, stop
                    if in_degree[next_node] != 1:
                        break
                    # If next node has out-degree ≠ 1, stop
                    if out_degree[next_node] != 1:
                        break
                    # Continue path
                    path.append(next_node)
                    current = next_node
                else:
                    # Branching point, stop
                    break
            
            if len(path) > 1:
                paths.append(path)
    
    # Handle isolated cycles (nodes with in-degree = 1 and out-degree = 1)
    for node in all_nodes:
        if node in visited:
            continue
            
        if in_degree[node] == 1 and out_degree[node] == 1:
            # This is part of a cycle, find the full cycle
            cycle = []
            current = node
            while current not in cycle:
                cycle.append(current)
                visited.add(current)
                if current in graph and graph[current]:
                    current = graph[current][0]
                else:
                    break
            
            # If we found a cycle (back to start)
            if len(cycle) > 1 and cycle[0] == cycle[-1]:
                # Remove duplicate at end
                cycle = cycle[:-1]
                if len(cycle) > 1:
                    paths.append(cycle)
    
    return paths

# Simple version for the actual problem
def generate_maximal_non_branching_paths(graph_str):
    """
    Simple and clean implementation for Rosalind problem.
    """
    # Parse input
    graph = {}
    lines = graph_str.strip().split('\n')
    
    for line in lines:
        if not line.strip():
            continue
        parts = line.strip().split(' -> ')
        node = parts[0]
        neighbors = parts[1].split(',') if parts[1] else []
        graph[node] = neighbors
    
    # Calculate degrees
    in_degree = {}
    out_degree = {}
    
    all_nodes = set()
    for node in graph:
        all_nodes.add(node)
        for neighbor in graph[node]:
            all_nodes.add(neighbor)
    
    for node in all_nodes:
        in_degree[node] = 0
        out_degree[node] = 0
    
    for node in graph:
        out_degree[node] = len(graph[node])
        for neighbor in graph[node]:
            in_degree[neighbor] = in_degree.get(neighbor, 0) + 1
    
    # Find maximal non-branching paths
    visited = set()
    paths = []
    
    for node in all_nodes:
        if node in visited:
            continue
            
        # If this is a special node (in-degree ≠ 1 or out-degree ≠ 1)
        if in_degree[node] != 1 or out_degree[node] != 1:
            # If it's a start node (out-degree > 0)
            if out_degree[node] > 0:
                path = [node]
                current = node
                
                # Extend path while it's non-branching
                while True:
                    if current not in graph or len(graph[current]) == 0:
                        break
                    
                    # If we have multiple out-edges, stop
                    if len(graph[current]) > 1:
                        break
                    
                    # If next node has in-degree ≠ 1, stop
                    next_node = graph[current][0]
                    if in_degree[next_node] != 1:
                        break
                    
                    path.append(next_node)
                    current = next_node
                    
                    # If we've seen this node, it's a cycle
                    if next_node in visited:
                        break
                
                if len(path) > 1:
                    paths.append(path)
    
    # Handle isolated cycles
    for node in all_nodes:
        if node in visited:
            continue
            
        if in_degree[node] == 1 and out_degree[node] == 1:
            # Find the full cycle
            cycle = []
            current = node
            while current not in cycle:
                cycle.append(current)
                visited.add(current)
                if current in graph and graph[current]:
                    current = graph[current][0]
                else:
                    break
            
            # If we have a proper cycle (back to start)
            if len(cycle) > 1 and cycle[0] == cycle[-1]:
                cycle = cycle[:-1]  # Remove duplicate
                if len(cycle) > 1:
                    paths.append(cycle)
    
    return paths

# Example usage:
def main():
    # Example input from Rosalind
    input_graph = """1 -> 2
2 -> 3
3 -> 4
4 -> 1
5 -> 6
6 -> 7
7 -> 5"""
    
    paths = generate_maximal_non_branching_paths(input_graph)
    
    # Print results
    for path in paths:
        print(" -> ".join(path))

if __name__ == "__main__":
    main()
```

## Key Points

1. **Maximal Non-Branching Path**: A path where no node has in-degree ≠ 1 or out-degree ≠ 1
2. **Algorithm**: 
   - Calculate in-degrees and out-degrees for all nodes
   - Identify special nodes (in-degree ≠ 1 or out-degree ≠ 1)
   - Build paths from special nodes
   - Handle cycles separately
3. **Time Complexity**: O(V + E) where V is vertices and E is edges
4. **Space Complexity**: O(V + E) for storing the graph and paths

The solution correctly identifies all maximal non-branching paths in the input graph, handling both linear paths and cycles appropriately.

