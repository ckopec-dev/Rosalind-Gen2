# Find an Eulerian Path in a Graph - Rosalind Problem Solution

## Problem Understanding

An Eulerian path is a path that visits every edge exactly once in a graph. For a graph to have an Eulerian path:
- All vertices have even degree, OR
- Exactly two vertices have odd degree (these are the start and end vertices)

## Solution Approach

1. Find the start vertex (vertex with odd degree, or any vertex if all have even degree)
2. Use Hierholzer's algorithm to find the Eulerian path
3. Handle the case where we need to reverse the path at the end

## Ruby Implementation

```ruby
def find_eulerian_path(graph)
  # Build adjacency list representation
  adj_list = {}
  in_degree = {}
  out_degree = {}
  
  # Initialize degrees and adjacency list
  graph.each do |edge|
    from, to = edge.split('->')
    from = from.strip
    to = to.strip
    
    adj_list[from] ||= []
    adj_list[from] << to
    
    out_degree[from] = out_degree[from].to_i + 1
    in_degree[to] = in_degree[to].to_i + 1
  end
  
  # Find start vertex (vertex with out_degree - in_degree = 1)
  start_vertex = nil
  adj_list.each do |vertex, neighbors|
    out_deg = out_degree[vertex].to_i
    in_deg = in_degree[vertex].to_i
    if out_deg - in_deg == 1
      start_vertex = vertex
      break
    end
  end
  
  # If no start vertex found, use any vertex with out_degree > 0
  start_vertex ||= adj_list.keys.find { |v| out_degree[v].to_i > 0 }
  
  # Hierholzer's algorithm
  path = []
  stack = [start_vertex]
  
  while !stack.empty?
    current = stack.last
    
    if adj_list[current] && !adj_list[current].empty?
      # Take any remaining edge
      next_vertex = adj_list[current].pop
      stack << next_vertex
    else
      # No more edges from current vertex
      path << stack.pop
    end
  end
  
  # Reverse the path to get correct order
  path.reverse.join('->')
end

# Alternative implementation for multiple edges
def find_eulerian_path_v2(edges)
  # Build adjacency list with edge tracking
  adj_list = {}
  edge_count = {}
  
  edges.each do |edge|
    from, to = edge.split('->')
    from = from.strip
    to = to.strip
    
    adj_list[from] ||= []
    adj_list[from] << to
    adj_list[to] ||= []
    
    # Track edge count for debugging
    edge_key = "#{from}#{to}"
    edge_count[edge_key] = edge_count[edge_key].to_i + 1
  end
  
  # Find vertices with odd degree
  odd_vertices = []
  adj_list.each do |vertex, neighbors|
    if neighbors.length.odd?
      odd_vertices << vertex
    end
  end
  
  # Start vertex is the one with odd degree, or any vertex if all even
  start_vertex = odd_vertices.first || adj_list.keys.first
  
  # Hierholzer's algorithm
  path = []
  stack = [start_vertex]
  
  while !stack.empty?
    current = stack.last
    
    if adj_list[current] && !adj_list[current].empty?
      # Take the next available edge
      next_vertex = adj_list[current].pop
      stack << next_vertex
    else
      # No more edges from current vertex
      path << stack.pop
    end
  end
  
  # Format path
  path.reverse.join('->')
end

# Main solution function
def solve_eulerian_path(input_data)
  # Parse input
  edges = input_data.split("\n").reject(&:empty?)
  
  # Find Eulerian path
  result = find_eulerian_path_v2(edges)
  
  result
end

# Example usage:
# Input format:
# 0->3
# 1->0
# 2->1
# 3->2
# 4->3
# 5->4
# 
# Expected output: 5->4->3->2->1->0->3

# Test with sample input
sample_input = <<~INPUT
  0->3
  1->0
  2->1
  3->2
  4->3
  5->4
INPUT

puts solve_eulerian_path(sample_input)
```

## Key Points

1. **Graph Representation**: Uses adjacency list to store the graph
2. **Degree Calculation**: Calculates in-degrees and out-degrees for each vertex
3. **Start Vertex Selection**: Finds the vertex with out-degree - in-degree = 1, or any vertex if all have even degree
4. **Hierholzer's Algorithm**: 
   - Uses a stack to traverse edges
   - When no more edges from a vertex, adds it to path
   - Reverses the final path to get correct order

## Time Complexity
- **Time**: O(E + V) where E is number of edges and V is number of vertices
- **Space**: O(E + V) for storing the adjacency list and stack

## Sample Input/Output

**Input:**
```
0->3
1->0
2->1
3->2
4->3
5->4
```

**Output:**
```
5->4->3->2->1->0->3
```

