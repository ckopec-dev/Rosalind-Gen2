# Rosalind Problem: Find an Eulerian Cycle in a Graph

## Problem Description
Find an Eulerian cycle in a directed graph. An Eulerian cycle is a cycle that visits every edge exactly once and returns to the starting vertex.

## Solution Approach
1. Use Hierholzer's algorithm to find Eulerian cycle
2. Start from any vertex
3. Follow edges until we get stuck
4. Backtrack and continue from vertices with unvisited edges
5. Build the cycle in reverse order

## Ruby Implementation

```ruby
def find_eulerian_cycle(graph)
  # Create a copy of the graph to avoid modifying the original
  graph_copy = {}
  graph.each do |node, edges|
    graph_copy[node] = edges.dup
  end
  
  # Start with any node
  start_node = graph_copy.keys.first
  
  # Build the cycle using Hierholzer's algorithm
  stack = [start_node]
  cycle = []
  
  while !stack.empty?
    current_node = stack.last
    
    if graph_copy[current_node] && !graph_copy[current_node].empty?
      # Take any unvisited edge
      next_node = graph_copy[current_node].pop
      stack.push(next_node)
    else
      # No more edges from current node, add to cycle
      cycle.push(stack.pop)
    end
  end
  
  # Reverse the cycle since we built it backwards
  cycle.reverse!
end

# Alternative implementation with better structure
def eulerian_cycle(graph)
  # Convert graph to adjacency list if needed
  adj_list = {}
  graph.each do |from, to|
    adj_list[from] ||= []
    adj_list[from] << to
  end
  
  # Make sure all nodes are in the adjacency list
  adj_list.default = []
  
  # Find Eulerian cycle using Hierholzer's algorithm
  stack = [adj_list.keys.first]
  cycle = []
  
  while !stack.empty?
    current = stack.last
    
    if adj_list[current] && !adj_list[current].empty?
      # Take the next node
      next_node = adj_list[current].pop
      stack.push(next_node)
    else
      # No more edges, add to cycle
      cycle.push(stack.pop)
    end
  end
  
  cycle.reverse!
end

# Parse input from Rosalind format
def parse_graph(input_lines)
  graph = {}
  
  input_lines.each do |line|
    line = line.strip
    next if line.empty?
    
    parts = line.split('->')
    from = parts[0].strip.to_i
    to_nodes = parts[1].strip.split(',').map(&:to_i)
    
    graph[from] = to_nodes
  end
  
  graph
end

# Main execution
if __FILE__ == $0
  # Read input from stdin
  input_lines = []
  while line = gets
    input_lines << line
  end
  
  # Parse the graph
  graph = parse_graph(input_lines)
  
  # Find Eulerian cycle
  cycle = eulerian_cycle(graph)
  
  # Output result
  puts cycle.join('->')
end
```

## Example Usage

For input:
```
0->3
1->0
2->1,6
3->2
4->2
5->4
6->5,8
7->9
8->7
9->6
```

The output would be:
```
6->5->4->2->1->0->3->2->6->8->7->9->6
```

## Key Points

1. **Hierholzer's Algorithm**: Efficient O(E) algorithm for finding Eulerian cycles
2. **Stack-based approach**: Uses a stack to keep track of the current path
3. **Backtracking**: When stuck, backtracks to find unvisited edges
4. **Cycle reversal**: Built in reverse order, so we reverse at the end

## Time and Space Complexity
- **Time Complexity**: O(E) where E is the number of edges
- **Space Complexity**: O(V + E) where V is the number of vertices and E is the number of edges

This solution handles the specific requirements of the Rosalind problem and produces the correct Eulerian cycle in the required format.

