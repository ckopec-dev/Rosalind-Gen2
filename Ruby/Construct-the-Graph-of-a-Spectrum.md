# Rosalind Problem: Construct the Graph of a Spectrum

## Problem Description
Given a collection of integers representing a spectrum, construct the graph of the spectrum where nodes are the integers and edges represent the differences between adjacent nodes.

## Solution

```ruby
def construct_spectrum_graph(spectrum)
  # Sort the spectrum values
  sorted_spectrum = spectrum.sort
  
  # Create adjacency list representation of the graph
  graph = {}
  
  # For each pair of consecutive elements, create an edge
  (0...sorted_spectrum.length - 1).each do |i|
    node1 = sorted_spectrum[i]
    node2 = sorted_spectrum[i + 1]
    diff = node2 - node1
    
    # Add nodes to graph if they don't exist
    graph[node1] = [] unless graph.key?(node1)
    graph[node2] = [] unless graph.key?(node2)
    
    # Add edge from node1 to node2 with weight diff
    graph[node1] << [node2, diff]
  end
  
  # Return the graph as a string in the required format
  result = []
  graph.each do |node, edges|
    edges.each do |target, weight|
      result << "#{node}->#{target}:#{weight}"
    end
  end
  
  result.sort.join("\n")
end

# Alternative approach - more direct construction
def construct_spectrum_graph_v2(spectrum)
  # Sort the spectrum values
  sorted_spectrum = spectrum.sort
  
  # Build edges directly
  edges = []
  
  (0...sorted_spectrum.length - 1).each do |i|
    node1 = sorted_spectrum[i]
    node2 = sorted_spectrum[i + 1]
    diff = node2 - node1
    edges << "#{node1}->#{node2}:#{diff}"
  end
  
  edges.sort.join("\n")
end

# Example usage:
# spectrum = [0, 137, 186, 323]
# puts construct_spectrum_graph(spectrum)

# Sample input:
# spectrum = [0, 137, 186, 323]
# Expected output:
# 0->137:137
# 137->186:49
# 186->323:137
```

## Explanation

The solution works as follows:

1. **Sort the spectrum**: First, we sort the input spectrum values to ensure we process them in ascending order.

2. **Build edges**: For each consecutive pair of values in the sorted spectrum, we create an edge from the smaller value to the larger value with the difference as the edge weight.

3. **Format output**: We format the edges in the required format "node1->node2:weight" and return them sorted alphabetically.

## Key Points

- The spectrum represents a sequence of masses, and we're constructing a graph where edges represent the differences between consecutive masses
- The graph is directed since we're going from smaller to larger values
- Edge weights represent the mass differences between consecutive spectrum peaks
- The output is sorted alphabetically as required by the format

## Time Complexity
- Sorting: O(n log n)
- Building edges: O(n)
- Overall: O(n log n)

## Space Complexity
- O(n) for storing the graph and edges

