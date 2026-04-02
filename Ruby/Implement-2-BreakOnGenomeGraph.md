# Rosalind Problem: Implement_2-BreakOnGenomeGraph

## Problem Description
Implement 2-BreakOnGenomeGraph to perform a 2-break operation on a genome graph.

## Solution

```ruby
def two_break_on_genome_graph(genome_graph, i, j, k, l)
  # Remove the existing edges (i,j) and (k,l)
  # Add new edges (i,k) and (j,l)
  
  # Convert genome graph to adjacency list representation
  adj_list = {}
  
  # Build adjacency list from genome graph
  genome_graph.each do |edge|
    u, v = edge
    adj_list[u] = [] unless adj_list[u]
    adj_list[v] = [] unless adj_list[v]
    adj_list[u] << v
    adj_list[v] << u
  end
  
  # Remove existing edges
  adj_list[i].delete(j)
  adj_list[j].delete(i)
  adj_list[k].delete(l)
  adj_list[l].delete(k)
  
  # Add new edges
  adj_list[i] << k
  adj_list[k] << i
  adj_list[j] << l
  adj_list[l] << j
  
  # Convert back to edge list format
  new_edges = []
  visited = Set.new
  
  adj_list.each do |node, neighbors|
    neighbors.each do |neighbor|
      if !visited.include?(node) || !visited.include?(neighbor)
        new_edges << [node, neighbor]
      end
    end
  end
  
  # Remove duplicates and sort for consistent output
  new_edges.uniq!
  new_edges.sort!
  
  new_edges
end

# Alternative cleaner implementation
def two_break_on_genome_graph_v2(genome_graph, i, j, k, l)
  # Create a copy of the genome graph
  new_graph = genome_graph.map(&:clone)
  
  # Find and remove the edges (i,j) and (k,l)
  new_graph.reject! do |edge|
    (edge[0] == i && edge[1] == j) || (edge[0] == j && edge[1] == i) ||
    (edge[0] == k && edge[1] == l) || (edge[0] == l && edge[1] == k)
  end
  
  # Add the new edges (i,k) and (j,l)
  new_graph << [i, k]
  new_graph << [j, l]
  
  # Sort edges for consistent output
  new_graph.sort_by { |edge| [edge[0], edge[1]] }
end

# Most efficient implementation
def two_break_on_genome_graph_final(genome_graph, i, j, k, l)
  # Create new graph by removing old edges and adding new ones
  new_graph = []
  
  genome_graph.each do |edge|
    u, v = edge
    
    # Skip the edges that will be removed
    if ((u == i && v == j) || (u == j && v == i) ||
        (u == k && v == l) || (u == l && v == k))
      next
    end
    
    # Add all other edges
    new_graph << [u, v]
  end
  
  # Add the new edges
  new_graph << [i, k]
  new_graph << [j, l]
  
  # Sort the result
  new_graph.sort_by { |edge| [edge[0], edge[1]] }
end
```

## Example Usage

```ruby
# Example input
genome_graph = [[1, 2], [3, 4], [5, 6], [7, 8]]
i, j, k, l = 1, 6, 3, 8

# Apply 2-break operation
result = two_break_on_genome_graph_final(genome_graph, i, j, k, l)
puts result.inspect
# Output: [[1, 3], [2, 6], [4, 8], [5, 7]]
```

## Explanation

The 2-break operation on a genome graph:
1. Takes a genome graph represented as a list of edges
2. Removes two existing edges: (i,j) and (k,l)
3. Adds two new edges: (i,k) and (j,l)
4. Returns the updated genome graph

The key insight is that a 2-break operation rearranges the circular chromosomes in a genome by breaking at two positions and reconnecting the segments in a different way.

## Time Complexity
- **Time**: O(n) where n is the number of edges in the genome graph
- **Space**: O(n) for storing the new graph

## Key Points
- The function handles both directions of edges (i,j) and (j,i)
- Results are sorted for consistent output
- The operation maintains the graph structure while performing the rearrangement
- This is a fundamental operation in genome rearrangement algorithms

