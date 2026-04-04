# Rosalind Problem: Genome Assembly with Perfect Coverage

## Problem Understanding

In genome assembly with perfect coverage, we need to reconstruct a DNA sequence from its k-mers (substrings of length k). When we have perfect coverage, every possible k-mer of the original sequence appears exactly once in our collection.

## Approach

1. **De Bruijn Graph Construction**: Build a graph where nodes are (k-1)-mers and edges represent k-mers
2. **Eulerian Path**: Find an Eulerian path in the graph (since each node has equal in-degree and out-degree)
3. **Sequence Reconstruction**: Traverse the path and reconstruct the original sequence

## Solution

```ruby
def genome_assembly_with_perfect_coverage(kmers)
  # Build De Bruijn graph
  graph = {}
  
  # For each k-mer, create an edge from (k-1)-mer prefix to (k-1)-mer suffix
  kmers.each do |kmer|
    prefix = kmer[0...kmer.length-1]
    suffix = kmer[1..kmer.length-1]
    
    graph[prefix] ||= []
    graph[prefix] << suffix
  end
  
  # Find Eulerian path
  start_node = find_start_node(graph)
  path = eulerian_path(graph, start_node)
  
  # Reconstruct sequence
  return reconstruct_sequence(path)
end

def find_start_node(graph)
  # Find node with out-degree - in-degree = 1
  in_degrees = {}
  out_degrees = {}
  
  # Initialize all nodes
  graph.each_key do |node|
    in_degrees[node] = 0
    out_degrees[node] = 0
  end
  
  # Calculate in-degrees and out-degrees
  graph.each do |node, neighbors|
    out_degrees[node] = neighbors.length
    neighbors.each do |neighbor|
      in_degrees[neighbor] = in_degrees[neighbor] + 1
    end
  end
  
  # Find start node (out-degree - in-degree = 1)
  start_node = nil
  graph.each do |node, _|
    if out_degrees[node] - in_degrees[node] == 1
      start_node = node
      break
    end
  end
  
  # If no start node found, use any node
  start_node ||= graph.keys.first
end

def eulerian_path(graph, start_node)
  # Hierholzer's algorithm for Eulerian path
  stack = [start_node]
  path = []
  
  while !stack.empty?
    current = stack.last
    
    if graph[current] && !graph[current].empty?
      # Get next node and remove edge
      next_node = graph[current].pop
      stack << next_node
    else
      # Backtrack
      path << stack.pop
    end
  end
  
  # Path is constructed backwards
  path.reverse
end

def reconstruct_sequence(path)
  # First node contributes all characters
  # Subsequent nodes contribute only the last character
  if path.empty?
    return ""
  end
  
  sequence = path[0]
  (1...path.length).each do |i|
    sequence += path[i][-1]
  end
  
  sequence
end

# Alternative simpler approach for perfect coverage case
def genome_assembly_perfect_coverage(kmers)
  # For perfect coverage, we can directly construct the sequence
  # by connecting k-mers through overlapping
  return "" if kmers.empty?
  
  # Build the De Bruijn graph
  graph = {}
  kmers.each do |kmer|
    prefix = kmer[0...kmer.length-1]
    suffix = kmer[1..kmer.length-1]
    
    graph[prefix] ||= []
    graph[prefix] << suffix
  end
  
  # Find Eulerian path
  start_node = kmers.first[0...kmers.first.length-1]
  path = eulerian_path_simple(graph, start_node)
  
  # Reconstruct sequence
  return reconstruct_sequence(path)
end

def eulerian_path_simple(graph, start_node)
  # Simple implementation for perfect coverage case
  stack = [start_node]
  path = []
  
  while !stack.empty?
    current = stack.last
    
    if graph[current] && !graph[current].empty?
      next_node = graph[current].pop
      stack << next_node
    else
      path << stack.pop
    end
  end
  
  path.reverse
end

# Main solution function
def solve_genome_assembly(kmers)
  # Read input kmers (in real Rosalind problem, these would be from file)
  # For demonstration, assuming kmers is an array of strings
  
  genome_assembly_perfect_coverage(kmers)
end

# Example usage:
# kmers = ["ATTAC", "TACGA", "ACGAT", "CGATT", "GATTA", "ATTAC"]
# result = solve_genome_assembly(kmers)
# puts result
```

## Explanation

The solution works as follows:

1. **Graph Construction**: For each k-mer, we create a directed edge from its prefix (k-1 characters) to its suffix (k-1 characters)

2. **Eulerian Path Finding**: We use Hierholzer's algorithm to find an Eulerian path through the De Bruijn graph

3. **Sequence Reconstruction**: We reconstruct the original sequence by taking the first k-mer and then appending the last character of each subsequent k-mer in the path

## Time Complexity
- **Time**: O(|E| + |V|) where E is the number of edges and V is the number of vertices
- **Space**: O(|V| + |E|) for storing the graph

## Key Insight for Perfect Coverage
In perfect coverage, every k-mer appears exactly once, and the De Bruijn graph will have an Eulerian path that visits every edge exactly once, allowing us to reconstruct the original sequence perfectly.

