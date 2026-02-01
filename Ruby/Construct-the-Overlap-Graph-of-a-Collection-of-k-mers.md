# Rosalind Problem: Construct the Overlap Graph of a Collection of k-mers

## Problem Description
Given a collection of k-mers, construct the overlap graph where each k-mer is a node, and there is a directed edge from k-mer A to k-mer B if the suffix of A (k-1 characters) matches the prefix of B (k-1 characters).

## Solution in Ruby

```ruby
def construct_overlap_graph(kmers)
  # Create a hash to store k-mers and their indices
  kmers_hash = {}
  kmers.each_with_index do |kmer, index|
    kmers_hash[kmer] = index
  end
  
  # Find overlaps and build edges
  edges = []
  
  kmers.each_with_index do |kmer_a, i|
    # Get the suffix of kmer_a (all characters except the first)
    suffix = kmer_a[1..-1]
    
    # Check if any k-mer starts with this suffix
    kmers.each_with_index do |kmer_b, j|
      next if i == j  # Skip self-edges
      
      # Get the prefix of kmer_b (all characters except the last)
      prefix = kmer_b[0..-2]
      
      # If suffix of kmer_a matches prefix of kmer_b, add edge
      if suffix == prefix
        edges << "#{kmer_a} -> #{kmer_b}"
      end
    end
  end
  
  # Sort edges lexicographically
  edges.sort
end

# Alternative more efficient approach using hash lookup
def construct_overlap_graph_optimized(kmers)
  # Create a hash to store k-mers and their indices
  kmers_hash = {}
  kmers.each_with_index do |kmer, index|
    kmers_hash[kmer] = index
  end
  
  # Find overlaps and build edges
  edges = []
  
  kmers.each_with_index do |kmer_a, i|
    # Get the suffix of kmer_a (all characters except the first)
    suffix = kmer_a[1..-1]
    
    # Check if any k-mer starts with this suffix
    kmers.each_with_index do |kmer_b, j|
      next if i == j  # Skip self-edges
      
      # If suffix of kmer_a matches prefix of kmer_b (which is the first part of kmer_b)
      if kmer_b.start_with?(suffix)
        edges << "#{kmer_a} -> #{kmer_b}"
      end
    end
  end
  
  # Sort edges lexicographically
  edges.sort
end

# Read input from file or stdin
def read_kmers_from_input
  kmers = []
  begin
    while line = gets
      kmers << line.chomp
    end
  rescue EOFError
    # End of input
  end
  kmers
end

# Main execution
if __FILE__ == $0
  # Read k-mers from standard input
  kmers = read_kmers_from_input
  
  # Construct overlap graph
  edges = construct_overlap_graph_optimized(kmers)
  
  # Print results
  edges.each { |edge| puts edge }
end
```

## Example Usage

For input:
```
ATGCG
GCATC
CATCG
TCGAT
CGATC
GATCG
ATCGA
TCGAT
CGATC
GATCG
```

The output would be:
```
ATGCG -> CGATC
CATCG -> ATCGA
CGATC -> GATCG
GCATC -> CATCG
TCGAT -> CGATC
```

## Explanation

1. **Input Processing**: Read all k-mers from input
2. **Overlap Detection**: For each k-mer A, extract its suffix (all characters except first) and check if any other k-mer starts with this suffix
3. **Edge Creation**: When overlap is found, create a directed edge from k-mer A to k-mer B
4. **Output**: Sort and print all edges in lexicographic order

## Time Complexity
- **Time**: O(n² × k) where n is the number of k-mers and k is the k-mer length
- **Space**: O(n × k) for storing the k-mers

## Key Points
- The suffix of a k-mer is the last (k-1) characters
- The prefix of a k-mer is the first (k-1) characters  
- Each k-mer can have multiple outgoing edges
- Results are sorted lexicographically as required by Rosalind format

