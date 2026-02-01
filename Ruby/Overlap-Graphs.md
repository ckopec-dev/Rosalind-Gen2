# Rosalind Problem: Overlap Graphs (Overlap_Graphs)

## Problem Description
Given a collection of DNA strings in FASTA format, construct a directed graph where each node represents a DNA string and there is a directed edge from string A to string B if the suffix of A (with length k) matches the prefix of B.

## Solution

```ruby
def overlap_graphs(fasta_data, k = 3)
  # Parse FASTA data into hash of names and sequences
  sequences = {}
  current_name = nil
  current_seq = ""
  
  fasta_data.each_line do |line|
    if line.start_with?('>'))
      if current_name
        sequences[current_name] = current_seq
      end
      current_name = line.strip[1..-1]
      current_seq = ""
    else
      current_seq += line.strip
    end
  end
  
  # Don't forget the last sequence
  if current_name
    sequences[current_name] = current_seq
  end
  
  # Find overlaps
  overlaps = []
  
  sequences.each do |name1, seq1|
    sequences.each do |name2, seq2|
      # Skip self-edges
      next if name1 == name2
      
      # Check if suffix of seq1 matches prefix of seq2
      if seq1[-k..-1] == seq2[0...k]
        overlaps << [name1, name2]
      end
    end
  end
  
  overlaps
end

# Alternative more efficient approach using hash for lookup
def overlap_graphs_optimized(fasta_data, k = 3)
  # Parse FASTA data
  sequences = {}
  current_name = nil
  current_seq = ""
  
  fasta_data.each_line do |line|
    if line.start_with?('>'))
      if current_name
        sequences[current_name] = current_seq
      end
      current_name = line.strip[1..-1]
      current_seq = ""
    else
      current_seq += line.strip
    end
  end
  
  if current_name
    sequences[current_name] = current_seq
  end
  
  # Create suffix -> names mapping for efficient lookup
  suffix_map = {}
  sequences.each do |name, seq|
    suffix = seq[-k..-1]
    suffix_map[suffix] ||= []
    suffix_map[suffix] << name
  end
  
  # Find overlaps
  overlaps = []
  
  sequences.each do |name1, seq1|
    prefix = seq1[0...k]
    if suffix_map.key?(prefix)
      suffix_map[prefix].each do |name2|
        # Skip self-edges
        next if name1 == name2
        overlaps << [name1, name2]
      end
    end
  end
  
  overlaps
end

# Example usage:
# Sample FASTA data
fasta_input = ">Rosalind_0498
AAATAAA
>Rosalind_2391
AAATTTT
>Rosalind_2323
TTTTTTT"

# Get overlaps with k=3
result = overlap_graphs_optimized(fasta_input, 3)

# Print results
result.each do |from, to|
  puts "#{from} #{to}"
end
```

## Explanation

1. **FASTA Parsing**: The function first parses the FASTA format data, extracting sequence names and their corresponding DNA sequences.

2. **Overlap Detection**: For each pair of sequences, we check if the suffix of the first sequence (last k characters) matches the prefix of the second sequence (first k characters).

3. **Optimization**: The optimized version uses a hash map to store suffixes and their corresponding sequence names, reducing the time complexity from O(n²) to O(n) for the lookup phase.

4. **Output**: The function returns pairs of sequence names where an overlap exists, representing the directed edges in the overlap graph.

## Time Complexity
- **Basic version**: O(n² × k) where n is the number of sequences and k is the overlap length
- **Optimized version**: O(n × k) for the lookup phase plus O(n² × k) for suffix/prefix comparison

## Space Complexity
- O(n × k) for storing the sequences and suffix mappings

## Sample Output
For the example input, if sequences have overlaps, the output would be:
```
Rosalind_0498 Rosalind_2391
```

This indicates that sequence Rosalind_0498 has an overlap with Rosalind_2391 where the suffix of the first matches the prefix of the second.

