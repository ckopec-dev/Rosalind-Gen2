# Rosalind Problem: Construct the De Bruijn Graph of a String

## Problem Description
Given a string Text, construct the de Bruijn graph of order k (k-mer composition) of Text.

## Solution

```ruby
def de_bruijn_graph(text, k)
  # Create a hash to store edges
  edges = {}
  
  # Generate all k-mers from the text
  (0..text.length - k).each do |i|
    kmer = text[i, k]
    
    # Extract prefix (k-1) and suffix (k-1) of the k-mer
    prefix = kmer[0, k-1]
    suffix = kmer[1, k-1]
    
    # Add edge from prefix to suffix
    if edges.key?(prefix)
      edges[prefix] << suffix
    else
      edges[prefix] = [suffix]
    end
  end
  
  # Sort suffixes for consistent output
  edges.each do |prefix, suffixes|
    edges[prefix] = suffixes.sort
  end
  
  # Format output
  result = []
  edges.sort.each do |prefix, suffixes|
    suffixes.each do |suffix|
      result << "#{prefix} -> #{suffix}"
    end
  end
  
  result
end

# Alternative more concise implementation
def de_bruijn_graph_concise(text, k)
  edges = {}
  
  # Generate all k-mers and build edges
  (0..text.length - k).each do |i|
    prefix = text[i, k-1]
    suffix = text[i+1, k-1]
    
    edges[prefix] ||= []
    edges[prefix] << suffix
  end
  
  # Format and sort output
  edges.sort.map do |prefix, suffixes|
    suffixes.sort.map { |suffix| "#{prefix} -> #{suffix}" }
  end.flatten
end

# Example usage:
# text = "AAGATTCTCTAC"
# k = 4
# result = de_bruijn_graph(text, k)
# puts result.join("\n")

# Read input from file or stdin
def solve_de_bruijn
  input = gets.chomp
  k = gets.chomp.to_i
  
  result = de_bruijn_graph(input, k)
  puts result.join("\n")
end

# Test with example
if __FILE__ == $0
  # Example from Rosalind
  text = "AAGATTCTCTAC"
  k = 4
  
  puts "Input text: #{text}"
  puts "k-mer size: #{k}"
  puts "De Bruijn graph:"
  
  result = de_bruijn_graph(text, k)
  result.each { |line| puts line }
end
```

## Explanation

The algorithm works as follows:

1. **Extract k-mers**: For each position in the text, extract a k-mer of length k
2. **Create edges**: For each k-mer, create an edge from its prefix (k-1 characters) to its suffix (k-1 characters)
3. **Build adjacency list**: Store these edges in a hash where keys are prefixes and values are arrays of suffixes
4. **Format output**: Sort the suffixes and format as "prefix -> suffix" lines

## Time and Space Complexity

- **Time Complexity**: O(n × k) where n is the length of the text and k is the k-mer size
- **Space Complexity**: O(n × k) for storing the edges

## Example

For input text "AAGATTCTCTAC" and k=4:
- K-mers: AAGA, AGAT, GATT, ATTC, TTCT, TCTC, CTCT, TCTA, CTAC
- Edges: 
  - AAG → AGA
  - AGA → GAT  
  - GAT → ATT
  - ATT → TTC
  - TTC → TCT
  - TCT → CTC
  - CTC → TCA
  - CTAC → TAC

The output will be formatted as:
```
AAG -> AGA
AGA -> GAT
ATT -> TTC
CTC -> TCA
GAT -> ATT
TCT -> CTC
TTC -> TCT
```

