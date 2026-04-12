# Rosalind Problem: Creating a Distance Matrix (Creating_a_Distance_Matrix)

## Problem Description
Given: A collection of n (n≤100) DNA strings s1,…,sn of equal length (at most 1000 bp). Return: The matrix D corresponding to the p-distance dp between the strings.

The p-distance is the proportion of corresponding symbols that differ between two strings.

## Solution

```ruby
def read_fasta(file)
  sequences = {}
  current_id = nil
  current_seq = ""
  
  File.readlines(file).each do |line|
    line = line.strip
    if line.start_with?(">") 
      if current_id
        sequences[current_id] = current_seq
      end
      current_id = line[1..-1]
      current_seq = ""
    else
      current_seq += line
    end
  end
  
  if current_id
    sequences[current_id] = current_seq
  end
  
  sequences
end

def calculate_p_distance(seq1, seq2)
  if seq1.length != seq2.length
    raise "Sequences must be of equal length"
  end
  
  differences = 0
  seq1.length.times do |i|
    differences += 1 if seq1[i] != seq2[i]
  end
  
  differences.to_f / seq1.length
end

def create_distance_matrix(sequences)
  n = sequences.length
  matrix = Array.new(n) { Array.new(n, 0.0) }
  
  sequences.keys.each_with_index do |id1, i|
    sequences.keys.each_with_index do |id2, j|
      if i == j
        matrix[i][j] = 0.0
      else
        matrix[i][j] = calculate_p_distance(sequences[id1], sequences[id2])
      end
    end
  end
  
  matrix
end

def print_distance_matrix(matrix)
  matrix.each do |row|
    puts row.map { |val| sprintf("%.5f", val) }.join(" ")
  end
end

# Main execution
if __FILE__ == $0
  # Read input file
  if ARGV.length < 1
    puts "Usage: ruby creating_a_distance_matrix.rb <input_file>"
    exit 1
  end
  
  input_file = ARGV[0]
  
  # Read sequences
  sequences = read_fasta(input_file)
  
  # Create and print distance matrix
  distance_matrix = create_distance_matrix(sequences)
  print_distance_matrix(distance_matrix)
end
```

## Alternative Solution (More Concise)

```ruby
def p_distance(seq1, seq2)
  return 0.0 if seq1 == seq2
  differences = seq1.chars.zip(seq2.chars).count { |a, b| a != b }
  differences.to_f / seq1.length
end

def distance_matrix(sequences)
  n = sequences.length
  matrix = Array.new(n) { Array.new(n, 0.0) }
  
  (0...n).each do |i|
    (0...n).each do |j|
      matrix[i][j] = i == j ? 0.0 : p_distance(sequences[i], sequences[j])
    end
  end
  
  matrix
end

# Read input and solve
sequences = []
File.readlines(ARGV[0]).each do |line|
  if line.start_with?(">") 
    sequences << ""
  else
    sequences[-1] += line.strip
  end
end

distance_matrix(sequences).each do |row|
  puts row.map { |d| sprintf("%.5f", d) }.join(" ")
end
```

## Example Usage

If you have an input file `rosalind_pdst.txt`:
```
>Rosalind_0203
AAATTT
>Rosalind_0204
AAATTT
>Rosalind_0205
AAATTT
```

Running:
```bash
ruby creating_a_distance_matrix.rb rosalind_pdst.txt
```

Would output:
```
0.00000 0.00000 0.00000
0.00000 0.00000 0.00000
0.00000 0.00000 0.00000
```

## Key Points

1. **p-distance calculation**: Count differing positions and divide by total length
2. **Matrix format**: Output as space-separated values with 5 decimal places
3. **Fasta parsing**: Handle FASTA format input with headers
4. **Edge cases**: Handle identical sequences (distance = 0)
5. **Precision**: Format output to 5 decimal places as required

The solution handles the core requirements of creating a distance matrix from DNA sequences using the p-distance metric.

