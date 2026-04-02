# Rosalind Problem: Implement_RandomizedMotifSearch

## Problem Description
Implement the randomized motif search algorithm to find the best motifs in a set of DNA sequences.

## Solution

```ruby
def randomized_motif_search(dna_sequences, k, t)
  # Initialize random motifs
  motifs = []
  dna_sequences.each do |sequence|
    start = rand(sequence.length - k + 1)
    motifs << sequence[start, k]
  end
  
  best_motifs = motifs.dup
  
  loop do
    # Create profile from current motifs
    profile = create_profile(motifs, k, t)
    
    # Generate new motifs based on profile
    new_motifs = []
    dna_sequences.each do |sequence|
      new_motifs << profile_most_probable_kmer(sequence, k, profile)
    end
    
    # Check if new motifs are better
    if score(new_motifs) < score(best_motifs)
      best_motifs = new_motifs.dup
    else
      return best_motifs
    end
    
    motifs = new_motifs
  end
end

def create_profile(motifs, k, t)
  # Initialize profile matrix with pseudocounts
  profile = Array.new(4) { Array.new(k, 1.0) }
  
  # Count nucleotides at each position
  motifs.each do |motif|
    motif.chars.each_with_index do |nucleotide, i|
      case nucleotide
      when 'A'
        profile[0][i] += 1
      when 'C'
        profile[1][i] += 1
      when 'G'
        profile[2][i] += 1
      when 'T'
        profile[3][i] += 1
      end
    end
  end
  
  # Convert counts to probabilities
  total = t + 4  # Add pseudocounts
  profile.each do |row|
    row.each_with_index do |count, i|
      row[i] = count / total
    end
  end
  
  profile
end

def profile_most_probable_kmer(sequence, k, profile)
  max_probability = -1.0
  best_kmer = sequence[0, k]
  
  (sequence.length - k + 1).times do |i|
    kmer = sequence[i, k]
    probability = 1.0
    
    kmer.chars.each_with_index do |nucleotide, j|
      case nucleotide
      when 'A'
        probability *= profile[0][j]
      when 'C'
        probability *= profile[1][j]
      when 'G'
        probability *= profile[2][j]
      when 'T'
        probability *= profile[3][j]
      end
    end
    
    if probability > max_probability
      max_probability = probability
      best_kmer = kmer
    end
  end
  
  best_kmer
end

def score(motifs)
  # Calculate score based on number of mismatches
  k = motifs[0].length
  score = 0
  
  (0...k).each do |i|
    nucleotide_count = Hash.new(0)
    motifs.each do |motif|
      nucleotide_count[motif[i]] += 1
    end
    
    # Score is total motifs minus count of most frequent nucleotide
    score += motifs.length - nucleotide_count.values.max
  end
  
  score
end

# Read input from stdin
input = STDIN.read
lines = input.split("\n").reject(&:empty?)

# Parse input
k, t = lines[0].split.map(&:to_i)
dna_sequences = lines[1..-1]

# Run randomized motif search
result = randomized_motif_search(dna_sequences, k, t)

# Output result
puts result.join("\n")
```

## Explanation

This implementation follows the randomized motif search algorithm:

1. **Initialization**: Start with random k-mers from each DNA sequence
2. **Profile Creation**: Build a profile matrix from current motifs with pseudocounts
3. **Motif Generation**: For each sequence, find the most probable k-mer based on the profile
4. **Comparison**: If new motifs are better (lower score), update the best motifs
5. **Iteration**: Continue until no improvement is found

Key components:
- `create_profile`: Builds a probability profile matrix with pseudocounts
- `profile_most_probable_kmer`: Finds the k-mer with highest probability for a given profile
- `score`: Calculates the motif score (number of mismatches from consensus)
- Main algorithm: Repeatedly improves motifs until convergence

The algorithm uses pseudocounts to avoid zero probabilities and handles the random initialization properly.

