# Rosalind Problem: Implement LeaderboardCyclopeptideSequencing

## Problem Description
Implement the LeaderboardCyclopeptideSequencing algorithm to find the peptide with the highest score that can be formed from a given spectrum.

## Solution

```ruby
def leaderboard_cyclopeptide_sequencing(spectrum, n)
  # Initialize leaderboard with empty peptide
  leaderboard = [[]]
  leader_peptide = []
  leader_score = 0
  
  # While leaderboard is not empty
  while !leaderboard.empty?
    # Extend each peptide in leaderboard by adding each amino acid
    leaderboard = expand(leaderboard)
    
    # For each peptide in leaderboard
    leaderboard.each_with_index do |peptide, index|
      # If peptide has length equal to spectrum length
      if peptide.length == spectrum.length - 1
        # Calculate score
        score = cyclopeptide_score(peptide, spectrum)
        # If score is greater than leader score
        if score > leader_score
          leader_score = score
          leader_peptide = peptide
        end
        # Remove peptide from leaderboard
        leaderboard.delete_at(index)
      else
        # Calculate score
        score = cyclopeptide_score(peptide, spectrum)
        # If score is less than leader score, remove peptide
        if score < leader_score
          leaderboard.delete_at(index)
        end
      end
    end
    
    # Trim leaderboard to top N peptides
    leaderboard = trim(leaderboard, spectrum, n)
  end
  
  return leader_peptide
end

def expand(leaderboard)
  amino_acids = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]
  new_leaderboard = []
  
  leaderboard.each do |peptide|
    amino_acids.each do |aa|
      new_leaderboard << peptide + [aa]
    end
  end
  
  return new_leaderboard
end

def cyclopeptide_score(peptide, spectrum)
  # Generate theoretical spectrum
  theoretical_spectrum = generate_cyclopeptide_spectrum(peptide)
  
  # Calculate score
  score = 0
  spectrum_copy = spectrum.dup
  
  theoretical_spectrum.each do |mass|
    if spectrum_copy.include?(mass)
      spectrum_copy.delete_at(spectrum_copy.index(mass))
      score += 1
    end
  end
  
  return score
end

def generate_cyclopeptide_spectrum(peptide)
  # Generate all subpeptides
  subpeptides = []
  n = peptide.length
  
  # Add empty subpeptide
  subpeptides << []
  
  # Add all subpeptides of length 1 to n-1
  (1...n).each do |length|
    (0...n).each do |start|
      if start + length <= n
        subpeptides << peptide[start...(start + length)]
      else
        # Wrap around
        wrapped = peptide[start..-1] + peptide[0...(start + length - n)]
        subpeptides << wrapped
      end
    end
  end
  
  # Add full peptide
  subpeptides << peptide
  
  # Calculate masses for each subpeptide
  masses = []
  subpeptides.each do |subpep|
    mass = subpep.sum
    masses << mass
  end
  
  # Sort masses
  masses.sort!
  
  return masses
end

def trim(leaderboard, spectrum, n)
  # Calculate scores for all peptides
  scores = []
  leaderboard.each do |peptide|
    scores << cyclopeptide_score(peptide, spectrum)
  end
  
  # Sort peptides by scores in descending order
  sorted_pairs = leaderboard.zip(scores).sort_by { |peptide, score| -score }
  
  # Return top N peptides
  trimmed = sorted_pairs.first(n).map(&:first)
  
  return trimmed
end

# Alternative cleaner implementation
def leaderboard_cyclopeptide_sequencing_v2(spectrum, n)
  amino_acids = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]
  leaderboard = [[]]
  leader_peptide = []
  leader_score = 0
  
  while !leaderboard.empty?
    # Expand peptides
    leaderboard = leaderboard.flat_map { |peptide| amino_acids.map { |aa| peptide + [aa] } }
    
    # Score and filter peptides
    leaderboard_with_scores = leaderboard.map do |peptide|
      score = cyclopeptide_score(peptide, spectrum)
      [peptide, score]
    end
    
    # Update leader if needed
    leaderboard_with_scores.each do |peptide, score|
      if peptide.length == spectrum.length - 1
        if score > leader_score
          leader_score = score
          leader_peptide = peptide
        end
      end
    end
    
    # Filter peptides based on score
    leaderboard = leaderboard_with_scores.select { |peptide, score| score >= leader_score }.map(&:first)
    
    # Trim to top N
    leaderboard = trim(leaderboard, spectrum, n)
  end
  
  return leader_peptide
end

# Test with example
spectrum = [0, 113, 128, 186, 244, 299, 314, 372, 427, 442, 500]
n = 10

result = leaderboard_cyclopeptide_sequencing(spectrum, n)
puts "Result: #{result.join(' ')}"
```

## Explanation

This implementation solves the LeaderboardCyclopeptideSequencing problem by:

1. **Initialization**: Start with an empty peptide in the leaderboard
2. **Expansion**: For each peptide in the leaderboard, extend it by adding each amino acid
3. **Scoring**: Calculate the cyclopeptide score for each peptide
4. **Filtering**: Remove peptides with scores lower than the current leader
5. **Trimming**: Keep only the top N peptides in the leaderboard
6. **Termination**: Continue until all peptides in the leaderboard have the same length as the spectrum

## Key Functions

- `expand`: Adds all possible amino acids to each peptide in the leaderboard
- `cyclopeptide_score`: Calculates how well a peptide matches the spectrum
- `generate_cyclopeptide_spectrum`: Creates the theoretical spectrum for a given peptide
- `trim`: Keeps only the top N peptides based on their scores

## Time Complexity
O(N × M × L) where N is the number of peptides, M is the number of amino acids, and L is the length of the spectrum.

## Space Complexity
O(N × L) for storing the leaderboard and peptide information.

