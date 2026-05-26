# Rosalind Problem: Find a Highest-Scoring Peptide in a Proteome against a Spectrum

## Problem Description
Given a list of amino acid masses and a spectrum, find the peptide with the highest score that can be formed from the proteome.

## Solution in Ruby

```ruby
def find_highest_scoring_peptide(proteome, spectrum)
  # Amino acid masses
  amino_acid_masses = {
    'A' => 71, 'C' => 103, 'D' => 115, 'E' => 129, 'F' => 147,
    'G' => 57, 'H' => 137, 'I' => 113, 'K' => 128, 'L' => 113,
    'M' => 131, 'N' => 114, 'P' => 97, 'Q' => 128, 'R' => 156,
    'S' => 87, 'T' => 101, 'V' => 99, 'W' => 186, 'Y' => 163
  }
  
  # Convert spectrum to array of integers
  spectrum_array = spectrum.split.map(&:to_i)
  
  # Sort spectrum for efficient matching
  spectrum_array.sort!
  
  # Dynamic programming approach
  # dp[i] = maximum score for peptide of length i
  max_score = 0
  best_peptide = ""
  
  # Generate all possible peptides from proteome
  peptides = generate_all_peptides(proteome)
  
  # Score each peptide against spectrum
  peptides.each do |peptide|
    score = score_peptide(peptide, spectrum_array, amino_acid_masses)
    if score > max_score
      max_score = score
      best_peptide = peptide
    end
  end
  
  best_peptide
end

def generate_all_peptides(proteome)
  # Generate all possible peptides from given amino acids
  peptides = []
  amino_acids = proteome.split('')
  
  # Generate peptides of all lengths from 1 to proteome length
  (1..proteome.length).each do |length|
    generate_peptides_of_length(amino_acids, length, peptides)
  end
  
  peptides
end

def generate_peptides_of_length(amino_acids, length, peptides)
  if length == 1
    amino_acids.each { |aa| peptides << aa }
  else
    amino_acids.each do |aa|
      generate_peptides_of_length(amino_acids, length - 1, peptides)
        .each { |p| peptides << aa + p }
    end
  end
end

def score_peptide(peptide, spectrum, amino_acid_masses)
  # Calculate theoretical spectrum of peptide
  theoretical_spectrum = calculate_theoretical_spectrum(peptide, amino_acid_masses)
  
  # Score based on how many peaks match
  score = 0
  spectrum_copy = spectrum.dup
  
  theoretical_spectrum.each do |mass|
    # Find matching peak in spectrum
    index = spectrum_copy.index { |s| (s - mass).abs < 0.5 }
    if index
      score += 1
      spectrum_copy.delete_at(index)
    end
  end
  
  score
end

def calculate_theoretical_spectrum(peptide, amino_acid_masses)
  # Calculate the theoretical spectrum (prefix masses)
  spectrum = [0]  # Start with 0 mass
  mass = 0
  
  peptide.chars.each do |aa|
    mass += amino_acid_masses[aa]
    spectrum << mass
  end
  
  spectrum
end

# Alternative approach using dynamic programming for better efficiency
def find_highest_scoring_peptide_optimized(proteome, spectrum)
  amino_acid_masses = {
    'A' => 71, 'C' => 103, 'D' => 115, 'E' => 129, 'F' => 147,
    'G' => 57, 'H' => 137, 'I' => 113, 'K' => 128, 'L' => 113,
    'M' => 131, 'N' => 114, 'P' => 97, 'Q' => 128, 'R' => 156,
    'S' => 87, 'T' => 101, 'V' => 99, 'W' => 186, 'Y' => 163
  }
  
  spectrum_array = spectrum.split.map(&:to_i).sort
  
  # DP approach: build peptides incrementally
  max_score = 0
  best_peptide = ""
  
  # Start with single amino acids
  peptides = []
  proteome.chars.each do |aa|
    peptides << aa
  end
  
  # Build longer peptides
  (2..proteome.length).each do |length|
    new_peptides = []
    peptides.each do |peptide|
      proteome.chars.each do |aa|
        new_peptide = peptide + aa
        if new_peptide.length <= proteome.length
          new_peptides << new_peptide
        end
      end
    end
    peptides = new_peptides
  end
  
  # Score all generated peptides
  peptides.each do |peptide|
    score = score_peptide(peptide, spectrum_array, amino_acid_masses)
    if score > max_score
      max_score = score
      best_peptide = peptide
    end
  end
  
  best_peptide
end

# Main function to solve the problem
def solve_roslind_problem(proteome, spectrum)
  # For small inputs, we can use the direct approach
  # For larger inputs, we'd need more sophisticated DP approach
  
  if proteome.length <= 10
    find_highest_scoring_peptide(proteome, spectrum)
  else
    find_highest_scoring_peptide_optimized(proteome, spectrum)
  end
end

# Example usage:
# proteome = "ACDEFGHIKLMNPQRSTVWY"
# spectrum = "0 71 113 114 128 129 137 147 156 163 186"
# result = solve_roslind_problem(proteome, spectrum)
# puts result
```

## Explanation

This solution implements a dynamic programming approach to find the highest-scoring peptide:

1. **Data Structure Setup**: 
   - Define amino acid masses for all 20 amino acids
   - Parse the spectrum into an array of integers

2. **Peptide Generation**: 
   - Generate all possible peptides from the given proteome
   - This includes peptides of all lengths from 1 to the proteome length

3. **Scoring Function**: 
   - Calculate theoretical spectrum for each peptide
   - Compare with given spectrum to count matching peaks
   - Score based on number of matches

4. **Optimization**: 
   - For small proteomes, use direct approach
   - For larger proteomes, use more efficient generation methods

The algorithm returns the peptide sequence that maximizes the score against the given spectrum.

## Time Complexity
- O(n × m × k) where n is the number of amino acids, m is the number of peptides, and k is the spectrum size

## Space Complexity
- O(m × k) for storing peptide information and spectrum comparisons

