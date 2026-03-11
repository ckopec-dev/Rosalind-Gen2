# Rosalind Problem: Convert a Peptide into a Peptide Vector

## Problem Description
Given a peptide, we need to convert it into a peptide vector. The peptide vector is a binary vector of length 3n (where n is the length of the peptide) where each amino acid contributes 3 positions in the vector. For each amino acid, we set the corresponding 3 positions to 1.

## Solution

```ruby
def peptide_to_vector(peptide)
  # Create a mapping from amino acids to their positions in the vector
  # Each amino acid contributes 3 positions (3 nucleotides)
  vector_length = peptide.length * 3
  peptide_vector = Array.new(vector_length, 0)
  
  # Define the amino acid to position mapping
  amino_acids = {
    'A' => 0, 'C' => 1, 'D' => 2, 'E' => 3, 'F' => 4, 'G' => 5,
    'H' => 6, 'I' => 7, 'K' => 8, 'L' => 9, 'M' => 10, 'N' => 11,
    'P' => 12, 'Q' => 13, 'R' => 14, 'S' => 15, 'T' => 16, 'V' => 17,
    'W' => 18, 'Y' => 19
  }
  
  # Process each amino acid in the peptide
  peptide.each_char.with_index do |amino_acid, index|
    # Get the position of this amino acid
    pos = amino_acids[amino_acid]
    
    # Set the 3 positions in the vector
    # Each amino acid occupies 3 consecutive positions
    start_pos = index * 3
    peptide_vector[start_pos] = 1
    peptide_vector[start_pos + 1] = 1
    peptide_vector[start_pos + 2] = 1
  end
  
  peptide_vector
end

# Alternative approach using a more direct mapping
def peptide_to_vector_v2(peptide)
  # Create a mapping from amino acids to their 3-bit representation
  amino_to_bits = {
    'A' => 0, 'C' => 1, 'D' => 2, 'E' => 3, 'F' => 4, 'G' => 5,
    'H' => 6, 'I' => 7, 'K' => 8, 'L' => 9, 'M' => 10, 'N' => 11,
    'P' => 12, 'Q' => 13, 'R' => 14, 'S' => 15, 'T' => 16, 'V' => 17,
    'W' => 18, 'Y' => 19
  }
  
  # Initialize vector
  vector_length = peptide.length * 3
  result = Array.new(vector_length, 0)
  
  # For each amino acid, set 3 consecutive positions to 1
  peptide.each_char.with_index do |aa, i|
    start_pos = i * 3
    result[start_pos] = 1
    result[start_pos + 1] = 1
    result[start_pos + 2] = 1
  end
  
  result
end

# Test the function
def test_peptide_to_vector
  # Test case 1
  peptide1 = "LEQN"
  result1 = peptide_to_vector(peptide1)
  puts "Peptide: #{peptide1}"
  puts "Vector: #{result1.join(' ')}"
  puts
  
  # Test case 2
  peptide2 = "NQ"
  result2 = peptide_to_vector(peptide2)
  puts "Peptide: #{peptide2}"
  puts "Vector: #{result2.join(' ')}"
  puts
  
  # Test case 3 - single amino acid
  peptide3 = "A"
  result3 = peptide_to_vector(peptide3)
  puts "Peptide: #{peptide3}"
  puts "Vector: #{result3.join(' ')}"
end

# Run tests
test_peptide_to_vector
```

## Explanation

The solution works as follows:

1. **Initialize the vector**: Create a binary vector of length 3n (where n is the peptide length) filled with zeros.

2. **Process each amino acid**: For each amino acid in the peptide:
   - Calculate the starting position in the vector (3 positions per amino acid)
   - Set the 3 consecutive positions to 1

3. **Return the vector**: The resulting binary vector represents the peptide.

## Example Walkthrough

For peptide "LEQN":
- L (position 9) → positions 0, 1, 2
- E (position 3) → positions 3, 4, 5  
- Q (position 13) → positions 6, 7, 8
- N (position 11) → positions 9, 10, 11

Result: [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the peptide
- **Space Complexity**: O(3n) = O(n) for the output vector

The solution efficiently converts any peptide string into its corresponding peptide vector representation.

