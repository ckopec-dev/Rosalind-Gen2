# Rosalind Problem: Calculating Protein Mass

## Problem Description
In mass spectrometry, the mass of a protein is calculated by summing the masses of its constituent amino acids. Given a protein string, we need to calculate its total mass.

## Solution in Ruby

```ruby
def calculate_protein_mass(protein_string)
  # Mass table for amino acids
  mass_table = {
    'A' => 71.03711,
    'C' => 103.00919,
    'D' => 115.02694,
    'E' => 129.04259,
    'F' => 147.06841,
    'G' => 57.02137,
    'H' => 137.05891,
    'I' => 113.08406,
    'K' => 128.09496,
    'L' => 113.08406,
    'M' => 131.04049,
    'N' => 114.04293,
    'P' => 97.05276,
    'Q' => 128.05858,
    'R' => 156.10111,
    'S' => 87.03203,
    'T' => 101.04768,
    'V' => 99.06841,
    'W' => 186.07931,
    'Y' => 163.06333
  }
  
  # Initialize total mass
  total_mass = 0.0
  
  # Sum up the mass of each amino acid
  protein_string.each_char do |amino_acid|
    total_mass += mass_table[amino_acid]
  end
  
  # Return the total mass rounded to 3 decimal places
  total_mass.round(3)
end

# Example usage:
# protein = "SKADYEK"
# puts calculate_protein_mass(protein)
```

## Alternative Implementation (More Concise)

```ruby
def calculate_protein_mass(protein_string)
  mass_table = {
    'A' => 71.03711, 'C' => 103.00919, 'D' => 115.02694, 'E' => 129.04259,
    'F' => 147.06841, 'G' => 57.02137, 'H' => 137.05891, 'I' => 113.08406,
    'K' => 128.09496, 'L' => 113.08406, 'M' => 131.04049, 'N' => 114.04293,
    'P' => 97.05276, 'Q' => 128.05858, 'R' => 156.10111, 'S' => 87.03203,
    'T' => 101.04768, 'V' => 99.06841, 'W' => 186.07931, 'Y' => 163.06333
  }
  
  protein_string.chars.map { |aa| mass_table[aa] }.sum.round(3)
end
```

## How to Use

1. Define your protein string
2. Call the `calculate_protein_mass` function with the protein string as argument
3. The function returns the total mass rounded to 3 decimal places

## Example

For the protein string "SKADYEK":
- S = 87.032
- K = 128.095
- A = 71.037
- D = 115.027
- Y = 163.063
- E = 129.043
- K = 128.095

Total mass = 791.392

The solution handles the standard amino acid masses and returns the result with appropriate precision for mass spectrometry calculations.

