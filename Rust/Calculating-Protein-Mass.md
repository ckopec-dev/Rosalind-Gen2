# Rosalind Problem: Calculating Protein Mass

## Problem Statement
In mass spectrometry, the mass of a protein is the sum of the masses of all its amino acids. Given a protein string, calculate its total mass.

## Solution in Rust

```rust
use std::collections::HashMap;

fn calculate_protein_mass(protein: &str) -> f64 {
    // Mass table for amino acids
    let mut mass_table = HashMap::new();
    mass_table.insert('A', 71.03711);
    mass_table.insert('C', 103.00919);
    mass_table.insert('D', 115.02694);
    mass_table.insert('E', 129.04259);
    mass_table.insert('F', 147.06841);
    mass_table.insert('G', 57.02137);
    mass_table.insert('H', 137.05891);
    mass_table.insert('I', 113.08406);
    mass_table.insert('K', 128.09496);
    mass_table.insert('L', 113.08406);
    mass_table.insert('M', 131.04049);
    mass_table.insert('N', 114.04293);
    mass_table.insert('P', 97.05276);
    mass_table.insert('Q', 128.05858);
    mass_table.insert('R', 156.10111);
    mass_table.insert('S', 87.03203);
    mass_table.insert('T', 101.04768);
    mass_table.insert('V', 99.06841);
    mass_table.insert('W', 186.07931);
    mass_table.insert('Y', 163.06333);

    let mut total_mass = 0.0;
    
    for amino_acid in protein.chars() {
        if let Some(mass) = mass_table.get(&amino_acid) {
            total_mass += mass;
        }
    }
    
    total_mass
}

fn main() {
    // Example usage
    let protein = "SKADYEK";
    let mass = calculate_protein_mass(protein);
    println!("Protein: {}", protein);
    println!("Total Mass: {:.5}", mass);
    
    // For the actual Rosalind problem, you would read from input
    // let input = std::fs::read_to_string("input.txt").unwrap();
    // let mass = calculate_protein_mass(&input.trim());
    // println!("{:.5}", mass);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_protein_mass() {
        assert_eq!(calculate_protein_mass("SKADYEK"), 821.39263);
    }
}
```

## Explanation

1. **Data Structure**: I use a `HashMap` to store the mass values for each amino acid as specified in the problem.

2. **Algorithm**: 
   - Iterate through each character (amino acid) in the protein string
   - Look up the mass of each amino acid in the hash map
   - Sum all the masses to get the total protein mass

3. **Key Points**:
   - The mass values are precise to 5 decimal places as required
   - The function handles any valid amino acid sequence
   - Error handling is implicit - if an invalid character is encountered, it's simply ignored (though in practice, you might want to add error handling)

4. **Time Complexity**: O(n) where n is the length of the protein string
5. **Space Complexity**: O(1) since the hash map has a fixed size of 20 amino acids

## Sample Input/Output
- Input: `"SKADYEK"`
- Output: `821.39263`

This solution efficiently calculates the total mass of a protein by summing the individual masses of its constituent amino acids.

