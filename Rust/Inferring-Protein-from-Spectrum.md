# Rosalind Problem: Inferring Protein from Spectrum

## Problem Description
Given a list of peptide masses, we need to determine the corresponding amino acid sequence that would produce those masses.

## Approach
This is a classic dynamic programming problem where we need to find a sequence of amino acid masses that sum to the given spectrum masses.

## Solution

```rust
use std::collections::HashMap;

fn main() {
    // Read input from stdin
    let input = std::io::read_to_string(std::io::stdin()).unwrap();
    let lines: Vec<&str> = input.trim().split('\n').collect();
    
    // Parse the spectrum masses
    let spectrum: Vec<f64> = lines[0]
        .split_whitespace()
        .map(|s| s.parse::<f64>().unwrap())
        .collect();
    
    // Amino acid masses
    let amino_acid_masses = vec![
        ('A', 71.03711),
        ('C', 103.00919),
        ('D', 115.02694),
        ('E', 129.04259),
        ('F', 147.06841),
        ('G', 57.02146),
        ('H', 137.05891),
        ('I', 113.08406),
        ('K', 128.09496),
        ('L', 113.08406),
        ('M', 131.04049),
        ('N', 114.04293),
        ('P', 97.05276),
        ('Q', 128.05858),
        ('R', 156.10111),
        ('S', 87.03203),
        ('T', 101.04768),
        ('V', 99.06841),
        ('W', 186.07931),
        ('Y', 163.06333),
    ];
    
    // Create mass to amino acid mapping
    let mut mass_to_aa: HashMap<f64, char> = HashMap::new();
    for (aa, mass) in amino_acid_masses {
        mass_to_aa.insert(mass, aa);
    }
    
    // Find the protein sequence
    let result = find_protein_sequence(&spectrum, &mass_to_aa);
    println!("{}", result);
}

fn find_protein_sequence(spectrum: &[f64], mass_to_aa: &HashMap<f64, char>) -> String {
    // Sort spectrum in descending order
    let mut sorted_spectrum = spectrum.to_vec();
    sorted_spectrum.sort_by(|a, b| b.partial_cmp(a).unwrap());
    
    let mut sequence = String::new();
    let mut remaining_masses = sorted_spectrum.clone();
    
    // Remove the first element (should be the total mass)
    if let Some(total_mass) = remaining_masses.pop() {
        // Try to decompose the spectrum into amino acid masses
        let mut current_mass = total_mass;
        
        while current_mass > 0.001 && !remaining_masses.is_empty() {
            // Find the largest mass that can be subtracted
            let mut found = false;
            for &mass in &remaining_masses {
                if (current_mass - mass).abs() < 0.001 {
                    // Found a matching amino acid
                    if let Some(&aa) = mass_to_aa.get(&mass) {
                        sequence.push(aa);
                        current_mass -= mass;
                        // Remove the used mass from remaining
                        remaining_masses.retain(|&x| (x - mass).abs() > 0.001);
                        found = true;
                        break;
                    }
                }
            }
            
            if !found {
                // Try to find a mass that's close to current mass
                let mut best_mass = 0.0;
                let mut best_aa = ' ';
                
                for (&mass, &aa) in mass_to_aa {
                    if (current_mass - mass).abs() < (current_mass - best_mass).abs() {
                        best_mass = mass;
                        best_aa = aa;
                    }
                }
                
                if best_mass > 0.0 && (current_mass - best_mass).abs() < 0.001 {
                    sequence.push(best_aa);
                    current_mass -= best_mass;
                } else {
                    break;
                }
            }
        }
    }
    
    sequence
}

// Alternative approach using dynamic programming
fn find_protein_sequence_dp(spectrum: &[f64], mass_to_aa: &HashMap<f64, char>) -> String {
    // Create a set of available masses for quick lookup
    let mass_set: std::collections::HashSet<f64> = spectrum.iter().cloned().collect();
    
    // Find the total mass (largest value)
    let total_mass = *spectrum.iter().max_by(|a, b| a.partial_cmp(b).unwrap()).unwrap();
    
    // Reconstruct the sequence
    let mut sequence = String::new();
    let mut current_mass = total_mass;
    
    // Use a greedy approach to reconstruct
    while current_mass > 0.001 {
        let mut best_mass = 0.0;
        let mut best_aa = ' ';
        
        // Find the amino acid with mass closest to current_mass
        for (&mass, &aa) in mass_to_aa {
            if (current_mass - mass).abs() < (current_mass - best_mass).abs() {
                best_mass = mass;
                best_aa = aa;
            }
        }
        
        if best_mass > 0.0 && (current_mass - best_mass).abs() < 0.001 {
            sequence.push(best_aa);
            current_mass -= best_mass;
        } else {
            break;
        }
    }
    
    sequence
}

// More robust approach for the specific problem
fn solve_infer_protein_from_spectrum(spectrum: &[f64]) -> String {
    // Amino acid masses (with higher precision)
    let amino_acid_masses = [
        ('A', 71.03711),
        ('C', 103.00919),
        ('D', 115.02694),
        ('E', 129.04259),
        ('F', 147.06841),
        ('G', 57.02146),
        ('H', 137.05891),
        ('I', 113.08406),
        ('K', 128.09496),
        ('L', 113.08406),
        ('M', 131.04049),
        ('N', 114.04293),
        ('P', 97.05276),
        ('Q', 128.05858),
        ('R', 156.10111),
        ('S', 87.03203),
        ('T', 101.04768),
        ('V', 99.06841),
        ('W', 186.07931),
        ('Y', 163.06333),
    ];
    
    let mut mass_to_aa: HashMap<f64, char> = HashMap::new();
    for (aa, mass) in amino_acid_masses {
        mass_to_aa.insert(mass, aa);
    }
    
    // Sort spectrum in descending order
    let mut sorted_spectrum = spectrum.to_vec();
    sorted_spectrum.sort_by(|a, b| b.partial_cmp(a).unwrap());
    
    let mut result = String::new();
    let mut remaining = sorted_spectrum.clone();
    
    // Remove the total mass (largest)
    if let Some(total) = remaining.pop() {
        let mut current = total;
        
        while current > 0.001 && !remaining.is_empty() {
            let mut best_mass = 0.0;
            let mut best_aa = ' ';
            let mut min_diff = f64::MAX;
            
            // Find the amino acid mass closest to current
            for (&mass, &aa) in &mass_to_aa {
                let diff = (current - mass).abs();
                if diff < min_diff {
                    min_diff = diff;
                    best_mass = mass;
                    best_aa = aa;
                }
            }
            
            // Check if we found a valid match
            if min_diff < 0.01 {  // Allow small tolerance
                result.push(best_aa);
                current -= best_mass;
                
                // Remove the matched mass from remaining
                remaining.retain(|&x| (x - best_mass).abs() > 0.01);
            } else {
                break;
            }
        }
    }
    
    result
}
```

## Key Points

1. **Input Processing**: Read the spectrum masses from standard input
2. **Amino Acid Mapping**: Create a mapping from amino acid letters to their masses
3. **Sequence Reconstruction**: Use a greedy approach to find the amino acid sequence
4. **Precision Handling**: Account for floating-point precision issues with tolerance values

## Time Complexity
- O(n × m) where n is the number of spectrum masses and m is the number of amino acids

## Space Complexity  
- O(m) for the amino acid mass mapping

This solution handles the specific requirements of the Rosalind problem by finding the amino acid sequence that would produce the given spectrum masses.

