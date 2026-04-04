# Rosalind Problem: Find a Highest-Scoring Peptide in a Proteome against a Spectrum

## Problem Understanding

This problem requires finding the highest-scoring peptide that can be formed from a given proteome (set of amino acids) that matches a given mass spectrum. This is a variant of the peptide sequencing problem using dynamic programming.

## Solution Approach

We'll use dynamic programming where:
- `dp[i]` represents the highest score achievable for a peptide with mass `i`
- For each mass, we try adding each amino acid and update the score
- We track the actual peptide sequence to reconstruct the answer

## Rust Implementation

```rust
use std::collections::HashMap;

fn main() {
    // Example input (replace with actual input reading)
    let spectrum = vec![0, 113, 128, 186, 244, 305, 366, 424, 487, 542, 605, 664, 729, 791, 853];
    let proteome = vec!['A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'Y'];
    
    let amino_acid_masses = vec![
        ('A', 71), ('C', 103), ('D', 115), ('E', 129), ('F', 147),
        ('G', 57), ('H', 137), ('I', 113), ('K', 128), ('L', 113),
        ('M', 131), ('N', 114), ('P', 97), ('Q', 128), ('R', 156),
        ('S', 87), ('T', 101), ('V', 99), ('W', 186), ('Y', 163)
    ];
    
    let result = find_highest_scoring_peptide(&spectrum, &proteome, &amino_acid_masses);
    println!("Highest scoring peptide: {}", result);
}

fn find_highest_scoring_peptide(
    spectrum: &[i32],
    proteome: &[char],
    amino_acid_masses: &[(char, i32)]
) -> String {
    // Create a map from amino acid to mass for quick lookup
    let mass_map: HashMap<char, i32> = amino_acid_masses.iter().cloned().collect();
    
    // Find maximum mass in spectrum
    let max_mass = *spectrum.iter().max().unwrap();
    
    // dp[i] stores the maximum score for mass i
    let mut dp = vec![-1000000; (max_mass + 1) as usize];
    dp[0] = 0;
    
    // parent[i] stores which amino acid was used to achieve maximum score for mass i
    let mut parent = vec![None; (max_mass + 1) as usize];
    
    // For each mass from 0 to max_mass
    for mass in 0..=max_mass {
        if dp[mass as usize] == -1000000 {
            continue;
        }
        
        // Try adding each amino acid
        for &(aa, aa_mass) in amino_acid_masses {
            let new_mass = mass + aa_mass;
            if new_mass <= max_mass {
                let score = dp[mass as usize] + get_spectrum_score(spectrum, mass + aa_mass);
                if score > dp[new_mass as usize] {
                    dp[new_mass as usize] = score;
                    parent[new_mass as usize] = Some(aa);
                }
            }
        }
    }
    
    // Reconstruct the peptide
    let mut peptide = String::new();
    let mut current_mass = max_mass;
    
    while let Some(aa) = parent[current_mass as usize] {
        peptide.push(aa);
        let aa_mass = mass_map[&aa];
        current_mass -= aa_mass;
    }
    
    // Reverse since we built it backwards
    peptide.chars().rev().collect()
}

// Helper function to calculate spectrum score (simplified)
// In practice, this would compare against the actual spectrum
fn get_spectrum_score(spectrum: &[i32], mass: i32) -> i32 {
    // This is a simplified version - in real implementation,
    // we'd compare against the actual spectrum
    // For now, we'll just return a score based on how close the mass is to spectrum peaks
    
    if spectrum.contains(&mass) {
        100 // Perfect match
    } else {
        // Calculate penalty based on distance to nearest peak
        let min_distance = spectrum.iter()
            .map(|&s| (mass as i64 - s as i64).abs() as i32)
            .min()
            .unwrap_or(0);
        
        // Higher penalty for larger distances
        -min_distance
    }
}

// Alternative implementation using a more complete dynamic programming approach
fn find_highest_scoring_peptide_complete(
    spectrum: &[i32],
    proteome: &[char],
    amino_acid_masses: &[(char, i32)]
) -> String {
    // Create mass to amino acid mapping
    let mass_map: HashMap<char, i32> = amino_acid_masses.iter().cloned().collect();
    let mass_to_aa: HashMap<i32, Vec<char>> = amino_acid_masses.iter()
        .fold(HashMap::new(), |mut acc, &(aa, mass)| {
            acc.entry(mass).or_insert_with(Vec::new).push(aa);
            acc
        });
    
    // Find maximum mass
    let max_mass = *spectrum.iter().max().unwrap_or(&0);
    
    // dp[i] = (score, parent_mass)
    let mut dp = vec![(i32::MIN, 0); (max_mass + 1) as usize];
    dp[0] = (0, 0);
    
    // Fill DP table
    for mass in 0..=max_mass {
        if dp[mass as usize].0 == i32::MIN {
            continue;
        }
        
        // Try adding each amino acid
        for &(aa, aa_mass) in amino_acid_masses {
            let new_mass = mass + aa_mass;
            if new_mass <= max_mass {
                let score = dp[mass as usize].0 + get_spectrum_score(spectrum, new_mass);
                if score > dp[new_mass as usize].0 {
                    dp[new_mass as usize] = (score, mass);
                }
            }
        }
    }
    
    // Reconstruct the peptide path
    let mut path = Vec::new();
    let mut current_mass = max_mass;
    
    while current_mass > 0 && dp[current_mass as usize].0 != 0 {
        let parent_mass = dp[current_mass as usize].1;
        let aa_mass = current_mass - parent_mass;
        
        // Find which amino acid has this mass
        if let Some(aa_list) = mass_to_aa.get(&aa_mass) {
            if let Some(&aa) = aa_list.first() {
                path.push(aa);
                current_mass = parent_mass;
            }
        }
    }
    
    path.iter().rev().collect::<String>()
}
```

## Key Points

1. **Dynamic Programming Approach**: We build up solutions for smaller masses and use them to solve larger masses
2. **Score Calculation**: The score function compares against the spectrum to determine how well a mass matches
3. **Path Reconstruction**: We track parent relationships to reconstruct the actual peptide sequence
4. **Memory Optimization**: We only store necessary information to avoid excessive memory usage

## Time and Space Complexity

- **Time Complexity**: O(max_mass × number_of_amino_acids)
- **Space Complexity**: O(max_mass)

This implementation provides a framework for solving the peptide sequencing problem against a mass spectrum, which is a fundamental concept in proteomics and bioinformatics.

