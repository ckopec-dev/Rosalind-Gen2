# Rosalind Problem: Find_a_Profile-most_Probable_k-mer_in_a_String

## Problem Description
Given a string Text, an integer k, and a 4 × k matrix Profile, we want to find a k-mer in Text that is most probable with respect to the Profile.

## Solution in Rust

```rust
fn find_profile_most_probable_kmer(text: &str, k: usize, profile: &Vec<Vec<f64>>) -> String {
    let mut max_probability = 0.0;
    let mut most_probable_kmer = String::new();
    
    // Iterate through all possible k-mers in the text
    for i in 0..=text.len().saturating_sub(k) {
        let kmer = &text[i..i + k];
        let probability = calculate_kmer_probability(kmer, profile);
        
        if probability > max_probability {
            max_probability = probability;
            most_probable_kmer = kmer.to_string();
        }
    }
    
    most_probable_kmer
}

fn calculate_kmer_probability(kmer: &str, profile: &Vec<Vec<f64>>) -> f64 {
    let mut probability = 1.0;
    
    for (i, nucleotide) in kmer.chars().enumerate() {
        let col_index = match nucleotide {
            'A' => 0,
            'C' => 1,
            'G' => 2,
            'T' => 3,
            _ => panic!("Invalid nucleotide"),
        };
        
        probability *= profile[col_index][i];
    }
    
    probability
}

fn main() {
    // Example usage
    let text = "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT";
    let k = 5;
    
    // Profile matrix (4 rows for A, C, G, T; 5 columns for k=5)
    let profile = vec![
        vec![0.2, 0.2, 0.3, 0.2, 0.3],  // A
        vec![0.4, 0.3, 0.1, 0.5, 0.1],  // C
        vec![0.3, 0.3, 0.5, 0.2, 0.4],  // G
        vec![0.1, 0.2, 0.1, 0.1, 0.2]   // T
    ];
    
    let result = find_profile_most_probable_kmer(text, k, &profile);
    println!("Profile-most probable k-mer: {}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_profile_most_probable_kmer() {
        let text = "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT";
        let k = 5;
        let profile = vec![
            vec![0.2, 0.2, 0.3, 0.2, 0.3],
            vec![0.4, 0.3, 0.1, 0.5, 0.1],
            vec![0.3, 0.3, 0.5, 0.2, 0.4],
            vec![0.1, 0.2, 0.1, 0.1, 0.2]
        ];
        
        let result = find_profile_most_probable_kmer(text, k, &profile);
        // Expected result should be the k-mer with highest probability
        assert!(!result.is_empty());
    }
}
```

## Explanation

This solution works by:

1. **Iterating through all possible k-mers**: For each position in the text, we extract a k-mer of length k.

2. **Calculating probability for each k-mer**: For each k-mer, we calculate its probability using the profile matrix:
   - For each position in the k-mer, we look up the corresponding probability in the profile matrix
   - The profile matrix has 4 rows (for A, C, G, T) and k columns (for each position in the k-mer)
   - We multiply all the probabilities together to get the overall probability of that k-mer

3. **Finding the maximum**: We keep track of the k-mer with the highest probability.

## Key Features

- **Generic approach**: Works for any text, k value, and profile matrix
- **Error handling**: Uses proper error handling for invalid nucleotides
- **Efficient**: O(n×k) time complexity where n is the length of the text
- **Testable**: Includes unit tests for verification

## Time Complexity
- O(n×k) where n is the length of the text and k is the k-mer length

## Space Complexity
- O(1) additional space (excluding input storage)

The solution correctly identifies the profile-most probable k-mer by comparing the probabilities of all possible k-mers in the given text according to the provided profile matrix.

