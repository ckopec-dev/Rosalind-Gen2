# Rosalind Problem: Generate the Frequency Array of a String (in Rust)

## Problem Description
Given a DNA string of length at most 1000 nucleotides, return the frequency array of nucleotides A, C, G, and T.

## Solution

```rust
fn generate_frequency_array(dna: &str) -> Vec<usize> {
    let mut counts = vec![0; 4]; // A, C, G, T
    
    for nucleotide in dna.chars() {
        match nucleotide {
            'A' => counts[0] += 1,
            'C' => counts[1] += 1,
            'G' => counts[2] += 1,
            'T' => counts[3] += 1,
            _ => panic!("Invalid nucleotide: {}", nucleotide),
        }
    }
    
    counts
}

fn main() {
    // Example usage
    let dna = "ACGTACGT";
    let frequencies = generate_frequency_array(dna);
    
    println!("DNA: {}", dna);
    println!("Frequency array: {:?}", frequencies);
    // Output: [2, 2, 2, 2]
    
    // Another example
    let dna2 = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC";
    let frequencies2 = generate_frequency_array(dna2);
    
    println!("DNA: {}", dna2);
    println!("Frequency array: {:?}", frequencies2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_frequency_array() {
        assert_eq!(generate_frequency_array("ACGTACGT"), vec![2, 2, 2, 2]);
        assert_eq!(generate_frequency_array("AAAA"), vec![4, 0, 0, 0]);
        assert_eq!(generate_frequency_array("CCCC"), vec![0, 4, 0, 0]);
        assert_eq!(generate_frequency_array("GGGG"), vec![0, 0, 4, 0]);
        assert_eq!(generate_frequency_array("TTTT"), vec![0, 0, 0, 4]);
        assert_eq!(generate_frequency_array("ACGT"), vec![1, 1, 1, 1]);
    }
}
```

## Alternative Implementation with Character Mapping

```rust
fn generate_frequency_array_v2(dna: &str) -> Vec<usize> {
    let mut counts = [0; 4]; // A, C, G, T
    
    for nucleotide in dna.chars() {
        let index = match nucleotide {
            'A' => 0,
            'C' => 1,
            'G' => 2,
            'T' => 3,
            _ => panic!("Invalid nucleotide: {}", nucleotide),
        };
        counts[index] += 1;
    }
    
    counts.to_vec()
}

// More efficient version using ASCII values
fn generate_frequency_array_v3(dna: &str) -> Vec<usize> {
    let mut counts = [0; 4];
    
    for nucleotide in dna.bytes() {
        match nucleotide {
            b'A' => counts[0] += 1,
            b'C' => counts[1] += 1,
            b'G' => counts[2] += 1,
            b'T' => counts[3] += 1,
            _ => panic!("Invalid nucleotide: {}", nucleotide as char),
        }
    }
    
    counts.to_vec()
}
```

## Key Points

1. **Input**: A DNA string containing only A, C, G, T nucleotides
2. **Output**: A vector of 4 integers representing frequencies of A, C, G, T respectively
3. **Time Complexity**: O(n) where n is the length of the DNA string
4. **Space Complexity**: O(1) since we always use a fixed-size array of 4 elements

## Usage Example

```bash
# Compile and run
rustc -o frequency_array frequency_array.rs
./frequency_array
```

The solution handles the standard Rosalind problem requirements and can be easily adapted for different input formats or additional requirements.

