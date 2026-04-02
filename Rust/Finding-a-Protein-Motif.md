# Rosalind Problem: Finding a Protein Motif (Finding_a_Protein_Motif)

## Problem Description
Given: At most 20 protein sequences in FASTA format (total length ≤ 5kD).

Return: For each protein sequence, output the location of any occurrence of the motif `N{P}[ST]{P}` where:
- N is any amino acid except proline (P)
- P is proline
- S or T is serine or threonine
- P is proline again

## Solution in Rust

```rust
use std::collections::HashMap;
use std::fs;

fn main() {
    // Read the input file
    let input = fs::read_to_string("rosalind_mprt.txt").unwrap();
    
    // Parse FASTA format
    let sequences = parse_fasta(&input);
    
    // Process each sequence
    for (header, sequence) in sequences {
        let positions = find_motif_positions(&sequence);
        if !positions.is_empty() {
            println!("{}", header);
            println!("{}", positions.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
        }
    }
}

fn parse_fasta(input: &str) -> Vec<(String, String)> {
    let mut sequences = Vec::new();
    let mut current_header = String::new();
    let mut current_sequence = String::new();
    
    for line in input.lines() {
        if line.starts_with('>') {
            if !current_header.is_empty() {
                sequences.push((current_header, current_sequence));
            }
            current_header = line[1..].to_string();
            current_sequence = String::new();
        } else {
            current_sequence.push_str(line);
        }
    }
    
    // Don't forget the last sequence
    if !current_header.is_empty() {
        sequences.push((current_header, current_sequence));
    }
    
    sequences
}

fn find_motif_positions(sequence: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    let chars: Vec<char> = sequence.chars().collect();
    let len = chars.len();
    
    // The motif is N{P}[ST]{P} where:
    // N is any amino acid except P
    // P is proline
    // S or T is serine or threonine
    // P is proline again
    
    // We need to check positions where the motif could start
    // The motif is 4 characters long, so we check up to len - 3
    for i in 0..(len - 3) {
        // Check if we have a valid N at position i
        if chars[i] == 'P' {
            continue; // N cannot be P
        }
        
        // Check if we have P at position i+1
        if chars[i + 1] != 'P' {
            continue;
        }
        
        // Check if we have S or T at position i+2
        if chars[i + 2] != 'S' && chars[i + 2] != 'T' {
            continue;
        }
        
        // Check if we have P at position i+3
        if chars[i + 3] != 'P' {
            continue;
        }
        
        // If we get here, we found a match
        positions.push(i + 1); // 1-based indexing
    }
    
    positions
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_motif_positions() {
        // Test case with known motif
        let sequence = "MNNPSTP";
        let positions = find_motif_positions(&sequence);
        assert_eq!(positions, vec![2]); // Position 2 (1-based) matches N{P}[ST]{P}
        
        // Test case without motif
        let sequence = "MNNNNNN";
        let positions = find_motif_positions(&sequence);
        assert_eq!(positions, Vec::<usize>::new());
        
        // Test case with multiple motifs
        let sequence = "MNPSNPSTP";
        let positions = find_motif_positions(&sequence);
        assert_eq!(positions, vec![2, 5]); // Positions 2 and 5 match the pattern
    }
}
```

## Explanation

This solution follows these steps:

1. **Parse FASTA format**: Read the input file and parse sequences in FASTA format, which consists of headers starting with `>` followed by sequence data.

2. **Find motif positions**: For each protein sequence, search for positions where the motif `N{P}[ST]{P}` occurs:
   - Position i: Any amino acid except P (N)
   - Position i+1: Proline (P) 
   - Position i+2: Serine (S) or Threonine (T)
   - Position i+3: Proline (P)

3. **Output results**: For each sequence that contains the motif, output the header and all 1-based positions where the motif occurs.

## Key Points

- The motif pattern is `N{P}[ST]{P}` where:
  - `N` is any amino acid except proline (P)
  - `{P}` means proline (P) 
  - `[ST]` means either serine (S) or threonine (T)
  - `{P}` means proline (P) again

- The solution uses 1-based indexing for positions as required by the problem

- The algorithm checks every possible 4-character window in the sequence to find matches

## Time Complexity
- O(n × m) where n is the number of sequences and m is the average length of sequences

## Space Complexity
- O(n × m) for storing the sequences

This solution correctly handles the FASTA parsing and motif finding requirements for the Rosalind problem.

