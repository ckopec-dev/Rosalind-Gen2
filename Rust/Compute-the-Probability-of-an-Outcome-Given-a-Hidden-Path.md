# Rosalind Problem: Compute the Probability of an Outcome Given a Hidden Path

This problem asks us to compute the probability of observing a specific sequence of outcomes given a hidden path in a Hidden Markov Model (HMM).

## Problem Understanding

We are given:
- A hidden path (sequence of states)
- A sequence of observations (emissions)
- The emission probabilities for each state

We need to calculate the probability of observing the given sequence of outcomes given the hidden path.

## Solution Approach

In a Hidden Markov Model, the probability of observing a sequence given a hidden path is the product of emission probabilities for each observation given its corresponding hidden state.

## Rust Implementation

```rust
use std::collections::HashMap;

fn compute_probability_of_outcome_given_path(
    hidden_path: &str,
    emissions: &str,
    emission_probs: &HashMap<(char, char), f64>
) -> f64 {
    let path_chars: Vec<char> = hidden_path.chars().collect();
    let emission_chars: Vec<char> = emissions.chars().collect();
    
    // Check that lengths match
    if path_chars.len() != emission_chars.len() {
        panic!("Hidden path and emissions must have the same length");
    }
    
    let mut probability = 1.0;
    
    // For each position, multiply by the emission probability
    for (state, emission) in path_chars.iter().zip(emission_chars.iter()) {
        let key = (*state, *emission);
        if let Some(&prob) = emission_probs.get(&key) {
            probability *= prob;
        } else {
            panic!("No emission probability found for state {:?} and emission {:?}", state, emission);
        }
    }
    
    probability
}

fn main() {
    // Example usage
    let hidden_path = "AABBBAABABAAA";
    let emissions = "xyxxyxyxxyxxy";
    
    // Create emission probability map
    let mut emission_probs = HashMap::new();
    emission_probs.insert(('A', 'x'), 0.176);
    emission_probs.insert(('A', 'y'), 0.596);
    emission_probs.insert(('A', 'z'), 0.228);
    emission_probs.insert(('B', 'x'), 0.225);
    emission_probs.insert(('B', 'y'), 0.572);
    emission_probs.insert(('B', 'z'), 0.203);
    
    let result = compute_probability_of_outcome_given_path(
        hidden_path,
        emissions,
        &emission_probs
    );
    
    println!("Probability: {:.10}", result);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_compute_probability() {
        let hidden_path = "AABB";
        let emissions = "xyxz";
        
        let mut emission_probs = HashMap::new();
        emission_probs.insert(('A', 'x'), 0.176);
        emission_probs.insert(('A', 'y'), 0.596);
        emission_probs.insert(('A', 'z'), 0.228);
        emission_probs.insert(('B', 'x'), 0.225);
        emission_probs.insert(('B', 'y'), 0.572);
        emission_probs.insert(('B', 'z'), 0.203);
        
        let result = compute_probability_of_outcome_given_path(
            hidden_path,
            emissions,
            &emission_probs
        );
        
        // Expected: 0.176 * 0.572 * 0.203 * 0.228 = 0.004759...
        let expected = 0.176 * 0.572 * 0.203 * 0.228;
        assert!((result - expected).abs() < 1e-10);
    }
}
```

## Explanation

1. **Input Parameters**:
   - `hidden_path`: A string representing the sequence of hidden states
   - `emissions`: A string representing the observed sequence
   - `emission_probs`: A HashMap mapping (state, emission) pairs to their probabilities

2. **Algorithm**:
   - Convert both strings to character vectors
   - Validate that the lengths match
   - For each position, multiply the emission probability of the current emission given the current state
   - Return the final probability

3. **Time Complexity**: O(n) where n is the length of the path/sequence
4. **Space Complexity**: O(1) additional space (not counting input storage)

## Key Points

- The probability of a sequence of observations given a hidden path is the product of individual emission probabilities
- Each emission probability is looked up in the provided emission probability table
- The solution assumes that the emission probabilities are provided in the correct format

This approach directly implements the mathematical formula for computing the probability of observations given a hidden path in a Hidden Markov Model.

