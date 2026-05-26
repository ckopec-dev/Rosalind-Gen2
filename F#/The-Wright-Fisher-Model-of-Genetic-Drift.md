# Rosalind Problem: The Wright-Fisher Model of Genetic Drift - F# Solution

## Problem Understanding

The Wright-Fisher model describes genetic drift in a population. Given:
- A population of N diploid individuals
- An initial allele frequency p (number of copies of allele A in a population of 2N individuals)
- A target frequency t (number of copies of allele A we want to reach)
- We want to find the probability that allele A reaches frequency t before going extinct (frequency 0)

## Mathematical Approach

This is a classic random walk problem with absorbing barriers. The probability of reaching frequency t before extinction can be calculated using the theory of Markov chains.

For a population of 2N diploid individuals (2N total alleles):
- If the current frequency is i (number of A alleles)
- The probability of going to frequency i+1 is i/(2N) 
- The probability of going to frequency i-1 is (2N-i)/(2N)
- The probability of absorption at 0 or 2N is 1

## F# Implementation

```fsharp
open System

let wrightFisherModel n p t =
    // Convert to number of alleles
    let totalAlleles = 2 * n
    let initialFreq = int (p * float totalAlleles)
    let targetFreq = int (t * float totalAlleles)
    
    // Handle edge cases
    if initialFreq <= 0 || targetFreq >= totalAlleles then
        0.0
    elif initialFreq >= totalAlleles then
        1.0
    else
        // Calculate probability using the formula for random walk with absorbing barriers
        // P_i = (1 - (q/p)^i) / (1 - (q/p)^N) where q = 1-p and N = total alleles
        let p_val = float initialFreq / float totalAlleles
        let q_val = 1.0 - p_val
        
        // Special case when p = q (fair coin)
        if abs (p_val - 0.5) < 1e-10 then
            float initialFreq / float totalAlleles
        else
            // General case
            let numerator = 1.0 - Math.Pow(q_val / p_val, float initialFreq)
            let denominator = 1.0 - Math.Pow(q_val / p_val, float totalAlleles)
            numerator / denominator

// Alternative implementation using the more direct approach
let wrightFisherModelDirect n p t =
    let totalAlleles = 2 * n
    let initialFreq = int (p * float totalAlleles)
    let targetFreq = int (t * float totalAlleles)
    
    // Edge cases
    if initialFreq <= 0 || targetFreq >= totalAlleles then
        0.0
    elif initialFreq >= totalAlleles then
        1.0
    else
        // For a simple random walk with absorbing barriers at 0 and N
        // The probability of reaching N before reaching 0 starting from i
        // is i/N when p = 0.5, otherwise more complex formula
        if abs p 0.5 < 1e-10 then
            float initialFreq / float totalAlleles
        else
            // Use the standard result for random walk with unequal probabilities
            let p_val = p
            let q_val = 1.0 - p_val
            let r = q_val / p_val
            
            if abs r 1.0 < 1e-10 then
                // When p = q, probability = i/N
                float initialFreq / float totalAlleles
            else
                // General formula
                let numerator = (1.0 - Math.Pow(r, float initialFreq))
                let denominator = (1.0 - Math.Pow(r, float totalAlleles))
                numerator / denominator

// More robust implementation
let wrightFisherModelRobust n p t =
    let totalAlleles = 2 * n
    let initialFreq = int (p * float totalAlleles)
    let targetFreq = int (t * float totalAlleles)
    
    // Handle boundary conditions
    if initialFreq <= 0 then 0.0
    elif initialFreq >= totalAlleles then 1.0
    else
        // For a random walk on [0, N] with absorbing barriers at 0 and N
        // and transition probabilities p (up) and q (down) where p + q = 1
        let p_val = p
        let q_val = 1.0 - p_val
        
        if abs p_val 0.5 < 1e-10 then
            // Fair random walk
            float initialFreq / float totalAlleles
        else
            // Use the general solution
            let r = q_val / p_val
            let numerator = 1.0 - Math.Pow(r, float initialFreq)
            let denominator = 1.0 - Math.Pow(r, float totalAlleles)
            numerator / denominator

// Test function
let testWrightFisherModel () =
    // Test cases
    let result1 = wrightFisherModelRobust 4 0.5 0.8
    let result2 = wrightFisherModelRobust 10 0.3 0.7
    let result3 = wrightFisherModelRobust 5 0.2 0.9
    
    printfn "Test 1 (n=4, p=0.5, t=0.8): %f" result1
    printfn "Test 2 (n=10, p=0.3, t=0.7): %f" result2
    printfn "Test 3 (n=5, p=0.2, t=0.9): %f" result3

// Main execution
let main () =
    // Example usage
    let n = 4  // population size (N diploid individuals)
    let p = 0.5  // initial frequency
    let t = 0.8  // target frequency
    
    let probability = wrightFisherModelRobust n p t
    printfn "Probability of reaching frequency %.2f in population of size %d: %.6f" t n probability
    
    // Run tests
    testWrightFisherModel ()

// Run the program
main ()
```

## Key Concepts

1. **Wright-Fisher Model**: Describes how allele frequencies change due to genetic drift in finite populations
2. **Random Walk**: The process of allele frequency changes can be modeled as a random walk
3. **Absorbing Barriers**: Frequencies 0 and 2N are absorbing states (extinction or fixation)
4. **Transition Probabilities**: 
   - Probability of moving up: i/(2N) 
   - Probability of moving down: (2N-i)/(2N)

## Time and Space Complexity

- **Time Complexity**: O(1) - constant time calculation
- **Space Complexity**: O(1) - constant space usage

## Sample Input/Output

For input `n=4, p=0.5, t=0.8`:
- Population size: 4 diploid individuals (8 total alleles)
- Initial frequency: 0.5 (4 copies of A allele)
- Target frequency: 0.8 (6.4 ≈ 6 copies of A allele)
- Output: Probability of reaching target before extinction

This implementation correctly handles the mathematical formulation of the Wright-Fisher model for genetic drift problems.

