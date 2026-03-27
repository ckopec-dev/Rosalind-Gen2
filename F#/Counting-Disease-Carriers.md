# Rosalind Problem: Counting Disease Carriers (F# Solution)

## Problem Description
Given three positive integers representing the number of individuals in a population with genotypes AA, Aa, and aa respectively, we need to determine the probability that a randomly selected individual carries at least one A allele (i.e., is either AA or Aa).

## Solution Approach
1. Calculate the total population size
2. Determine the probability of selecting an individual with at least one A allele
3. This is equivalent to 1 minus the probability of selecting an aa individual

## F# Implementation

```fsharp
let countingDiseaseCarriers (AA: int) (Aa: int) (aa: int) : float =
    let total = AA + Aa + aa
    if total = 0 then 0.0
    else
        // Probability of selecting an aa individual
        let prob_aa = float aa / float total
        // Probability of selecting individual with at least one A allele
        1.0 - prob_aa

// Alternative implementation with more explicit calculation
let countingDiseaseCarriers2 (AA: int) (Aa: int) (aa: int) : float =
    let total = AA + Aa + aa
    if total = 0 then 0.0
    else
        // Probability of selecting AA or Aa individual
        let prob_carriers = float (AA + Aa) / float total
        prob_carriers

// Test function
let testCountingDiseaseCarriers () =
    // Test case 1: AA=1, Aa=1, aa=1
    let result1 = countingDiseaseCarriers 1 1 1
    printfn "Test 1 - AA=1, Aa=1, aa=1: %f" result1  // Expected: 0.666667
    
    // Test case 2: AA=2, Aa=1, aa=0
    let result2 = countingDiseaseCarriers 2 1 0
    printfn "Test 2 - AA=2, Aa=1, aa=0: %f" result2  // Expected: 1.0
    
    // Test case 3: AA=0, Aa=0, aa=3
    let result3 = countingDiseaseCarriers 0 0 3
    printfn "Test 3 - AA=0, Aa=0, aa=3: %f" result3  // Expected: 0.0

// Main execution
testCountingDiseaseCarriers ()
```

## Explanation

The solution works by:

1. **Calculating total population**: `AA + Aa + aa`
2. **Finding probability of selecting aa individual**: `float aa / float total`
3. **Calculating probability of carriers**: `1.0 - prob_aa`

Since we want the probability that a randomly selected individual carries at least one A allele, we can either:
- Directly calculate the probability of selecting AA or Aa individuals
- Calculate 1 minus the probability of selecting an aa individual

## Sample Input/Output

```
Input: AA=1, Aa=1, aa=1
Output: 0.666667

Input: AA=2, Aa=1, aa=0  
Output: 1.000000

Input: AA=0, Aa=0, aa=3
Output: 0.000000
```

## Key Features

- **Type safety**: Uses `float` for probability calculations
- **Edge case handling**: Handles zero population size
- **Clear logic**: Direct mathematical approach
- **Efficient**: Single pass calculation with O(1) time complexity

The solution correctly handles all edge cases and provides the probability that a randomly selected individual from the population carries at least one A allele.

