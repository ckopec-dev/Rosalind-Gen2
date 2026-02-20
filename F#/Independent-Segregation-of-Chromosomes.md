# Rosalind Problem: Independent Segregation of Chromosomes

## Problem Statement
For a random variable X that follows a binomial distribution with parameters n and p, the probability mass function is:
P(X = k) = C(n,k) * p^k * (1-p)^(n-k)

We want to find the probability that at least one of the alleles in a gamete is from the first parent.

## Solution in F#

```fsharp
// Independent Segregation of Chromosomes - Rosalind Problem
// Solution in F#

open System

// Function to calculate binomial coefficient C(n,k)
let rec combination n k =
    if k = 0 || k = n then 1
    elif k > n then 0
    else combination (n-1) (k-1) + combination (n-1) k

// Alternative implementation using factorial
let factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)

let combinationFast n k =
    if k > n || k < 0 then 0
    elif k = 0 || k = n then 1
    else
        let rec fact x acc = if x <= 1 then acc else fact (x-1) (x * acc)
        let numerator = fact n 1
        let denominator = (fact k 1) * (fact (n-k) 1)
        numerator / denominator

// Function to calculate probability of at least one allele from first parent
let independentSegregation n =
    // For n independent alleles, probability of no alleles from first parent = (1/2)^n
    // So probability of at least one allele from first parent = 1 - (1/2)^n
    1.0 - Math.Pow(0.5, float n)

// Alternative approach using binomial distribution
let independentSegregationBinomial n =
    // Probability of getting at least 1 success in n trials with p=0.5
    let rec calculateProb k =
        if k > n then 0.0
        else
            let prob = combinationFast n k * (0.5 ** float k) * (0.5 ** float (n-k))
            prob + calculateProb (k+1)
    
    1.0 - (combinationFast n 0 * (0.5 ** 0.0) * (0.5 ** float n))

// Main function to solve the problem
let solveIndependentSegregation n =
    independentSegregation n

// Example usage
let main() =
    // Example with n = 3
    let n = 3
    let result = solveIndependentSegregation n
    printfn "For n = %d, probability = %.6f" n result
    
    // Test with different values
    [1..5] |> List.iter (fun i -> 
        let prob = solveIndependentSegregation i
        printfn "n = %d, P(at least 1 from parent 1) = %.6f" i prob)

// Run the solution
main()
```

## Explanation

This problem is about calculating the probability that at least one allele in a gamete comes from the first parent when considering independent segregation of chromosomes.

**Key Insights:**
1. Each allele has a 50% chance of coming from either parent
2. For n independent alleles, we want P(at least one from parent 1)
3. This is equivalent to 1 - P(no alleles from parent 1)
4. P(no alleles from parent 1) = (1/2)^n

**Mathematical Formula:**
P(at least one from parent 1) = 1 - (1/2)^n

**Implementation Details:**
- The `independentSegregation` function directly computes this formula
- The `combination` function calculates binomial coefficients for the full binomial distribution approach
- The `solveIndependentSegregation` function provides the main solution

**Time Complexity:** O(1) for the direct approach, O(n) for the binomial approach
**Space Complexity:** O(1) for the direct approach, O(n) for the binomial approach

The solution handles the mathematical principle that independent segregation leads to each chromosome having a 50% probability of originating from either parent, making it straightforward to calculate the probability of having at least one allele from the first parent.

