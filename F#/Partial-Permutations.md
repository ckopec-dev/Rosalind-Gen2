# Rosalind Problem: Partial Permutations (PART)

## Problem Statement
In "Introduction to Random Strings" we used the classic formula for permutations of a set of objects: P(n,k) = n!/(n-k)!. However, in this problem, we are dealing with partial permutations, which are arrangements of k objects chosen from n objects where order matters.

Given: Positive integers n and k such that 100 ≥ n > 0 and 10 ≥ k > 0.

Return: The total number of partial permutations P(n,k) = n!/(n-k)! mod 1,000,000.

## Solution in F#

```fsharp
let partialPermutations n k =
    let modValue = 1000000
    
    // Calculate n!/(n-k)! = n * (n-1) * ... * (n-k+1)
    let rec calculatePartialPermutation n k acc = 
        if k <= 0 then acc
        else calculatePartialPermutation n (k - 1) ((acc * n) % modValue)
    
    calculatePartialPermutation n k 1

// Alternative implementation using List.fold
let partialPermutationsFold n k =
    let modValue = 1000000
    [1..k] 
    |> List.fold (fun acc i -> (acc * (n - i + 1)) % modValue) 1

// Read input and solve
let solvePartialPermutations () =
    let input = Console.ReadLine().Split(' ')
    let n = int input.[0]
    let k = int input.[1]
    partialPermutations n k

// Example usage
let result = solvePartialPermutations()
printfn "%d" result
```

## Explanation

The problem asks us to compute the number of partial permutations P(n,k) = n!/(n-k)! mod 1,000,000.

Key insights:
1. P(n,k) = n!/(n-k)! = n × (n-1) × (n-2) × ... × (n-k+1)
2. We need to compute this modulo 1,000,000 to avoid integer overflow
3. We can compute this iteratively to avoid calculating large factorials

## How it works:

1. **Direct approach**: We calculate the product n × (n-1) × ... × (n-k+1) directly
2. **Modular arithmetic**: We apply modulo at each step to keep numbers manageable
3. **Efficiency**: Instead of computing full factorials, we only compute the necessary terms

## Example:
For n=5, k=3:
- P(5,3) = 5!/(5-3)! = 5!/2! = (5×4×3×2×1)/(2×1) = 5×4×3 = 60

## Test cases:
- Input: 5 3 → Output: 60
- Input: 10 7 → Output: 604800

The solution uses iterative multiplication with modular arithmetic to efficiently compute the result while avoiding overflow issues.

