# Rosalind Problem: Counting Rooted Binary Trees

## Problem Statement
Given a positive integer n (n ≤ 1000), return the number of distinct rooted binary trees on n labeled taxa.

## Solution in F#

```fsharp
let countingRootedBinaryTrees n =
    // For a rooted binary tree with n labeled taxa,
    // the number of distinct trees is given by the formula:
    // (2n-3)!! * 2^(n-1) where !! is the double factorial
    // However, there's a simpler approach using the Catalan numbers
    
    // The number of rooted binary trees with n labeled leaves
    // is (2n-3)!! * 2^(n-1)
    // But we can also think of it as: (2n-3)!! * 2^(n-1) = (2n-3)!! * 2^(n-1)
    
    // Actually, for rooted binary trees with n labeled leaves:
    // The formula is (2n-3)!! * 2^(n-1) = (2n-3)!! * 2^(n-1)
    
    // Let's use a more direct approach:
    // The number of rooted binary trees with n labeled leaves is:
    // (2n-3)!! * 2^(n-1)
    
    // Helper function to compute double factorial
    let rec doubleFactorial n =
        if n <= 1 then 1L
        else n * doubleFactorial (n - 2)
    
    // For n labeled taxa, the number of rooted binary trees is:
    // (2n-3)!! * 2^(n-1)
    let doubleFact = doubleFactorial (2 * n - 3)
    let powerOfTwo = 2L ** (n - 1)
    
    doubleFact * powerOfTwo

// Alternative cleaner approach using direct formula
let countingRootedBinaryTrees' n =
    // For rooted binary trees with n labeled taxa, the answer is:
    // (2n-3)!! * 2^(n-1)
    // where (2n-3)!! = (2n-3) * (2n-5) * ... * 3 * 1
    
    let rec doubleFactorial n =
        if n <= 1 then 1L
        else n * doubleFactorial (n - 2)
    
    // Calculate (2n-3)!! * 2^(n-1)
    let doubleFact = doubleFactorial (2 * n - 3)
    let powerOfTwo = 2L ** (n - 1)
    
    doubleFact * powerOfTwo

// Even more efficient approach using the recurrence relation
let countingRootedBinaryTrees'' n =
    // The number of rooted binary trees with n labeled taxa is:
    // (2n-3)!! * 2^(n-1)
    
    if n <= 0 then 0L
    elif n = 1 then 1L
    else
        let rec doubleFact n acc =
            if n <= 1 then acc
            else doubleFact (n - 2) (n * acc)
        
        let df = doubleFact (2 * n - 3) 1L
        let power = 2L ** (n - 1)
        df * power

// Most practical implementation
let countRootedBinaryTrees n =
    if n <= 0 then 0L
    elif n = 1 then 1L
    else
        // Calculate (2n-3)!! = (2n-3) * (2n-5) * ... * 3 * 1
        let rec doubleFactorial n =
            if n <= 1 then 1L
            else n * doubleFactorial (n - 2)
        
        let doubleFact = doubleFactorial (2 * n - 3)
        let powerOfTwo = 2L ** (n - 1)
        doubleFact * powerOfTwo

// Test the function
let test = 
    let n = 4
    let result = countRootedBinaryTrees n
    printfn "Number of rooted binary trees with %d labeled taxa: %d" n result

// For the actual Rosalind submission, you would use:
let solve n = 
    countRootedBinaryTrees n
```

## Explanation

The problem asks for the number of distinct rooted binary trees that can be formed with n labeled taxa.

The mathematical formula for the number of rooted binary trees with n labeled leaves is:

**Number of trees = (2n-3)!! × 2^(n-1)**

Where:
- (2n-3)!! is the double factorial of (2n-3)
- 2^(n-1) is 2 raised to the power of (n-1)

**Double factorial definition:**
- n!! = n × (n-2) × (n-4) × ... × 1 (for odd n)
- n!! = n × (n-2) × (n-4) × ... × 2 (for even n)

**Example:**
For n = 4:
- (2×4-3)!! = 5!! = 5 × 3 × 1 = 15
- 2^(4-1) = 2^3 = 8
- Result = 15 × 8 = 120

The solution uses a recursive approach to calculate the double factorial and then multiplies by the appropriate power of 2.

