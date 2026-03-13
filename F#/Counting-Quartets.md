# Rosalind Problem: Counting Quartets (F# Solution)

## Problem Understanding

The problem asks us to count the number of quartets (4-tuples) that can be formed from a set of n taxa, where each quartet represents a possible evolutionary relationship pattern.

## Solution Approach

In phylogenetics, a quartet is a set of four taxa with a specific evolutionary relationship. For n taxa, we need to count how many distinct quartets can be formed.

The number of ways to choose 4 taxa from n taxa is C(n,4) = n!/(4!(n-4)!) = n(n-1)(n-2)(n-3)/24

## F# Implementation

```fsharp
open System

let countQuartets n =
    if n < 4 then
        0
    else
        let n = float n
        let result = (n * (n - 1.0) * (n - 2.0) * (n - 3.0)) / 24.0
        int result

// Alternative implementation using integer arithmetic to avoid floating point issues
let countQuartetsInt n =
    if n < 4 then
        0
    else
        let rec factorial n =
            if n <= 1 then 1
            else n * factorial (n - 1)
        
        let rec combination n k =
            if k > n || k < 0 then 0
            elif k = 0 || k = n then 1
            else combination (n - 1) (k - 1) + combination (n - 1) k
        
        // Using the formula C(n,4) = n!/(4!(n-4)!)
        let rec computeCombination n k =
            if k > n || k < 0 then 0
            elif k = 0 || k = n then 1
            elif k > n - k then computeCombination n (n - k)
            else
                let mutable result = 1L
                for i in 1..k do
                    result <- result * (int64 (n - i + 1)) / int64 i
                result
        
        int (computeCombination n 4)

// More efficient implementation using direct calculation
let countQuartetsEfficient n =
    if n < 4 then
        0
    else
        let n = float n
        let result = (n * (n - 1.0) * (n - 2.0) * (n - 3.0)) / 24.0
        int result

// Read input and solve
let solveQuartets inputFile =
    try
        let input = System.IO.File.ReadAllText(inputFile)
        let n = int input.Trim()
        let result = countQuartets n
        printfn "%d" result
        result
    with
    | :? System.IO.FileNotFoundException ->
        printfn "File not found"
        0
    | :? System.FormatException ->
        printfn "Invalid input format"
        0

// Example usage
let main argv =
    match argv.Length with
    | 0 -> 
        // Test with example
        printfn "Testing with n = 6:"
        printfn "%d" (countQuartets 6)
        printfn "Testing with n = 4:"
        printfn "%d" (countQuartets 4)
        printfn "Testing with n = 3:"
        printfn "%d" (countQuartets 3)
    | _ -> 
        // Read from file
        solveQuartets argv.[0]
    0

// Direct test function
let testQuartets() =
    printfn "Testing quartet counting:"
    printfn "n=4: %d" (countQuartets 4)  // Expected: 1
    printfn "n=5: %d" (countQuartets 5)  // Expected: 5
    printfn "n=6: %d" (countQuartets 6)  // Expected: 15
    printfn "n=7: %d" (countQuartets 7)  // Expected: 35
    printfn "n=8: %d" (countQuartets 8)  // Expected: 70

// Run tests
testQuartets()
```

## Explanation

The solution works by:

1. **Mathematical Formula**: For n taxa, the number of ways to choose 4 taxa (which form a quartet) is given by the combination formula C(n,4) = n!/(4!(n-4)!)

2. **Simplified Calculation**: This can be simplified to n(n-1)(n-2)(n-3)/24

3. **Edge Case Handling**: If n < 4, no quartets can be formed, so return 0

4. **Integer Arithmetic**: The solution handles both floating-point and integer arithmetic approaches for accuracy

## Sample Input/Output

**Input**: 
```
6
```

**Output**: 
```
15
```

This represents the number of ways to choose 4 taxa from 6 taxa, which equals 15.

## Time Complexity
- **Time**: O(1) - constant time calculation
- **Space**: O(1) - constant space usage

The solution is efficient and handles the mathematical computation directly without needing to generate actual quartets.

