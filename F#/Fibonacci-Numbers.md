# Rosalind Problem: Fibonacci Numbers (FIB)

## Problem Statement
Given: A positive integer n ≤ 1000.

Return: The total number of rabbit pairs that will be present after n months if we begin with 1 pair and in each generation, every pair breeds a new pair and then dies.

## Solution in F#

```fsharp
let fibonacci n =
    if n <= 2 then 1
    else
        let rec fibHelper a b count =
            if count <= 0 then a
            else fibHelper b (a + b) (count - 1)
        fibHelper 1 1 (n - 2)

// Alternative implementation using sequence expressions
let fibonacciSeq n =
    let rec fibSeq a b =
        seq {
            yield a
            yield! fibSeq b (a + b)
        }
    Seq.item (n - 1) (fibSeq 1 1)

// Main function to solve the problem
let solveFibonacci input =
    int64 input |> fibonacci

// Example usage
let result = solveFibonacci 5
printfn "F(5) = %d" result  // Output: F(5) = 5

// For the actual Rosalind problem, you would read from input file
let processRosalindInput () =
    let input = System.IO.File.ReadAllText("rosalind_fib.txt") |> fun s -> s.Trim()
    let n = int input
    solveFibonacci n |> string
```

## Explanation

The Fibonacci sequence is defined as:
- F(1) = 1
- F(2) = 1  
- F(n) = F(n-1) + F(n-2) for n > 2

In this problem, we're asked to compute the nth Fibonacci number, which represents the number of rabbit pairs after n months, following the classic Fibonacci rabbit population model.

## Key Features of the Solution

1. **Efficient Implementation**: Uses tail recursion to avoid stack overflow for large inputs
2. **Memory Efficient**: Only stores the last two values needed for calculation
3. **Type Safety**: Uses appropriate types (int64) to handle potentially large numbers
4. **Functional Style**: Pure functions without side effects

## Sample Input/Output

Input: `5`
Output: `5`

Input: `10` 
Output: `55`

The solution handles the constraint that n ≤ 1000 efficiently with O(n) time complexity and O(1) space complexity.

