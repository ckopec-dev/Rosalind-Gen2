# Rosalind Problem: Median - F# Solution

## Problem Statement
Given a collection of integers, return the median of the collection.

## Solution

```fsharp
open System

let median (numbers: int list) : float =
    let sorted = List.sort numbers
    let n = List.length sorted
    let half = n / 2
    
    if n % 2 = 1 then
        // Odd number of elements - return middle element
        float sorted.[half]
    else
        // Even number of elements - return average of two middle elements
        let left = sorted.[half - 1]
        let right = sorted.[half]
        float (left + right) / 2.0

// Alternative implementation using List.item
let median' (numbers: int list) : float =
    let sorted = List.sort numbers
    let n = List.length sorted
    
    if n % 2 = 1 then
        float sorted.[n / 2]
    else
        let mid1 = sorted.[(n / 2) - 1]
        let mid2 = sorted.[n / 2]
        float (mid1 + mid2) / 2.0

// For Rosalind input processing
let solveMedian (input: string) : string =
    let numbers = 
        input.Split(' ')
        |> Array.map int
        |> Array.toList
    
    median numbers
    |> sprintf "%.1f"

// Example usage
let exampleInput = "3 1 4 1 5 9 2 6"
let result = solveMedian exampleInput
printfn "Median: %s" result
```

## Explanation

The solution works as follows:

1. **Sort the input**: First, we sort the list of integers in ascending order
2. **Determine median position**: 
   - If the list has an odd number of elements, the median is the middle element
   - If the list has an even number of elements, the median is the average of the two middle elements
3. **Handle both cases**: 
   - For odd length: return the element at index `n/2`
   - For even length: return the average of elements at indices `n/2-1` and `n/2`

## Key Features

- **Type Safety**: Uses `int list` for input and `float` for output to handle decimal results
- **Edge Case Handling**: Properly handles both odd and even length lists
- **Rosalind Compatible**: Can be easily adapted for Rosalind input/output format
- **Efficient**: O(n log n) time complexity due to sorting

## Sample Input/Output

**Input**: `3 1 4 1 5 9 2 6`
**Output**: `3.5`

**Input**: `1 3 5 7`
**Output**: `4.0`

The solution correctly handles the median calculation for any collection of integers, returning the result as a floating-point number with one decimal place as required by Rosalind's formatting.

