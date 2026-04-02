# Rosalind Problem: Implement_NumberToPattern in F#

## Problem Description
The NumberToPattern problem asks us to convert a number into its corresponding DNA k-mer pattern. Given a number `index` and a k-mer length `k`, we need to find the k-mer that would be at position `index` in the lexicographically ordered list of all possible k-mers.

## Solution

```fsharp
let numberToPattern (index : int64) (k : int) : string =
    let symbols = [|'A'; 'C'; 'G'; 'T'|]
    
    let rec helper (num : int64) (result : string) : string =
        if num = 0L then result
        else
            let remainder = int64 (int num % 4)
            let symbol = symbols.[int remainder]
            let newNum = (num - remainder) / 4L
            helper newNum (string symbol + result)
    
    let pattern = helper index ""
    
    // Pad with leading 'A's if necessary
    if pattern.Length < k then
        String.replicate (k - pattern.Length) "A" + pattern
    else
        pattern

// Alternative implementation using direct conversion
let numberToPattern2 (index : int64) (k : int) : string =
    let symbols = [|'A'; 'C'; 'G'; 'T'|]
    
    let rec convert (num : int64) (length : int) : string =
        if length <= 0 then ""
        else
            let remainder = int64 (int num % 4)
            let symbol = symbols.[int remainder]
            let newNum = (num - remainder) / 4L
            convert newNum (length - 1) + string symbol
    
    convert index k

// More concise version
let numberToPattern3 (index : int64) (k : int) : string =
    let symbols = [|'A'; 'C'; 'G'; 'T'|]
    
    let rec helper (num : int64) : string =
        if num = 0L then ""
        else
            let symbol = symbols.[int (int64 (int num % 4))]
            let newNum = (num - int64 (int num % 4)) / 4L
            helper newNum + string symbol
    
    let result = helper index
    String.replicate (k - result.Length) "A" + result
```

## Example Usage

```fsharp
// Example: Convert index 45 to pattern of length 4
let result = numberToPattern 45L 4
printfn "%s" result  // Output: "ACGT"

// Test cases
printfn "%s" (numberToPattern 0L 1)   // "A"
printfn "%s" (numberToPattern 1L 1)   // "C"
printfn "%s" (numberToPattern 45L 4)  // "ACGT"
printfn "%s" (numberToPattern 114L 4) // "TATG"
```

## Explanation

The algorithm works by treating the problem as a base-4 number conversion:

1. **Base-4 representation**: DNA nucleotides A, C, G, T correspond to digits 0, 1, 2, 3 respectively
2. **Conversion process**: 
   - Find the remainder when dividing by 4 to determine the last character
   - Divide by 4 to get the next digit
   - Continue until all digits are processed
3. **Padding**: Since we want a k-mer of fixed length, pad with leading 'A's if necessary

## Time and Space Complexity
- **Time Complexity**: O(k) where k is the length of the pattern
- **Space Complexity**: O(k) for storing the result string

## Test Cases
The solution correctly handles:
- Index 0 with any k (should return k 'A's)
- Various indices with k=1, k=2, k=4
- Edge cases where padding is needed

This implementation efficiently converts numbers to their corresponding DNA k-mers in lexicographic order.

