# Rosalind Problem: Interleaving Two Motifs (F# Solution)

## Problem Understanding

The Interleaving Two Motifs problem asks us to find the number of ways to interleave two strings such that the result contains both strings as subsequences.

## Solution Approach

We'll use dynamic programming to solve this problem. The key insight is to use a 2D DP table where `dp[i][j]` represents the number of ways to interleave the first `i` characters of string1 and the first `j` characters of string2.

## F# Implementation

```fsharp
open System

let interleaving_motifs (s1: string) (s2: string) : int64 =
    let m = s1.Length
    let n = s2.Length
    
    // Handle edge cases
    if m = 0 then
        if n = 0 then 1L
        else 0L
    elif n = 0 then
        1L
    else
        // Create DP table
        let dp = Array2D.create (m + 1) (n + 1) 0L
        
        // Initialize base cases
        // When s1 is empty, there's only one way to interleave (take all from s2)
        for j in 0 .. n do
            dp.[0, j] <- 1L
        
        // When s2 is empty, there's only one way to interleave (take all from s1)
        for i in 0 .. m do
            dp.[i, 0] <- 1L
        
        // Fill the DP table
        for i in 1 .. m do
            for j in 1 .. n do
                if s1.[i - 1] = s2.[j - 1] then
                    // Characters match - we can take either character
                    dp.[i, j] <- dp.[i - 1, j] + dp.[i, j - 1]
                else
                    // Characters don't match - we must take the character from s1 or s2
                    dp.[i, j] <- dp.[i - 1, j] + dp.[i, j - 1]
        
        dp.[m, n]

// Alternative implementation using memoization
let interleaving_motifs_memo (s1: string) (s2: string) : int64 =
    let m = s1.Length
    let n = s2.Length
    
    if m = 0 && n = 0 then 1L
    elif m = 0 || n = 0 then 1L
    else
        let memo = Dictionary<(int * int), int64>()
        
        let rec count_interleavings i j =
            if i = 0 && j = 0 then 1L
            elif i = 0 then 1L
            elif j = 0 then 1L
            else
                match memo.TryFind((i, j)) with
                | Some result -> result
                | None ->
                    let result = 
                        if s1.[i - 1] = s2.[j - 1] then
                            count_interleavings (i - 1) j + count_interleavings i (j - 1)
                        else
                            count_interleavings (i - 1) j + count_interleavings i (j - 1)
                    memo.[(i, j)] <- result
                    result
        
        count_interleavings m n

// Test function
let test_interleaving() =
    // Test case 1
    let s1 = "AB"
    let s2 = "CD"
    let result1 = interleaving_motifs s1 s2
    printfn "Interleavings of %s and %s: %d" s1 s2 result1
    
    // Test case 2
    let s3 = "A"
    let s4 = "B"
    let result2 = interleaving_motifs s3 s4
    printfn "Interleavings of %s and %s: %d" s3 s4 result2
    
    // Test case 3
    let s5 = "AC"
    let s6 = "BD"
    let result3 = interleaving_motifs s5 s6
    printfn "Interleavings of %s and %s: %d" s5 s6 result3

// Main execution
let main() =
    test_interleaving()
    
    // Example from Rosalind problem
    let input1 = "AB"
    let input2 = "CD"
    let result = interleaving_motifs input1 input2
    printfn "Result: %d" result

// Run the program
main()
```

## Explanation

### Algorithm Steps:

1. **Base Cases**: 
   - If one string is empty, there's only one way to interleave (take all from the other string)
   - If both are empty, there's one way (empty result)

2. **DP Transition**:
   - `dp[i][j]` = number of ways to interleave first `i` chars of s1 and first `j` chars of s2
   - If `s1[i-1] = s2[j-1]`: we can choose either character, so `dp[i][j] = dp[i-1][j] + dp[i][j-1]`
   - If `s1[i-1] ≠ s2[j-1]`: we must take from one string or the other, so `dp[i][j] = dp[i-1][j] + dp[i][j-1]`

3. **Time Complexity**: O(m×n) where m and n are the lengths of the strings
4. **Space Complexity**: O(m×n) for the DP table

## Sample Input/Output

For input strings "AB" and "CD":
- Possible interleavings: "ABCD", "ACBD", "ACDB", "CABD", "CADB", "CDAB"
- Output: 6

This solution efficiently computes the number of valid interleavings using dynamic programming principles.

