# Rosalind Problem: Counting Optimal Alignments

## Problem Statement
Given two strings s and t, count the number of optimal global alignments between them.

## Solution in F#

```fsharp
open System

let countOptimalAlignments (s: string) (t: string) : int =
    let m = s.Length
    let n = t.Length
    
    // Create DP table for edit distances
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize base cases
    for i in 0 .. m do
        dp.[i, 0] <- i
    
    for j in 0 .. n do
        dp.[0, j] <- j
    
    // Fill the DP table for edit distances
    for i in 1 .. m do
        for j in 1 .. n do
            let matchCost = if s.[i-1] = t.[j-1] then 0 else 1
            dp.[i, j] <- min 
                (dp.[i-1, j] + 1)  // deletion
                (min 
                    (dp.[i, j-1] + 1)  // insertion
                    (dp.[i-1, j-1] + matchCost))  // substitution
    
    let optimalDistance = dp.[m, n]
    
    // Create DP table for counting alignments
    let count = Array2D.create (m + 1) (n + 1) 0L
    
    // Initialize base cases for counting
    count.[0, 0] <- 1L
    
    for i in 1 .. m do
        count.[i, 0] <- if dp.[i, 0] = optimalDistance then count.[i-1, 0] else 0L
    
    for j in 1 .. n do
        count.[0, j] <- if dp.[0, j] = optimalDistance then count.[0, j-1] else 0L
    
    // Fill the counting DP table
    for i in 1 .. m do
        for j in 1 .. n do
            let matchCost = if s.[i-1] = t.[j-1] then 0 else 1
            let currentDistance = dp.[i, j]
            
            if currentDistance = optimalDistance then
                let mut = if matchCost = 0 then 1L else 0L
                
                // Count paths from deletion
                if dp.[i-1, j] + 1 = currentDistance then
                    count.[i, j] <- count.[i, j] + count.[i-1, j]
                
                // Count paths from insertion
                if dp.[i, j-1] + 1 = currentDistance then
                    count.[i, j] <- count.[i, j] + count.[i, j-1]
                
                // Count paths from substitution/match
                if dp.[i-1, j-1] + matchCost = currentDistance then
                    count.[i, j] <- count.[i, j] + count.[i-1, j-1]
            else
                count.[i, j] <- 0L
    
    int count.[m, n]

// Alternative cleaner implementation
let countOptimalAlignmentsSimple (s: string) (t: string) : int =
    let m = s.Length
    let n = t.Length
    
    // DP table for edit distances
    let distances = Array2D.create (m + 1) (n + 1) 0
    
    // Initialize base cases
    for i in 0 .. m do distances.[i, 0] <- i
    for j in 0 .. n do distances.[0, j] <- j
    
    // Fill distance table
    for i in 1 .. m do
        for j in 1 .. n do
            let matchCost = if s.[i-1] = t.[j-1] then 0 else 1
            distances.[i, j] <- 
                min (distances.[i-1, j] + 1) 
                    (min (distances.[i, j-1] + 1) 
                        (distances.[i-1, j-1] + matchCost))
    
    // DP table for counting optimal alignments
    let counts = Array2D.create (m + 1) (n + 1) 0L
    
    // Base cases
    counts.[0, 0] <- 1L
    
    for i in 1 .. m do
        counts.[i, 0] <- if distances.[i, 0] = distances.[m, n] then counts.[i-1, 0] else 0L
    for j in 1 .. n do
        counts.[0, j] <- if distances.[0, j] = distances.[m, n] then counts.[0, j-1] else 0L
    
    // Fill counting table
    for i in 1 .. m do
        for j in 1 .. n do
            if distances.[i, j] = distances.[m, n] then
                // From deletion
                if distances.[i-1, j] + 1 = distances.[i, j] then
                    counts.[i, j] <- counts.[i, j] + counts.[i-1, j]
                
                // From insertion
                if distances.[i, j-1] + 1 = distances.[i, j] then
                    counts.[i, j] <- counts.[i, j] + counts.[i, j-1]
                
                // From substitution/match
                if distances.[i-1, j-1] + (if s.[i-1] = t.[j-1] then 0 else 1) = distances.[i, j] then
                    counts.[i, j] <- counts.[i, j] + counts.[i-1, j-1]
    
    int counts.[m, n]

// Example usage
let solveRosalindProblem () =
    let s = "AACCTTGG"
    let t = "ACACTGTGA"
    let result = countOptimalAlignmentsSimple s t
    printfn "Number of optimal alignments: %d" result
    result

// Test with example from Rosalind
let testExample () =
    let s = "ATATC"
    let t = "ATCT"
    let result = countOptimalAlignmentsSimple s t
    printfn "Test case result: %d" result
    result

// Run the test
testExample ()
```

## Explanation

This solution uses dynamic programming to solve the problem:

1. **First DP Table**: Calculate the edit distance matrix using standard dynamic programming for sequence alignment
2. **Second DP Table**: Count the number of ways to achieve the optimal edit distance
3. **Base Cases**: Initialize the counting table with 1 for the starting position
4. **Recurrence Relations**: For each cell, sum up the counts from valid previous positions that could lead to the current optimal distance

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the two DP tables

## Key Insights

1. The problem requires counting optimal alignments, not just computing the edit distance
2. We need to track both the distance and the number of ways to achieve that distance
3. The optimal alignment count is found in the bottom-right cell of the counting table
4. We only count paths that lead to the optimal edit distance, not all possible paths

The solution handles the specific requirements of counting optimal global alignments between two DNA sequences.

