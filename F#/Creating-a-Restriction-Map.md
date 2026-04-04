# Rosalind Problem: Creating a Restriction Map (F# Solution)

## Problem Understanding

The problem asks us to reconstruct the positions of restriction sites on a DNA sequence given the lengths of fragments produced by cutting the sequence with a restriction enzyme.

## Solution Approach

We need to:
1. Find all possible combinations of fragment lengths that sum to the total sequence length
2. For each combination, determine the positions of cuts
3. Validate that the resulting map matches the given fragment lengths

## F# Implementation

```fsharp
open System
open System.Collections.Generic

let rec permutations list =
    match list with
    | [] -> [[]]
    | head :: tail ->
        let perms = permutations tail
        [ for perm in perms do
            for i in 0 .. perm.Length do
                yield List.insert i head perm ]

let rec combinations list n =
    match n, list with
    | 0, _ -> [[]]
    | _, [] -> []
    | _, head :: tail ->
        let withHead = combinations tail (n - 1) |> List.map (fun c -> head :: c)
        let withoutHead = combinations tail n
        withHead @ withoutHead

let rec sum list =
    match list with
    | [] -> 0
    | head :: tail -> head + sum tail

let rec allSplits total lengthList =
    match lengthList with
    | [] -> [ [] ]
    | head :: tail ->
        let remaining = total - head
        if remaining < 0 then []
        else
            let subSplits = allSplits remaining tail
            [ for split in subSplits -> head :: split ]

let rec findPositions lengths =
    match lengths with
    | [] -> []
    | head :: tail ->
        let positions = List.scan (+) 0 lengths
        positions |> List.tail

let rec isValidMap sequence fragments =
    let totalLength = sequence.Length
    let fragmentSum = fragments |> List.sum
    if fragmentSum <> totalLength then false
    else
        let positions = findPositions fragments
        let cutPositions = positions |> List.map (fun x -> x - 1)
        // Check if all cuts are valid (within sequence bounds)
        cutPositions |> List.forall (fun pos -> pos >= 0 && pos < sequence.Length)

let createRestrictionMap sequence fragmentLengths =
    let totalLength = sequence.Length
    let fragmentSum = fragmentLengths |> List.sum
    
    if fragmentSum <> totalLength then
        None
    else
        // Generate all possible arrangements of fragments
        let fragmentPermutations = permutations fragmentLengths
        let validMaps = 
            fragmentPermutations 
            |> List.filter (fun frags -> 
                let positions = findPositions frags
                let cutPositions = positions |> List.map (fun x -> x - 1)
                cutPositions |> List.forall (fun pos -> pos >= 0 && pos < totalLength))
        
        if validMaps.IsEmpty then None
        else
            Some (List.head validMaps)

// More efficient approach for the specific problem
let solveRestrictionMap sequence fragmentLengths =
    let totalLength = sequence.Length
    let fragmentSum = fragmentLengths |> List.sum
    
    if fragmentSum <> totalLength then
        []
    else
        // The restriction map consists of the positions where cuts occur
        // For fragments of lengths [a, b, c], the cut positions are [a, a+b, a+b+c]
        let cutPositions = fragmentLengths |> List.scan (+) 0 |> List.tail
        cutPositions

// Alternative implementation using dynamic programming approach
let createRestrictionMapDP sequence fragmentLengths =
    let totalLength = sequence.Length
    let fragmentSum = fragmentLengths |> List.sum
    
    if fragmentSum <> totalLength then
        []
    else
        // Create the restriction map by calculating cumulative positions
        let rec buildMap lengths acc =
            match lengths with
            | [] -> acc
            | head :: tail ->
                let newAcc = if acc.IsEmpty then [head] else acc @ [head + acc.[acc.Length - 1]]
                buildMap tail newAcc
        
        buildMap fragmentLengths []

// Main function to solve the problem
let main sequence fragmentLengths =
    let result = createRestrictionMapDP sequence fragmentLengths
    result

// Example usage
let exampleSequence = "AACCGGTT"
let exampleFragments = [2; 2; 2; 2]

let solution = main exampleSequence exampleFragments
printfn "Restriction map: %A" solution

// More complex example
let sequence2 = "AACCGGTTAACCGGTT"
let fragments2 = [4; 4; 4; 4]

let solution2 = main sequence2 fragments2
printfn "Restriction map: %A" solution2
```

## Alternative Cleaner Implementation

```fsharp
open System

// Function to compute restriction map positions
let createRestrictionMap sequence fragmentLengths =
    let totalLength = sequence.Length
    let fragmentSum = fragmentLengths |> List.sum
    
    // Validate input
    if fragmentSum <> totalLength then
        failwith "Fragment lengths don't sum to sequence length"
    
    // Calculate cut positions (cumulative sum of fragment lengths)
    let cumulativeLengths = 
        fragmentLengths 
        |> List.scan (+) 0 
        |> List.tail
    
    cumulativeLengths

// Helper function to verify a solution
let verifyRestrictionMap sequence fragmentLengths positions =
    let totalLength = sequence.Length
    let fragmentSum = fragmentLengths |> List.sum
    
    if fragmentSum <> totalLength then
        false
    else
        // Check if positions are valid
        let validPositions = positions |> List.forall (fun pos -> pos > 0 && pos < totalLength)
        validPositions

// Example usage
let sequence = "AACCGGTT"
let fragments = [2; 2; 2; 2]

let map = createRestrictionMap sequence fragments
printfn "Restriction map positions: %A" map

// For a more complex example:
let sequence2 = "ATCGATCGATCG"
let fragments2 = [3; 3; 3; 3]

let map2 = createRestrictionMap sequence2 fragments2
printfn "Restriction map positions: %A" map2
```

## Key Points

1. **Input Validation**: Check that the sum of fragment lengths equals the sequence length
2. **Position Calculation**: Cut positions are cumulative sums of fragment lengths
3. **Output Format**: Return the positions where cuts occur (0-indexed)
4. **Edge Cases**: Handle empty sequences and invalid fragment combinations

## Time Complexity
- O(n) where n is the number of fragments
- Space complexity: O(n) for storing the result

This solution efficiently computes the restriction map by calculating the cumulative positions where cuts occur in the DNA sequence.

