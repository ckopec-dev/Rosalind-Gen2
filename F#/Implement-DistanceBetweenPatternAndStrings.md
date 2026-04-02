# Rosalind Problem: Implement DistanceBetweenPatternAndStrings

## Problem Description
The distance between a pattern and a set of strings is the sum of the distances between the pattern and each string in the set. The distance between a pattern and a string is the minimum Hamming distance between the pattern and any substring of the string.

## Solution in F#

```fsharp
// Function to calculate Hamming distance between two strings
let hammingDistance (pattern: string) (text: string) : int =
    let rec hammingHelper i count =
        if i >= pattern.Length then count
        else
            if pattern.[i] <> text.[i] then
                hammingHelper (i + 1) (count + 1)
            else
                hammingHelper (i + 1) count
    hammingHelper 0 0

// Function to find the minimum Hamming distance between pattern and all substrings of text
let minHammingDistance (pattern: string) (text: string) : int =
    let patternLength = pattern.Length
    let textLength = text.Length
    
    if patternLength > textLength then
        failwith "Pattern length cannot be greater than text length"
    
    let rec findMin i minDist =
        if i > textLength - patternLength then
            minDist
        else
            let substring = text.Substring(i, patternLength)
            let currentDist = hammingDistance pattern substring
            if currentDist < minDist then
                findMin (i + 1) currentDist
            else
                findMin (i + 1) minDist
    
    findMin 0 (textLength - patternLength + 1) // Initial large value

// Main function to calculate distance between pattern and set of strings
let distanceBetweenPatternAndStrings (pattern: string) (dna: string[]) : int =
    let strings = dna |> Array.toList
    let rec calculateDistance strings totalDistance =
        match strings with
        | [] -> totalDistance
        | head :: tail ->
            let minDist = minHammingDistance pattern head
            calculateDistance tail (totalDistance + minDist)
    
    calculateDistance strings 0

// Alternative implementation using List.sumBy
let distanceBetweenPatternAndStringsAlt (pattern: string) (dna: string[]) : int =
    dna
    |> Array.map (fun text -> minHammingDistance pattern text)
    |> Array.sum
```

## Example Usage

```fsharp
// Example usage
let pattern = "AAA"
let dna = [| "TTACCTTAAC"; "GATATCTGTC"; "ACGGCGTTCG"; "CCCTAAAGAG" |]

let result = distanceBetweenPatternAndStrings pattern dna
printfn "Distance: %d" result

// Expected output: 5
```

## Explanation

1. **hammingDistance**: Calculates the Hamming distance between two strings of equal length by counting the number of positions where characters differ.

2. **minHammingDistance**: Finds the minimum Hamming distance between the pattern and all possible substrings of the given text. It slides a window of pattern length across the text and calculates the Hamming distance for each substring.

3. **distanceBetweenPatternAndStrings**: Calculates the total distance by summing up the minimum Hamming distances between the pattern and each string in the DNA set.

## Time Complexity
- Hamming distance calculation: O(pattern_length)
- Finding minimum distance for one string: O(text_length × pattern_length)
- Overall: O(n × m × k) where n is the number of strings, m is the average string length, and k is the pattern length

## Space Complexity
O(1) additional space (excluding input storage)

