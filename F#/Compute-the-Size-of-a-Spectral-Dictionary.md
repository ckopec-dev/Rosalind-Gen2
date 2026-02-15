# Rosalind Problem: Compute the Size of a Spectral Dictionary

## Problem Statement
Given a string `s` and an integer `k`, we want to compute the size of the spectral dictionary of `s` with respect to `k`.

The spectral dictionary of a string `s` with respect to `k` is the set of all substrings of `s` of length `k`, where each substring is considered as a "spectral" representation.

## Solution in F#

```fsharp
let computeSpectralDictionarySize (s: string) (k: int) : int =
    if k <= 0 || s.Length < k then
        0
    else
        s.Length - k + 1

// Alternative implementation using Seq.windowed
let computeSpectralDictionarySizeAlt (s: string) (k: int) : int =
    if k <= 0 || s.Length < k then
        0
    else
        s
        |> Seq.windowed k
        |> Seq.length

// Example usage
let example1 = computeSpectralDictionarySize "ABCD" 2  // Returns 3
let example2 = computeSpectralDictionarySize "ABCD" 3  // Returns 2
let example3 = computeSpectralDictionarySize "ABCD" 4  // Returns 1
let example4 = computeSpectralDictionarySize "ABCD" 5  // Returns 0
```

## Explanation

The problem asks us to compute how many substrings of length `k` can be extracted from a string `s`.

For a string of length `n` and substring length `k`:
- The first substring starts at index 0
- The last substring starts at index `n - k`
- Therefore, there are `n - k + 1` possible substrings

## Key Points

1. **Edge Cases**: 
   - If `k <= 0`, return 0
   - If `k > s.Length`, return 0

2. **Formula**: For a string of length `n` and pattern length `k`, the number of substrings is `n - k + 1`

3. **Time Complexity**: O(1) - constant time calculation
4. **Space Complexity**: O(1) - no additional space needed

## Test Cases

```fsharp
// Test the function with various inputs
let testCases = [
    ("ABCD", 2, 3)
    ("ABCD", 3, 2)
    ("ABCD", 4, 1)
    ("ABCD", 5, 0)
    ("", 1, 0)
    ("A", 1, 1)
    ("ABC", 1, 3)
]

let runTests () =
    testCases |> List.iter (fun (s, k, expected) ->
        let result = computeSpectralDictionarySize s k
        printfn "Input: s=\"%s\", k=%d, Expected: %d, Got: %d" s k expected result
    )
```

The solution is straightforward since we're just counting how many substrings of length `k` can be formed from a string of length `n`, which is simply `n - k + 1`.

