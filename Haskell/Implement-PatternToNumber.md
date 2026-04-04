# Rosalind Problem: Implement PatternToNumber in Haskell

## Problem Description
The goal is to implement a function that converts a DNA pattern (string of nucleotides) into its lexicographically ordered index in the list of all possible k-mers sorted lexicographically.

## Solution

```haskell
-- Function to convert a DNA pattern to its lexicographically ordered index
patternToNumber :: String -> Int
patternToNumber = patternToNumberHelper 0
  where
    patternToNumberHelper :: Int -> String -> Int
    patternToNumberHelper acc [] = acc
    patternToNumberHelper acc (x:xs) = 
      let value = case x of
            'A' -> 0
            'C' -> 1
            'G' -> 2
            'T' -> 3
            _   -> error "Invalid DNA nucleotide"
          remaining = patternToNumberHelper acc xs
      in value * (4 ^ length xs) + remaining

-- Alternative implementation using fold
patternToNumber' :: String -> Int
patternToNumber' = foldl (\acc x -> 
  case x of
    'A' -> acc * 4 + 0
    'C' -> acc * 4 + 1
    'G' -> acc * 4 + 2
    'T' -> acc * 4 + 3
    _   -> error "Invalid DNA nucleotide"
  ) 0

-- More readable version with explicit length calculation
patternToNumber'' :: String -> Int
patternToNumber'' pattern = 
  let k = length pattern
  in go 0 0
  where
    go :: Int -> Int -> Int
    go index i
      | i >= k = 0
      | otherwise = 
          let value = case pattern !! i of
                'A' -> 0
                'C' -> 1
                'G' -> 2
                'T' -> 3
                _   -> error "Invalid DNA nucleotide"
              remaining = go (index + 1) (i + 1)
          in value * (4 ^ (k - i - 1)) + remaining
```

## Example Usage

```haskell
-- Examples:
-- patternToNumber "A"     -> 0
-- patternToNumber "C"     -> 1  
-- patternToNumber "G"     -> 2
-- patternToNumber "T"     -> 3
-- patternToNumber "AA"    -> 0
-- patternToNumber "AC"    -> 1
-- patternToNumber "AG"    -> 2
-- patternToNumber "AT"    -> 3
-- patternToNumber "CA"    -> 4
-- patternToNumber "TT"    -> 15
-- patternToNumber "ACGT"  -> 27
```

## Explanation

The algorithm works by treating the DNA pattern as a base-4 number where:
- A = 0
- C = 1  
- G = 2
- T = 3

For a k-mer of length k, each position contributes a value based on its position and the digit value:
- First position (most significant): digit × 4^(k-1)
- Second position: digit × 4^(k-2)
- And so on...

This is essentially converting from a base-4 representation to decimal, where the order of digits determines the final value.

The first implementation (`patternToNumber`) is the most efficient and readable, using recursion with an accumulator to build up the result from left to right.

