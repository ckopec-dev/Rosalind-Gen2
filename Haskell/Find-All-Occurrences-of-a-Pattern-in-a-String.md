# Rosalind Problem: Find All Occurrences of a Pattern in a String

## Problem Statement
Given two strings `s` and `t`, find all occurrences of `t` as a substring of `s` and return their starting positions (1-indexed).

## Solution in Haskell

```haskell
import Data.List (isPrefixOf)

-- Find all occurrences of a pattern in a string
findAllOccurrences :: String -> String -> [Int]
findAllOccurrences pattern text = 
    let patternLength = length pattern
        textLength = length text
    in if patternLength > textLength
       then []
       else findOccurrences 1 text
  where
    findOccurrences :: Int -> String -> [Int]
    findOccurrences _ [] = []
    findOccurrences pos remaining
        | pattern `isPrefixOf` remaining = pos : findOccurrences (pos + 1) (tail remaining)
        | otherwise = findOccurrences (pos + 1) (tail remaining)

-- Alternative implementation using zipWith and tails
findAllOccurrences' :: String -> String -> [Int]
findAllOccurrences' pattern text = 
    let patternLength = length pattern
        textLength = length text
    in if patternLength > textLength
       then []
       else [i + 1 | (i, window) <- zip [0..] (take (textLength - patternLength + 1) (tails text)),
                      window `isPrefixOf` pattern]
  where
    tails :: [a] -> [[a]]
    tails [] = []
    tails xs = xs : tails (tail xs)

-- Most concise implementation
findAllOccurrences'' :: String -> String -> [Int]
findAllOccurrences'' pattern text = 
    [i + 1 | (i, window) <- zip [0..] (take (length text - length pattern + 1) (tails text)),
             window == pattern]
  where
    tails [] = []
    tails xs = xs : tails (tail xs)

-- Main function for Rosalind submission
solve :: String -> String -> [Int]
solve pattern text = findAllOccurrences pattern text

-- Example usage
main :: IO ()
main = do
    let pattern = "ATAT"
    let text = "GATATATGCATATACTT"
    let result = solve pattern text
    print result  -- Should output [2,4,10]
```

## Explanation

The solution works by:

1. **Length checking**: First, we check if the pattern is longer than the text. If so, return an empty list since no occurrences are possible.

2. **Sliding window approach**: We slide a window of pattern length across the text and check for matches.

3. **1-indexed positions**: Since the problem requires 1-indexed positions, we add 1 to the 0-indexed position.

## Key Functions

- `isPrefixOf`: Checks if one string is a prefix of another
- `tails`: Generates all suffixes of a list
- `zip`: Pairs elements from two lists
- List comprehension: Efficiently filters matching positions

## Time Complexity
- **Time**: O(n × m) where n is the length of text and m is the length of pattern
- **Space**: O(n) for storing the result

## Sample Input/Output
```
Input:
Pattern: "ATAT"
Text: "GATATATGCATATACTT"

Output: [2,4,10]
```

The pattern "ATAT" occurs at positions 2, 4, and 10 (1-indexed) in the text.

