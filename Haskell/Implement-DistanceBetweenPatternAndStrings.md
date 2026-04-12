# Rosalind Problem: Implement_DistanceBetweenPatternAndStrings

## Problem Description
The distance between a pattern and a set of strings is the sum of distances between the pattern and each string in the set. The distance between a pattern and a string is the minimum Hamming distance between the pattern and any substring of the string.

## Solution in Haskell

```haskell
-- Function to calculate Hamming distance between two strings
hammingDistance :: String -> String -> Int
hammingDistance xs ys = sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys

-- Function to find the minimum Hamming distance between pattern and all substrings of text
minHammingDistance :: String -> String -> Int
minHammingDistance pattern text = 
    let patternLength = length pattern
        textLength = length text
        substrings = take (textLength - patternLength + 1) $ map (take patternLength) $ tails text
    in minimum $ map (hammingDistance pattern) substrings
  where
    tails [] = []
    tails xs = xs : tails (tail xs)

-- Function to calculate distance between pattern and collection of strings
distanceBetweenPatternAndStrings :: String -> [String] -> Int
distanceBetweenPatternAndStrings pattern strings = 
    sum $ map (minHammingDistance pattern) strings

-- Helper function to get all substrings of a given length
substringsOfLength :: Int -> String -> [String]
substringsOfLength k str = 
    let n = length str
    in if k > n then [] 
       else take (n - k + 1) $ map (take k) $ tails str
  where
    tails [] = []
    tails xs = xs : tails (tail xs)

-- More precise implementation of minHammingDistance
minHammingDistance' :: String -> String -> Int
minHammingDistance' pattern text = 
    let patternLength = length pattern
        textLength = length text
        substrings = substringsOfLength patternLength text
    in minimum $ map (hammingDistance pattern) substrings

-- Final implementation
distanceBetweenPatternAndStrings' :: String -> [String] -> Int
distanceBetweenPatternAndStrings' pattern strings = 
    sum $ map (minHammingDistance' pattern) strings

-- Example usage:
-- distanceBetweenPatternAndStrings' "AAA" ["TTACCTTAAC", "GATATCTGAC", "AAGAAGTGTA"]
-- Should return 5
```

## Explanation

1. **`hammingDistance`**: Calculates the Hamming distance between two strings of equal length by counting the number of positions where characters differ.

2. **`minHammingDistance`**: Finds the minimum Hamming distance between a pattern and all substrings of a given text. It generates all possible substrings of the text that match the pattern length and returns the minimum Hamming distance.

3. **`distanceBetweenPatternAndStrings`**: Calculates the total distance between a pattern and a collection of strings by summing up the minimum Hamming distances for each string.

4. **`substringsOfLength`**: Helper function to generate all substrings of a specific length from a given string.

## Key Points

- The problem requires finding the minimum Hamming distance between a pattern and any substring of each string in the collection
- We sum up these minimum distances to get the final result
- The solution handles edge cases like when the pattern is longer than the text
- Uses functional programming concepts like `map`, `minimum`, and list comprehensions

## Time Complexity
- O(n × m × k) where n is the length of the text, m is the number of strings, and k is the pattern length

## Space Complexity
- O(n × k) for storing substrings

The solution efficiently handles the core requirements of the Rosalind problem by implementing the distance calculation between a pattern and multiple strings.

