# Rosalind Problem: Find_a_Median_String in Haskell

## Problem Understanding

The median string problem asks us to find a k-mer (a string of length k) that minimizes the total Hamming distance to all strings in a given collection of DNA strings.

## Solution Approach

1. Generate all possible k-mers of length k from the DNA alphabet
2. For each k-mer, calculate the total Hamming distance to all input strings
3. Return the k-mer with minimum total distance

## Haskell Implementation

```haskell
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Generate all possible k-mers of given length
kmers :: Int -> [Char] -> [String]
kmers 0 _ = [""]
kmers k alphabet = [c : rest | c <- alphabet, rest <- kmers (k-1) alphabet]

-- Calculate Hamming distance between two strings
hammingDistance :: String -> String -> Int
hammingDistance xs ys = sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys

-- Calculate total distance from a k-mer to all strings
totalDistance :: String -> [String] -> Int
totalDistance pattern strings = sum $ map (hammingDistance pattern) strings

-- Find median string (k-mer with minimum total distance)
findMedianString :: Int -> [String] -> String
findMedianString k dnaStrings = 
    let allKmers = kmers k "ACGT"
        distances = map (\kmer -> (kmer, totalDistance kmer dnaStrings)) allKmers
        minDistance = minimumBy (comparing snd) distances
    in fst minDistance

-- Alternative implementation with better performance
findMedianStringOptimized :: Int -> [String] -> String
findMedianStringOptimized k dnaStrings = 
    let allKmers = kmers k "ACGT"
        distances = map (totalDistance) allKmers
        minIdx = fst $ minimumBy (comparing snd) $ zip [0..] distances
    in allKmers !! minIdx

-- Main function to solve the problem
solveMedianString :: [String] -> String
solveMedianString input = 
    let k = length $ head input  -- assuming all strings have same length
        dnaStrings = tail input
    in findMedianString k dnaStrings

-- Example usage
exampleInput :: [String]
exampleInput = ["AAATTGACGCAT", "GACGACCAGGTT", "GGACAGATCACG", "TCGTCAGCGACA", "TCGTCAGCGACA"]

-- Test the solution
main :: IO ()
main = do
    let result = solveMedianString exampleInput
    putStrLn $ "Median string: " ++ result
```

## Explanation

### Key Functions:

1. **`kmers k alphabet`**: Generates all possible k-mers using the given alphabet
2. **`hammingDistance xs ys`**: Computes the Hamming distance between two strings
3. **`totalDistance pattern strings`**: Calculates the sum of Hamming distances from a pattern to all input strings
4. **`findMedianString k dnaStrings`**: Finds the k-mer with minimum total distance

### Algorithm Steps:

1. Generate all possible k-mers of length k using nucleotides "ACGT"
2. For each k-mer, compute its total distance to all DNA strings
3. Return the k-mer with the minimum total distance

### Time Complexity:
- O(4^k × n × m) where k is the pattern length, n is the number of strings, and m is the string length
- This is acceptable for small values of k (typically k ≤ 12)

### Space Complexity:
- O(4^k) for storing all possible k-mers

## Sample Input/Output

For the input:
```
AAATTGACGCAT
GACGACCAGGTT
GGACAGATCACG
TCGTCAGCGACA
TCGTCAGCGACA
```

The output would be a 3-mer (assuming k=3) that minimizes the total Hamming distance to all input strings.

This solution efficiently finds the median string by exhaustively searching through all possible k-mers and selecting the one with the minimum total distance to the input DNA strings.

