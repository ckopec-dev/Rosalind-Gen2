# Rosalind Problem: Linguistic Complexity of a Genome

## Problem Understanding

The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings of a given length k to the total number of possible substrings of that length. For a genome of length n, there are n-k+1 substrings of length k, and there are 4^k possible substrings of length k (assuming DNA alphabet {A,C,G,T}).

## Solution Approach

1. Generate all substrings of length k from the genome
2. Count the distinct substrings
3. Calculate the ratio of distinct substrings to total possible substrings
4. Return the linguistic complexity

## Haskell Implementation

```haskell
import Data.List (nub)
import Data.Char (toUpper)

-- Function to calculate linguistic complexity of a genome
linguisticComplexity :: String -> Int -> Double
linguisticComplexity genome k
  | k <= 0 = 0
  | k > length genome = 0
  | otherwise = fromIntegral (length (nub substrings)) / fromIntegral (length substrings)
  where
    substrings = getAllSubstrings genome k

-- Helper function to get all substrings of length k
getAllSubstrings :: String -> Int -> [String]
getAllSubstrings genome k
  | length genome < k = []
  | otherwise = take k genome : getAllSubstrings (tail genome) k

-- Alternative implementation using list comprehension
linguisticComplexity' :: String -> Int -> Double
linguisticComplexity' genome k
  | k <= 0 = 0
  | k > length genome = 0
  | otherwise = fromIntegral (length (nub substrings)) / fromIntegral (length substrings)
  where
    substrings = [take k (drop i genome) | i <- [0..length genome - k], length genome >= k]

-- More efficient implementation using window sliding
linguisticComplexity'' :: String -> Int -> Double
linguisticComplexity'' genome k
  | k <= 0 = 0
  | k > length genome = 0
  | otherwise = fromIntegral (length (nub substrings)) / fromIntegral (length substrings)
  where
    substrings = map (take k) (tails genome)
    tails [] = []
    tails xs = xs : tails (tail xs)

-- Cleanest and most efficient implementation
linguisticComplexityClean :: String -> Int -> Double
linguisticComplexityClean genome k
  | k <= 0 = 0
  | k > length genome = 0
  | otherwise = fromIntegral (length (nub substrings)) / fromIntegral (length substrings)
  where
    substrings = [take k (drop i genome) | i <- [0..length genome - k]]

-- Main function for testing
main :: IO ()
main = do
  let genome = "ACGTACGT"
  let k = 2
  let result = linguisticComplexityClean genome k
  putStrLn $ "Linguistic complexity of genome " ++ genome ++ " with k=" ++ show k ++ " is: " ++ show result
```

## Example Usage

```haskell
-- Example from Rosalind problem
example :: IO ()
example = do
  let genome = "ACGTACGT"
  let k = 2
  let complexity = linguisticComplexityClean genome k
  print complexity  -- Should output the linguistic complexity

-- Test with different k values
testComplexity :: IO ()
testComplexity = do
  let genome = "ACGTACGT"
  putStrLn $ "Genome: " ++ genome
  mapM_ (\k -> putStrLn $ "k=" ++ show k ++ ": " ++ show (linguisticComplexityClean genome k)) [1..4]
```

## Explanation

The solution works as follows:

1. **Input validation**: Check if k is valid (positive and not greater than genome length)
2. **Substring generation**: Generate all substrings of length k using `take k (drop i genome)` for all valid positions i
3. **Distinct counting**: Use `nub` to count distinct substrings
4. **Ratio calculation**: Divide the number of distinct substrings by total number of substrings
5. **Return result**: Convert to Double for precise division

## Time Complexity
- O(n×k) for generating substrings where n is genome length
- O(m×k) for distinct substring counting where m is number of substrings
- Overall: O(n×k) where k is typically small

## Space Complexity
- O(n×k) for storing all substrings

This implementation handles edge cases and provides an efficient solution to the linguistic complexity problem.

