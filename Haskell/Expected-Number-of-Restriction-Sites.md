# Rosalind Problem: Expected Number of Restriction Sites

## Problem Understanding

We need to calculate the expected number of restriction sites of a given length in a DNA sequence with a specific GC content.

## Solution Approach

1. Calculate the probability of each nucleotide based on GC content
2. Calculate the probability of a specific restriction site pattern
3. Calculate the expected number of occurrences

## Haskell Implementation

```haskell
-- Expected Number of Restriction Sites
-- Rosalind problem solution

module Main where

import Data.List (foldl')

-- Calculate probability of a nucleotide given GC content
-- For a given GC content, probability of G and C is each GC/2
-- Probability of A and T is each (1-GC)/2
nucleotideProb :: Double -> Char -> Double
nucleotideProb gc 'A' = (1 - gc) / 2
nucleotideProb gc 'T' = (1 - gc) / 2
nucleotideProb gc 'G' = gc / 2
nucleotideProb gc 'C' = gc / 2
nucleotideProb _ _ = 0.0

-- Calculate probability of a restriction site pattern
-- A restriction site pattern is a string of nucleotides
siteProb :: Double -> String -> Double
siteProb gc pattern = product [nucleotideProb gc nucleotide | nucleotide <- pattern]

-- Calculate expected number of restriction sites
-- Expected value = (length of sequence - length of pattern + 1) * probability of pattern
expectedSites :: Int -> Double -> String -> Double
expectedSites seqLength gc pattern = 
    let patternLength = length pattern
        numPositions = seqLength - patternLength + 1
        prob = siteProb gc pattern
    in fromIntegral numPositions * prob

-- Main function to solve the problem
solve :: Int -> Double -> String -> Double
solve seqLength gc pattern = expectedSites seqLength gc pattern

-- Example usage
main :: IO ()
main = do
    -- Example with sequence length 100, GC content 0.5, pattern "ATCG"
    let result = solve 100 0.5 "ATCG"
    print result
    
    -- Test with different parameters
    let result2 = solve 1000 0.3 "GC"
    print result2
```

## Alternative Implementation with More Detailed Approach

```haskell
-- More detailed implementation with explicit calculation steps

module RestrictionSites where

-- Calculate probability of a specific nucleotide given GC content
probOfNucleotide :: Double -> Char -> Double
probOfNucleotide gc 'A' = (1 - gc) / 2
probOfNucleotide gc 'T' = (1 - gc) / 2
probOfNucleotide gc 'G' = gc / 2
probOfNucleotide gc 'C' = gc / 2
probOfNucleotide _ _ = 0.0

-- Calculate probability of a restriction site pattern
-- Multiply probabilities of each nucleotide in the pattern
patternProbability :: Double -> String -> Double
patternProbability gc pattern = 
    foldl' (\acc nucleotide -> acc * probOfNucleotide gc nucleotide) 1.0 pattern

-- Calculate expected number of restriction sites
-- For a sequence of length n and pattern of length k, there are (n-k+1) possible positions
-- Expected value = number of positions * probability of pattern at each position
expectedRestrictionSites :: Int -> Double -> String -> Double
expectedRestrictionSites sequenceLength gc pattern = 
    let patternLength = length pattern
        positions = sequenceLength - patternLength + 1
        prob = patternProbability gc pattern
    in fromIntegral positions * prob

-- Example usage
example :: IO ()
example = do
    -- Example from Rosalind problem
    let seqLength = 100
    let gcContent = 0.5
    let restrictionPattern = "ATCG"
    
    let result = expectedRestrictionSites seqLength gcContent restrictionPattern
    putStrLn $ "Expected number of restriction sites: " ++ show result
```

## Sample Input/Output

For the input:
- Sequence length: 100
- GC content: 0.5  
- Restriction pattern: "ATCG"

The expected number of restriction sites would be calculated as:
1. Probability of "ATCG" = 0.25 × 0.25 × 0.25 × 0.25 = 0.00390625
2. Number of positions = 100 - 4 + 1 = 97
3. Expected sites = 97 × 0.00390625 = 0.37890625

## Key Concepts

1. **Probability Calculation**: Each nucleotide's probability depends on the GC content
2. **Pattern Probability**: Multiply individual nucleotide probabilities
3. **Expected Value**: Number of possible positions × probability of pattern
4. **Edge Cases**: Handle cases where pattern length exceeds sequence length

This solution efficiently calculates the expected number of restriction sites using probability theory and combinatorics.

