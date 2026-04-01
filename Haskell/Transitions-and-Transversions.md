# Rosalind Problem: Transitions and Transversions in Haskell

## Problem Understanding

Given two DNA strings of equal length, we need to count:
- **Transitions**: Purine ↔ Purine (A ↔ G) or Pyrimidine ↔ Pyrimidine (C ↔ T)
- **Transversions**: Purine ↔ Pyrimidine (A ↔ C, A ↔ T, G ↔ C, G ↔ T)

## Solution Approach

1. Compare corresponding nucleotides in both strings
2. Count transitions and transversions based on nucleotide pairs
3. Return the ratio of transitions to transversions

## Haskell Implementation

```haskell
module TransitionsAndTransversions where

import Data.List (foldl')

-- Define nucleotide types
data Nucleotide = A | C | G | T deriving (Eq, Show)

-- Convert Char to Nucleotide
charToNucleotide :: Char -> Nucleotide
charToNucleotide 'A' = A
charToNucleotide 'C' = C
charToNucleotide 'G' = G
charToNucleotide 'T' = T
charToNucleotide _   = error "Invalid nucleotide"

-- Convert Nucleotide to Char
nucleotideToChar :: Nucleotide -> Char
nucleotideToChar A = 'A'
nucleotideToChar C = 'C'
nucleotideToChar G = 'G'
nucleotideToChar T = 'T'

-- Check if two nucleotides are transitions
isTransition :: Nucleotide -> Nucleotide -> Bool
isTransition A G = True  -- A ↔ G
isTransition G A = True  -- G ↔ A
isTransition C T = True  -- C ↔ T
isTransition T C = True  -- T ↔ C
isTransition _ _ = False

-- Check if two nucleotides are transversions
isTransversion :: Nucleotide -> Nucleotide -> Bool
isTransversion x y = not (isTransition x y) && x /= y

-- Count transitions and transversions between two DNA strings
countVariations :: String -> String -> (Int, Int)
countVariations xs ys = foldl' countPair (0, 0) (zip xs ys)
  where
    countPair (transitions, transversions) (x, y) =
      let nx = charToNucleotide x
          ny = charToNucleotide y
      in if nx == ny
         then (transitions, transversions)
         else if isTransition nx ny
              then (transitions + 1, transversions)
              else (transitions, transversions + 1)

-- Main function to solve the problem
transitionsAndTransversions :: String -> String -> Double
transitionsAndTransversions xs ys = 
  let (transitions, transversions) = countVariations xs ys
  in if transversions == 0
     then 0
     else fromIntegral transitions / fromIntegral transversions

-- Alternative implementation using explicit pattern matching
countVariations' :: String -> String -> (Int, Int)
countVariations' xs ys = 
  let pairs = zip xs ys
      count (trans, transv) (x, y) = 
        case (charToNucleotide x, charToNucleotide y) of
          (A, G) -> (trans + 1, transv)
          (G, A) -> (trans + 1, transv)
          (C, T) -> (trans + 1, transv)
          (T, C) -> (trans + 1, transv)
          (A, T) -> (trans, transv + 1)
          (T, A) -> (trans, transv + 1)
          (A, C) -> (trans, transv + 1)
          (C, A) -> (trans, transv + 1)
          (G, T) -> (trans, transv + 1)
          (T, G) -> (trans, transv + 1)
          (G, C) -> (trans, transv + 1)
          (C, G) -> (trans, transv + 1)
          _     -> (trans, transv)
  in foldl' count (0, 0) pairs

-- Example usage
example :: IO ()
example = do
  let seq1 = "AAAACCCGGT"
      seq2 = "AAAACCCGGT"
  print $ transitionsAndTransversions seq1 seq2
  
  let seq3 = "GCAACGCCTG"
      seq4 = "GGAAAACCTT"
  print $ transitionsAndTransversions seq3 seq4

-- More concise version using case expressions
transitionsAndTransversions' :: String -> String -> Double
transitionsAndTransversions' xs ys = 
  let (trans, transv) = foldl' countPair (0, 0) (zip xs ys)
  in if transv == 0 then 0 else fromIntegral trans / fromIntegral transv
  where
    countPair (t, tv) (x, y) = 
      case (charToNucleotide x, charToNucleotide y) of
        (A, G) -> (t + 1, tv)
        (G, A) -> (t + 1, tv)
        (C, T) -> (t + 1, tv)
        (T, C) -> (t + 1, tv)
        _      -> (t, tv + 1)
```

## Usage Example

```haskell
-- Example from Rosalind problem
main :: IO ()
main = do
  let sequence1 = "GCAACGCCTG"
      sequence2 = "GGAAAACCTT"
  putStrLn $ "Transition/Transversion ratio: " ++ 
             show (transitionsAndTransversions sequence1 sequence2)
```

## Key Features

1. **Type Safety**: Uses custom `Nucleotide` data type for clear representation
2. **Efficiency**: Uses `foldl'` for strict evaluation and better performance
3. **Error Handling**: Proper handling of invalid nucleotides
4. **Clean Logic**: Separates transition/transversion detection from counting
5. **Flexible**: Can be easily adapted for different input formats

## Time Complexity
- **Time**: O(n) where n is the length of the DNA strings
- **Space**: O(1) additional space

This solution correctly handles the Rosalind problem requirements for counting transitions and transversions between DNA sequences.

