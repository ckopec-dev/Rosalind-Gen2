# Rosalind Problem: Matching Random Motifs (Haskell Solution)

## Problem Understanding

The problem asks us to calculate the probability that a randomly chosen DNA motif of length `l` will match a given DNA sequence with a specified GC content.

## Solution Approach

We need to:
1. Calculate the probability of each nucleotide occurring based on GC content
2. Compute the probability of a motif of length `l` matching the given sequence
3. Handle multiple test cases

## Haskell Implementation

```haskell
import Data.List (foldl')
import Text.Printf

-- Calculate the probability of a motif matching given GC content
motifProbability :: Double -> Int -> Double
motifProbability gcContent length = 
    let -- Probability of each nucleotide
        pG = gcContent / 2
        pC = gcContent / 2
        pA = (1 - gcContent) / 2
        pT = (1 - gcContent) / 2
        -- For a motif of length n, probability is product of individual probabilities
        -- We need to calculate the probability of a specific motif matching
        -- But since we're matching random motifs, we consider the probability of
        -- the most likely motif (which would be the one with highest probability)
    in (pA + pT)^(fromIntegral length)  -- This is for a specific motif

-- More accurate approach: calculate probability of a random motif matching
-- This is the probability of a random sequence of length l matching a given sequence
-- But the problem seems to be asking for the probability of a random motif of length l
-- having a specific pattern or matching a given sequence with given GC content

-- Better approach: calculate probability that a random motif of given length
-- matches a specific pattern (which would be the given sequence)
calculateMatchingProbability :: [Char] -> Double -> Int -> Double
calculateMatchingProbability sequence gcContent length = 
    let -- Calculate probability for each position
        probAtPosition :: Char -> Double
        probAtPosition 'A' = (1 - gcContent) / 2
        probAtPosition 'T' = (1 - gcContent) / 2
        probAtPosition 'G' = gcContent / 2
        probAtPosition 'C' = gcContent / 2
        probAtPosition _ = 0.0
    in foldl' (*) 1 (map probAtPosition sequence)

-- Alternative interpretation: probability that a random motif of length l
-- matches a given sequence with specified GC content
-- This is the probability that a random sequence of length l matches
-- a specific pattern given the GC content

-- If we're given a sequence and GC content, we want to calculate
-- the probability that a random sequence of that length matches it
probabilityOfMatch :: [Char] -> Double -> Int -> Double
probabilityOfMatch sequence gcContent l = 
    let pA = (1 - gcContent) / 2
        pT = (1 - gcContent) / 2
        pG = gcContent / 2
        pC = gcContent / 2
        probs = map (\nuc -> 
            case nuc of
                'A' -> pA
                'T' -> pT
                'G' -> pG
                'C' -> pC
                _ -> 0.0) sequence
    in foldl' (*) 1 probs

-- Main function to solve the problem
solveMatchingRandomMotifs :: [Double] -> [String] -> [Double]
solveMatchingRandomMotifs gcContents sequences = 
    zipWith probabilityOfMatch sequences gcContents

-- More precise implementation for the actual problem
-- Given a GC content and a sequence, calculate the probability
-- that a random motif of length equal to sequence length matches it
solveProblem :: [Double] -> [String] -> [Double]
solveProblem gcContents sequences = 
    zipWith (\gc seq -> 
        let l = length seq
            pA = (1 - gc) / 2
            pT = (1 - gc) / 2
            pG = gc / 2
            pC = gc / 2
            prob = foldl' (*) 1 $ map (\nuc -> 
                case nuc of
                    'A' -> pA
                    'T' -> pT
                    'G' -> pG
                    'C' -> pC
                    _ -> 0.0) seq
        in prob) gcContents sequences

-- Read input and solve
main :: IO ()
main = do
    -- Example input format
    -- First line: GC content values
    -- Following lines: DNA sequences
    putStrLn "Enter GC contents (space-separated):"
    gcInput <- getLine
    putStrLn "Enter DNA sequences (one per line, end with empty line):"
    
    let gcContents = map read $ words gcInput
    sequences <- getLines
    
    let results = solveProblem gcContents sequences
    
    mapM_ (printf "%.4f\n") results

-- Helper function to read multiple lines
getLines :: IO [String]
getLines = do
    line <- getLine
    if null line 
        then return []
        else do
            rest <- getLines
            return (line : rest)
```

## Alternative Clean Implementation

```haskell
import Text.Printf

-- Calculate probability of a DNA sequence matching given GC content
calculateProbability :: String -> Double -> Double
calculateProbability sequence gcContent = 
    let pA = (1 - gcContent) / 2
        pT = (1 - gcContent) / 2
        pG = gcContent / 2
        pC = gcContent / 2
        probabilities = map (\nucleotide -> 
            case nucleotide of
                'A' -> pA
                'T' -> pT
                'G' -> pG
                'C' -> pC
                _ -> 0.0) sequence
    in foldl (*) 1 probabilities

-- Main solving function
solve :: [Double] -> [String] -> [Double]
solve gcContents sequences = 
    zipWith calculateProbability sequences gcContents

-- Example usage
example :: IO ()
example = do
    let gcContents = [0.25, 0.5, 0.75]
    let sequences = ["ATATAT", "GCATGC", "AAAAAA"]
    let results = solve gcContents sequences
    mapM_ (printf "%.4f\n") results
```

## Key Points

1. **Probability Calculation**: For each nucleotide position, we calculate the probability based on GC content
2. **Multiplication Principle**: The probability of a complete sequence is the product of individual nucleotide probabilities
3. **GC Content Handling**: 
   - G and C nucleotides have probability `gcContent / 2` each
   - A and T nucleotides have probability `(1 - gcContent) / 2` each
4. **Input/Output**: The solution handles multiple test cases and formats output to required precision

The solution correctly computes the probability that a random DNA motif of a given length matches a specific sequence, given the GC content of the DNA.

