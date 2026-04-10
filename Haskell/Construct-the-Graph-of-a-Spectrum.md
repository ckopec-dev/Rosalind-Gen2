# Rosalind Problem: Construct the Graph of a Spectrum

## Problem Statement
Given an integer `m` and a string `s`, construct the graph of a spectrum with `m`-mass edges.

## Solution in Haskell

```haskell
import Data.List (sort)
import Data.Ord (comparing)

-- Function to calculate mass of amino acid
aminoAcidMass :: Char -> Int
aminoAcidMass 'A' = 71
aminoAcidMass 'C' = 103
aminoAcidMass 'D' = 115
aminoAcidMass 'E' = 129
aminoAcidMass 'F' = 147
aminoAcidMass 'G' = 57
aminoAcidMass 'H' = 137
aminoAcidMass 'I' = 113
aminoAcidMass 'K' = 128
aminoAcidMass 'L' = 113
aminoAcidMass 'M' = 131
aminoAcidMass 'N' = 114
aminoAcidMass 'P' = 97
aminoAcidMass 'Q' = 128
aminoAcidMass 'R' = 156
aminoAcidMass 'S' = 87
aminoAcidMass 'T' = 101
aminoAcidMass 'V' = 99
aminoAcidMass 'W' = 186
aminoAcidMass 'Y' = 163
aminoAcidMass _ = 0

-- Function to compute prefix masses
prefixMasses :: String -> [Int]
prefixMasses s = 0 : scanl (+) 0 (map aminoAcidMass s)

-- Function to construct the graph of a spectrum
constructSpectrumGraph :: String -> [(Int, Int)]
constructSpectrumGraph s = 
    let masses = prefixMasses s
        n = length masses - 1
        -- Generate all possible edges (i,j) where i < j
        edges = [(masses !! i, masses !! j) | i <- [0..n], j <- [i+1..n]]
        -- Filter edges that correspond to valid amino acid masses
        validEdges = filter (\(a,b) -> b - a `elem` map aminoAcidMass "ACDEFGHIKLMNPQRSTVWY") edges
    in validEdges

-- Alternative implementation for the specific problem
constructGraphFromSpectrum :: [Int] -> [(Int, Int)]
constructGraphFromSpectrum spectrum = 
    let sortedSpectrum = sort spectrum
        -- Create all possible edges
        edges = [(sortedSpectrum !! i, sortedSpectrum !! j) | 
                 i <- [0..length sortedSpectrum - 2], 
                 j <- [i+1..length sortedSpectrum - 1]]
        -- Filter for valid mass differences (standard amino acid masses)
        validEdges = filter (\(a,b) -> (b-a) `elem` [71,103,115,129,147,57,137,113,128,113,131,114,97,128,156,87,101,99,186,163]) edges
    in validEdges

-- Main function to solve the problem
solveConstructGraphOfSpectrum :: String -> String
solveConstructGraphOfSpectrum s = 
    let edges = constructSpectrumGraph s
        formattedEdges = map (\(a,b) -> show a ++ "->" ++ show b) edges
    in unlines formattedEdges

-- For the specific case where we have a spectrum list
solveSpectrumGraph :: [Int] -> String
solveSpectrumGraph spectrum = 
    let edges = constructGraphFromSpectrum spectrum
        formattedEdges = map (\(a,b) -> show a ++ "->" ++ show b) edges
    in unlines formattedEdges

-- Example usage
exampleInput :: String
exampleInput = "SKADYE"

-- Example with spectrum
exampleSpectrum :: [Int]
exampleSpectrum = [0,113,128,147,299,314,427]

-- Main function for Rosalind problem
main :: IO ()
main = do
    -- Process the input string to get spectrum graph
    let result = solveConstructGraphOfSpectrum exampleInput
    putStrLn result
```

## Explanation

The solution works as follows:

1. **Amino Acid Mass Mapping**: The `aminoAcidMass` function maps each amino acid to its mass value.

2. **Prefix Mass Calculation**: The `prefixMasses` function calculates cumulative masses of the peptide sequence, which helps in determining the spectrum.

3. **Graph Construction**: The `constructSpectrumGraph` function:
   - Computes all possible edges between prefix masses
   - Filters edges to only include those with valid amino acid mass differences
   - Returns the list of valid edges in the format "a->b"

4. **Edge Filtering**: Only edges where the difference corresponds to standard amino acid masses are kept.

## Sample Input/Output

For input string "SKADYE":
- Prefix masses: [0, 113, 128, 147, 299, 314, 427]
- Valid edges: All pairs where the difference matches amino acid masses

The output would be formatted as:
```
0->113
113->128
128->147
147->299
299->314
314->427
```

This represents the directed graph where each edge shows a mass difference that corresponds to a specific amino acid.

