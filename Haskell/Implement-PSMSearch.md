# Rosalind Problem: Implement PSMSearch in Haskell

## Problem Description
Implement a function to search for peptide sequences in a database using peptide spectrum matching (PSM) approach. This involves finding the best matching peptides given a theoretical spectrum and experimental spectrum.

## Solution

```haskell
import Data.List
import Data.Ord
import qualified Data.Map as Map

-- Define types for our problem
type Peptide = String
type Spectrum = [Double]
type Mass = Double
type Score = Int

-- Function to calculate theoretical spectrum of a peptide
theoreticalSpectrum :: Peptide -> Spectrum
theoreticalSpectrum peptide = 
    let masses = map (massFromAA) peptide
        prefixSums = scanl (+) 0 masses
        suffixSums = scanr (+) 0 masses
        -- Include 0 and total mass
        allSums = 0 : prefixSums ++ suffixSums
        -- Remove duplicates and sort
        uniqueSums = sort (nub allSums)
    in tail uniqueSums  -- Remove the initial 0

-- Amino acid masses (simplified)
massFromAA :: Char -> Mass
massFromAA 'A' = 71.03711
massFromAA 'C' = 103.00919
massFromAA 'D' = 115.02694
massFromAA 'E' = 129.04259
massFromAA 'F' = 147.06841
massFromAA 'G' = 57.02146
massFromAA 'H' = 137.05891
massFromAA 'I' = 113.08406
massFromAA 'K' = 128.09496
massFromAA 'L' = 113.08406
massFromAA 'M' = 131.04049
massFromAA 'N' = 114.04293
massFromAA 'P' = 97.05276
massFromAA 'Q' = 128.05858
massFromAA 'R' = 156.10111
massFromAA 'S' = 87.03203
massFromAA 'T' = 101.04768
massFromAA 'V' = 99.06841
massFromAA 'W' = 186.07931
massFromAA 'Y' = 163.06333
massFromAA _ = 0

-- Calculate the score between two spectra
spectrumScore :: Spectrum -> Spectrum -> Score
spectrumScore expSpec theorSpec = 
    let expSorted = sort expSpec
        theorSorted = sort theorSpec
        matches = zipWith (\a b -> if abs (a - b) < 0.01 then 1 else 0) 
                         expSorted theorSorted
    in sum matches

-- Find best matching peptides
psmSearch :: Spectrum -> [Peptide] -> [(Peptide, Score)]
psmSearch experimentalSpectrum peptides = 
    let theoreticalSpectra = map theoreticalSpectrum peptides
        scores = zipWith spectrumScore (repeat experimentalSpectrum) theoreticalSpectra
        results = zip peptides scores
    in sortBy (comparing snd) results

-- Alternative implementation with more precise matching
psmSearchPrecise :: Spectrum -> [Peptide] -> [(Peptide, Score)]
psmSearchPrecise experimentalSpectrum peptides = 
    let theoreticalSpectra = map theoreticalSpectrum peptides
        scores = zipWith (scoreSpectra experimentalSpectrum) theoreticalSpectra
        results = zip peptides scores
    in reverse (sortBy (comparing snd) results)

-- More precise spectrum scoring function
scoreSpectra :: Spectrum -> Spectrum -> Score
scoreSpectra expSpec theorSpec = 
    let expSorted = sort expSpec
        theorSorted = sort theorSpec
        -- Find matches within tolerance
        matches = matchWithinTolerance expSorted theorSorted 0.01
    in length matches

-- Helper function to match spectra within tolerance
matchWithinTolerance :: Spectrum -> Spectrum -> Double -> [(Double, Double)]
matchWithinTolerance [] _ _ = []
matchWithinTolerance _ [] _ = []
matchWithinTolerance (x:xs) (y:ys) tolerance
    | abs (x - y) <= tolerance = (x, y) : matchWithinTolerance xs ys tolerance
    | x < y = matchWithinTolerance xs (y:ys) tolerance
    | otherwise = matchWithinTolerance (x:xs) ys tolerance

-- Main function to solve the problem
solvePSMSearch :: Spectrum -> [Peptide] -> [Peptide]
solvePSMSearch experimentalSpectrum peptides = 
    let scores = map (\p -> (p, scoreSpectra experimentalSpectrum (theoreticalSpectrum p))) peptides
        sortedScores = sortBy (comparing snd) scores
        bestScore = snd (last sortedScores)
        bestPeptides = [p | (p, s) <- sortedScores, s == bestScore]
    in reverse bestPeptides

-- Example usage
main :: IO ()
main = do
    -- Example experimental spectrum
    let expSpec = [113.08406, 129.04259, 147.06841, 163.06333]
    -- Example peptides
    let peptides = ["ACDE", "FGHI", "JKLM", "NOPQ"]
    
    -- Find best matches
    let results = psmSearchPrecise expSpec peptides
    print results
    
    -- Get top scoring peptide
    let topPeptide = head (solvePSMSearch expSpec peptides)
    print $ "Best matching peptide: " ++ topPeptide
```

## Key Functions Explained

1. **`theoreticalSpectrum`**: Computes the theoretical mass spectrum of a peptide by:
   - Converting each amino acid to its mass
   - Calculating prefix and suffix sums
   - Sorting and removing duplicates

2. **`massFromAA`**: Maps amino acid characters to their masses (simplified version)

3. **`spectrumScore`**: Compares two spectra and returns the number of matching peaks within tolerance

4. **`psmSearch`**: Main search function that:
   - Computes theoretical spectra for all peptides
   - Scores each peptide against the experimental spectrum
   - Returns peptides sorted by score

5. **`solvePSMSearch`**: Returns the best matching peptide(s) based on highest score

## Usage Example

```haskell
-- Example usage:
let experimental = [113.08406, 129.04259, 147.06841, 163.06333]
let candidates = ["ACDE", "FGHI", "JKLM"]
let bestMatches = solvePSMSearch experimental candidates
```

This implementation provides a complete solution for peptide spectrum matching that can be used to identify the best matching peptides from a database given an experimental spectrum.

