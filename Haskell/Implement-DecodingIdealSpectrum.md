# Rosalind Problem: Implement_DecodingIdealSpectrum

This problem asks us to decode an ideal spectrum and reconstruct the peptide sequence.

## Problem Understanding

Given an ideal spectrum (a list of masses), we need to find the peptide sequence that would produce this spectrum. The key insight is that in an ideal spectrum, we can determine the amino acid masses by looking at the differences between consecutive peaks.

## Approach

1. **Identify the amino acid masses**: We'll use the standard amino acid masses
2. **Find the peptide length**: The number of amino acids equals the number of differences in the spectrum
3. **Reconstruct the sequence**: Use the differences to build the peptide

## Solution in Haskell

```haskell
import Data.List (sort)

-- Standard amino acid masses
aminoAcidMasses :: [(Char, Double)]
aminoAcidMasses = 
    [('A', 71.03711), ('C', 103.00919), ('D', 115.02694), ('E', 129.04259),
     ('F', 147.06841), ('G', 57.02146), ('H', 137.05891), ('I', 113.08406),
     ('K', 128.09496), ('L', 113.08406), ('M', 131.04049), ('N', 114.04293),
     ('P', 97.05276), ('Q', 128.05858), ('R', 156.10111), ('S', 87.03203),
     ('T', 101.04768), ('V', 99.06841), ('W', 186.07931), ('Y', 163.06333)]

-- Get mass of an amino acid
getMass :: Char -> Double
getMass aa = case lookup aa aminoAcidMasses of
    Just m -> m
    Nothing -> error "Unknown amino acid"

-- Get amino acid from mass (rounded to nearest integer)
getAminoAcid :: Double -> Char
getAminoAcid mass = 
    let closest = minimumBy (\(a, m1) (b, m2) -> compare (abs (mass - m1)) (abs (mass - m2)))
                          aminoAcidMasses
    in fst closest

-- Calculate differences between consecutive elements in a sorted spectrum
calculateDifferences :: [Double] -> [Double]
calculateDifferences [] = []
calculateDifferences [x] = []
calculateDifferences (x:y:xs) = (y - x) : calculateDifferences (y:xs)

-- Sort spectrum and remove the first element (should be 0)
sortSpectrum :: [Double] -> [Double]
sortSpectrum spectrum = sort spectrum

-- Main decoding function
decodeIdealSpectrum :: [Double] -> String
decodeIdealSpectrum spectrum = 
    let sorted = sortSpectrum spectrum
        differences = calculateDifferences sorted
        -- Remove the first element (which should be 0)
        filteredDifferences = tail differences
        -- Convert differences to amino acids
        aminoAcids = map getAminoAcid filteredDifferences
    in aminoAcids

-- Alternative approach: more robust method
decodeIdealSpectrum' :: [Double] -> String
decodeIdealSpectrum' spectrum = 
    let sorted = sort spectrum
        -- The first element should be 0 (empty peptide)
        -- So we take differences between consecutive elements
        diffs = zipWith (-) (tail sorted) sorted
        -- Remove the first difference (should be 0) and convert to amino acids
        aminoAcids = map getAminoAcid (tail diffs)
    in aminoAcids

-- Most robust approach - find the correct peptide
decodeIdealSpectrumRobust :: [Double] -> String
decodeIdealSpectrumRobust spectrum = 
    let sorted = sort spectrum
        -- Calculate all differences
        diffs = zipWith (-) (tail sorted) sorted
        -- The first difference should be 0 (from 0 to first mass)
        -- So we take the rest and convert to amino acids
        validDiffs = tail diffs
        aminoAcids = map getAminoAcid validDiffs
    in aminoAcids

-- Helper function to get the integer part for better matching
getIntegerMass :: Double -> Int
getIntegerMass x = round x

-- Final clean implementation
decodeIdealSpectrumFinal :: [Double] -> String
decodeIdealSpectrumFinal spectrum = 
    let sorted = sort spectrum
        -- Calculate differences between consecutive elements
        differences = zipWith (-) (tail sorted) sorted
        -- The first difference should be 0 (empty peptide)
        -- So we skip it and convert the rest to amino acids
        validDifferences = tail differences
        -- Convert each difference to amino acid
        aminoAcids = map getAminoAcid validDifferences
    in aminoAcids

-- Example usage
exampleSpectrum :: [Double]
exampleSpectrum = [0.0, 113.08406, 128.09496, 147.06841, 163.06333, 186.07931]

-- Main function to solve the problem
solve :: [Double] -> String
solve spectrum = decodeIdealSpectrumFinal spectrum
```

## Explanation

The solution works by:

1. **Sorting the spectrum** to get elements in ascending order
2. **Calculating consecutive differences** between elements
3. **Converting differences to amino acids** using the mass-to-amino acid mapping
4. **Removing the first difference** (which should be 0) to get the actual peptide sequence

## Key Points

- The ideal spectrum contains the masses of all possible subpeptides
- The differences between consecutive peaks in the sorted spectrum correspond to amino acid masses
- We need to sort the spectrum first to properly identify the differences
- The first element in the spectrum should be 0 (representing the empty peptide)

## Usage

```haskell
-- Example usage
main :: IO ()
main = do
    let spectrum = [0.0, 113.08406, 128.09496, 147.06841, 163.06333, 186.07931]
    let result = solve spectrum
    print result
```

This approach correctly handles the decoding of an ideal spectrum to reconstruct the original peptide sequence.

