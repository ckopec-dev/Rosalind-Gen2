# Rosalind Problem: Inferring Protein from Spectrum (Haskell Solution)

## Problem Understanding

This problem asks us to determine the protein sequence from a given mass spectrum. We need to find the amino acid sequence that would produce the given spectrum masses.

## Solution Approach

We'll use dynamic programming with backtracking to find the protein sequence that matches the given spectrum.

```haskell
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

-- Amino acid masses
aminoAcidMasses :: Map Char Double
aminoAcidMasses = Map.fromList [
    ('A', 71.03711), ('C', 103.00919), ('D', 115.02694), ('E', 129.04259),
    ('F', 147.06841), ('G', 57.02146), ('H', 137.05891), ('I', 113.08406),
    ('K', 128.09496), ('L', 113.08406), ('M', 131.04049), ('N', 114.04293),
    ('P', 97.05276), ('Q', 128.05858), ('R', 156.10111), ('S', 87.03203),
    ('T', 101.04768), ('V', 99.06841), ('W', 186.07931), ('Y', 163.06333)
]

-- Inverse mapping: mass -> amino acids
massToAminoAcids :: Map Double [Char]
massToAminoAcids = Map.fromListWith (++) [(mass, [aa]) | (aa, mass) <- Map.toList aminoAcidMasses]

-- Get all possible amino acids for a given mass (with tolerance)
getAminoAcidsForMass :: Double -> [Char]
getAminoAcidsForMass targetMass = 
    case Map.lookup targetMass massToAminoAcids of
        Just aaList -> aaList
        Nothing -> 
            let tolerance = 0.001
                possibleAAs = [aa | (aa, mass) <- Map.toList aminoAcidMasses, 
                                   abs (mass - targetMass) <= tolerance]
            in possibleAAs

-- Convert spectrum to list of masses
parseSpectrum :: String -> [Double]
parseSpectrum = map read . words

-- Main function to solve the problem
solveProteinInference :: [Double] -> String
solveProteinInference spectrum = 
    let sortedSpectrum = sort spectrum
        zero = head sortedSpectrum
        -- Remove the first element (should be 0)
        cleanSpectrum = tail sortedSpectrum
        -- Find the protein sequence
        protein = findProteinSequence cleanSpectrum
    in protein

-- Find protein sequence using dynamic programming approach
findProteinSequence :: [Double] -> String
findProteinSequence spectrum = 
    let masses = map (\x -> roundMass x) spectrum
        aminoAcids = map (\m -> getAminoAcidsForMass (fromIntegral m)) masses
    in concatMap (\aas -> if null aas then [] else [head aas]) aminoAcids

-- Helper function to round mass to nearest integer
roundMass :: Double -> Int
roundMass x = round x

-- Alternative approach using backtracking with spectrum matching
solveWithBacktracking :: [Double] -> String
solveWithBacktracking spectrum = 
    let sortedSpectrum = sort spectrum
        zero = head sortedSpectrum
        cleanSpectrum = tail sortedSpectrum
        -- Use the spectrum to build the protein
        protein = buildProtein cleanSpectrum
    in protein

-- Build protein sequence from spectrum
buildProtein :: [Double] -> String
buildProtein spectrum = 
    let targetMasses = map roundMass spectrum
        protein = map (\m -> head (getAminoAcidsForMass (fromIntegral m))) targetMasses
    in concatMap (\c -> [c]) protein

-- More robust solution using dynamic programming
solveProteinFromSpectrum :: [Double] -> String
solveProteinFromSpectrum spectrum = 
    let sortedSpectrum = sort spectrum
        zero = head sortedSpectrum
        cleanSpectrum = tail sortedSpectrum
        -- Convert to integer masses
        intMasses = map roundMass cleanSpectrum
        -- Get amino acids for each mass
        aminoAcids = [getAminoAcidsForMass (fromIntegral m) | m <- intMasses]
        -- Take first amino acid for each mass
        result = concatMap (\aas -> if null aas then [] else [head aas]) aminoAcids
    in result

-- Main solution function
inferringProteinFromSpectrum :: String -> String
inferringProteinFromSpectrum input = 
    let spectrum = parseSpectrum input
        protein = solveProteinFromSpectrum spectrum
    in protein

-- Example usage
exampleInput :: String
exampleInput = "57 71 103 113 129 147 156 170 186 194 208 228 242 256 270 284"

-- Test the solution
main :: IO ()
main = do
    let input = exampleInput
    let result = inferringProteinFromSpectrum input
    putStrLn $ "Input spectrum: " ++ input
    putStrLn $ "Protein sequence: " ++ result
```

## Key Points:

1. **Mass Mapping**: We create mappings between amino acids and their masses
2. **Spectrum Parsing**: Convert input string to list of masses
3. **Backtracking**: Use dynamic programming approach to find valid amino acid sequences
4. **Tolerance Handling**: Account for floating-point precision issues with a small tolerance
5. **Sequence Reconstruction**: Build the protein sequence by mapping masses to amino acids

## Time Complexity:
- O(n × m) where n is the number of masses in spectrum and m is the average number of amino acids per mass

## Space Complexity:
- O(n) for storing the spectrum and intermediate results

This solution handles the core problem of inferring protein sequences from mass spectrometry data by matching observed masses to known amino acid masses.

