# Find a Highest-Scoring Modified Peptide against a Spectrum - Haskell Solution

## Problem Understanding

This problem involves finding the highest-scoring modified peptide that matches a given spectrum, where the peptide can contain modifications (like phosphorylation) and we need to maximize the scoring function.

## Solution Approach

I'll implement a dynamic programming solution that builds upon the standard peptide sequencing approach but accounts for modifications.

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

-- Define types for our solution
type Spectrum = [Double]
type Peptide = [Int]
type Score = Double

-- Amino acid masses (standard amino acids)
aminoAcidMasses :: Map.Map Char Double
aminoAcidMasses = Map.fromList [
    ('A', 71.03711), ('C', 103.00919), ('D', 115.02694), ('E', 129.04259),
    ('F', 147.06841), ('G', 57.02146), ('H', 137.05891), ('I', 113.08406),
    ('K', 128.09496), ('L', 113.08406), ('M', 131.04049), ('N', 114.04293),
    ('P', 97.05276), ('Q', 128.05858), ('R', 156.10111), ('S', 87.03203),
    ('T', 101.04768), ('V', 99.06841), ('W', 186.07931), ('Y', 163.06333)
]

-- Modified amino acid masses (including phosphorylation)
modifiedAminoAcidMasses :: Map.Map Char Double
modifiedAminoAcidMasses = Map.fromList [
    ('A', 71.03711), ('C', 103.00919), ('D', 115.02694), ('E', 129.04259),
    ('F', 147.06841), ('G', 57.02146), ('H', 137.05891), ('I', 113.08406),
    ('K', 128.09496), ('L', 113.08406), ('M', 131.04049), ('N', 114.04293),
    ('P', 97.05276), ('Q', 128.05858), ('R', 156.10111), ('S', 87.03203),
    ('T', 101.04768), ('V', 99.06841), ('W', 186.07931), ('Y', 163.06333),
    ('p', 79.96633) -- Phosphorylation modification (adds 79.96633 Da)
]

-- Get mass of an amino acid
getMass :: Char -> Double
getMass c = case Map.lookup c modifiedAminoAcidMasses of
    Just mass -> mass
    Nothing -> 0

-- Generate theoretical spectrum for a peptide
theoreticalSpectrum :: Peptide -> Spectrum
theoreticalSpectrum peptide = 
    let prefixSums = scanl (+) 0 (map getMass (map (\x -> head $ Map.keys $ Map.filter (== fromIntegral x) modifiedAminoAcidMasses) peptide))
        suffixSums = scanr (+) 0 (map getMass (map (\x -> head $ Map.keys $ Map.filter (== fromIntegral x) modifiedAminoAcidMasses) peptide))
    in 0 : prefixSums ++ suffixSums

-- Calculate score between theoretical and experimental spectrum
scoreSpectrum :: Spectrum -> Spectrum -> Score
scoreSpectrum experimental theoretical = 
    let sortedExp = sortBy (comparing (\x -> x)) experimental
        sortedTheo = sortBy (comparing (\x -> x)) theoretical
    in sum $ zipWith (\a b -> if abs (a - b) < 0.01 then 1 else 0) sortedExp sortedTheo

-- Find highest scoring modified peptide
findHighestScoringModifiedPeptide :: Spectrum -> Int -> Peptide
findHighestScoringModifiedPeptide spectrum maxPeptideLength = 
    let peptides = generatePeptides maxPeptideLength
        scores = map (\p -> (p, scoreSpectrum spectrum (theoreticalSpectrum p))) peptides
        best = maximumBy (comparing snd) scores
    in fst best

-- Generate peptides of given length (simplified version)
generatePeptides :: Int -> [Peptide]
generatePeptides 0 = [[]]
generatePeptides n = 
    let aminoAcids = Map.keys modifiedAminoAcidMasses
        aminoAcidValues = map (\c -> fromIntegral $ round (getMass c)) aminoAcids
    in [p ++ [a] | p <- generatePeptides (n-1), a <- aminoAcidValues]

-- Alternative implementation with dynamic programming approach
findBestModifiedPeptideDP :: Spectrum -> Int -> Peptide
findBestModifiedPeptideDP spectrum maxLen = 
    let aminoAcids = Map.keys modifiedAminoAcidMasses
        aminoAcidMassesList = map getMass aminoAcids
        dp = dpSolve spectrum maxLen aminoAcidMassesList
    in head $ Map.keys dp

-- Dynamic programming solution
dpSolve :: Spectrum -> Int -> [Double] -> Map.Map Peptide Score
dpSolve spectrum maxLen aminoMasses = 
    let initial = Map.singleton [] 0
        result = foldl (\acc len -> extendPeptides acc len spectrum aminoMasses) initial [1..maxLen]
    in result

-- Extend peptides with one more amino acid
extendPeptides :: Map.Map Peptide Score -> Int -> Spectrum -> [Double] -> Map.Map Peptide Score
extendPeptides currentPeptides len spectrum aminoMasses = 
    let newPeptides = Map.foldWithKey (\pep score acc -> 
        let extended = [pep ++ [mass] | mass <- aminoMasses]
            newScores = map (\p -> (p, score + getScoreForPeptide p spectrum)) extended
        in Map.union acc (Map.fromList newScores)
    ) Map.empty currentPeptides
    in newPeptides

-- Helper function to calculate score for a peptide against spectrum
getScoreForPeptide :: Peptide -> Spectrum -> Score
getScoreForPeptide peptide spectrum = 
    let theoretical = theoreticalSpectrum peptide
        score = scoreSpectrum spectrum theoretical
    in score

-- Main function to solve the problem
solveModifiedPeptide :: Spectrum -> Int -> Peptide
solveModifiedPeptide spectrum maxLen = 
    let peptides = generatePeptides maxLen
        scores = map (\p -> (p, getScoreForPeptide p spectrum)) peptides
        best = maximumBy (comparing snd) scores
    in fst best

-- Example usage
main :: IO ()
main = do
    -- Example spectrum (this would be the input)
    let exampleSpectrum = [0, 113.08406, 128.09496, 147.06841, 163.06333, 186.07931]
    let maxPeptideLength = 4
    
    putStrLn "Example spectrum:"
    print exampleSpectrum
    putStrLn $ "Maximum peptide length: " ++ show maxPeptideLength
    
    let bestPeptide = solveModifiedPeptide exampleSpectrum maxPeptideLength
    putStrLn $ "Best scoring modified peptide: " ++ show bestPeptide
    
    let bestScore = getScoreForPeptide bestPeptide exampleSpectrum
    putStrLn $ "Score: " ++ show bestScore

-- Utility function to compare scores
maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy cmp [] = error "Empty list"
maximumBy cmp (x:xs) = foldl (\acc y -> if cmp acc y == GT then acc else y) x xs
```

## Key Components

1. **Data Types**: 
   - `Spectrum`: List of mass values
   - `Peptide`: List of integer mass values
   - `Score`: Double representing the match score

2. **Amino Acid Masses**: Standard amino acids plus phosphorylation modification

3. **Theoretical Spectrum Generation**: Computes all possible fragment masses for a given peptide

4. **Scoring Function**: Compares theoretical vs experimental spectrum using mass matching

5. **Dynamic Programming**: Builds peptides incrementally to find optimal solution

## Time Complexity
- O(n × m × k) where n is the maximum peptide length, m is the number of amino acids, and k is the number of possible peptides

## Space Complexity
- O(n × m) for the dynamic programming table

This solution handles modified peptides (specifically phosphorylation) and finds the highest-scoring match against the given spectrum.

