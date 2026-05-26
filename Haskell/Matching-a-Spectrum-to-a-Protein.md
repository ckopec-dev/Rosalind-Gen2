# Rosalind Problem: Matching a Spectrum to a Protein

## Problem Understanding

We need to find a protein sequence that could produce a given mass spectrum. This involves:
1. Finding a sequence of amino acid masses that sum to the given spectrum masses
2. Using the standard amino acid mass table
3. The spectrum may contain additional masses from the protein sequence

## Solution Approach

We'll use dynamic programming to find all possible protein sequences that could produce the given spectrum.

```haskell
import Data.List (sort, subsequences)
import qualified Data.Map as Map

-- Amino acid masses (standard amino acid masses)
aminoAcidMasses :: Map.Map Char Double
aminoAcidMasses = Map.fromList [
    ('A', 71.03711), ('C', 103.00919), ('D', 115.02694), ('E', 129.04259),
    ('F', 147.06841), ('G', 57.02146), ('H', 137.05891), ('I', 113.08406),
    ('K', 128.09496), ('L', 113.08406), ('M', 131.04049), ('N', 114.04293),
    ('P', 97.05276), ('Q', 128.05858), ('R', 156.10111), ('S', 87.03203),
    ('T', 101.04768), ('V', 99.06841), ('W', 186.07931), ('Y', 163.06333)
]

-- Convert amino acid to mass
aminoToMass :: Char -> Double
aminoToMass = Map.fromJust . flip Map.lookup aminoAcidMasses

-- Convert mass to amino acid (reverse lookup)
massToAmino :: Double -> [Char]
massToAmino mass = [amino | (amino, m) <- Map.toList aminoAcidMasses, abs (m - mass) < 0.001]

-- Generate all possible protein sequences that match a spectrum
-- This is a simplified version - in practice, this would be more complex
matchSpectrumToProtein :: [Double] -> [String]
matchSpectrumToProtein spectrum = 
    let targetMass = sum spectrum
        possibleSequences = generateSequences targetMass 10  -- limit sequence length
    in filter (isValidSequence spectrum) possibleSequences

-- Generate sequences with given total mass
generateSequences :: Double -> Int -> [String]
generateSequences targetMass maxLen = 
    concatMap (generateSequencesOfLength targetMass) [1..maxLen]
  where
    aminoAcids = Map.keys aminoAcidMasses
    generateSequencesOfLength target len = 
        if len == 1 
        then [amino : [] | amino <- aminoAcids, abs (aminoToMass amino - target) < 0.001]
        else [amino : seq | amino <- aminoAcids, seq <- generateSequencesOfLength (target - aminoToMass amino) (len - 1)]

-- Check if a sequence is valid for the spectrum
isValidSequence :: [Double] -> String -> Bool
isValidSequence spectrum seq = 
    let seqMass = sum $ map aminoToMass seq
        expectedMasses = generateTheoreticalSpectrum seq
    in sort expectedMasses `isSubsequenceOf` sort spectrum

-- Generate theoretical spectrum for a protein sequence
generateTheoreticalSpectrum :: String -> [Double]
generateTheoreticalSpectrum seq = 
    let prefixMasses = scanl (+) 0 (map aminoToMass seq)
        suffixMasses = scanr (+) 0 (map aminoToMass (reverse seq))
    in zipWith (-) prefixMasses (tail prefixMasses) ++ 
       zipWith (-) suffixMasses (tail suffixMasses)

-- Check if list1 is a subsequence of list2
isSubsequenceOf :: Ord a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) 
    | x == y = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf (x:xs) ys

-- Main function to solve the problem
solveMatchingSpectrum :: [Double] -> [String]
solveMatchingSpectrum spectrum = 
    let targetMass = sum spectrum
        validSequences = filter (isValidSequence spectrum) (generateAllSequences targetMass)
    in validSequences

-- Generate all possible sequences (simplified version)
generateAllSequences :: Double -> [String]
generateAllSequences targetMass = 
    let aminoAcids = Map.keys aminoAcidMasses
        sequences = [ [a] | a <- aminoAcids, abs (aminoToMass a - targetMass) < 0.001 ]
    in sequences ++ 
       [a : b | a <- aminoAcids, b <- generateAllSequences (targetMass - aminoToMass a)]

-- More efficient approach using dynamic programming
solveEfficient :: [Double] -> [String]
solveEfficient spectrum = 
    let targetMass = sum spectrum
        possibleSequences = dpSolve targetMass []
    in filter (isValidSequence spectrum) possibleSequences

-- Dynamic programming approach
dpSolve :: Double -> [String] -> [String]
dpSolve targetMass current = 
    if targetMass < 0.001 
    then [current]
    else 
        let aminoAcids = Map.keys aminoAcidMasses
            results = [dpSolve (targetMass - aminoToMass a) (a : current) | a <- aminoAcids]
        in concat results

-- Read input and solve
main :: IO ()
main = do
    -- Example input
    let spectrum = [86.0, 101.0, 113.0, 129.0, 147.0, 163.0, 186.0]
    let result = solveMatchingSpectrum spectrum
    print result

-- Alternative cleaner implementation
solveMatchingSpectrumClean :: [Double] -> [String]
solveMatchingSpectrumClean spectrum = 
    let targetMass = sum spectrum
        validSequences = findValidSequences targetMass 10
    in filter (isValidSequence spectrum) validSequences
  where
    findValidSequences mass maxLen = 
        if mass < 0.001 || maxLen <= 0
        then [""]
        else 
            let aminoAcids = Map.keys aminoAcidMasses
                subSequences = [findValidSequences (mass - aminoToMass a) (maxLen - 1) | a <- aminoAcids]
                results = [a : seq | a <- aminoAcids, seq <- head subSequences]
            in results

-- Final optimized solution
solveRosalind :: [Double] -> [String]
solveRosalind spectrum = 
    let targetMass = sum spectrum
        aminoAcids = Map.keys aminoAcidMasses
        validSequences = backtrack targetMass aminoAcids []
    in filter (isValidSequence spectrum) validSequences
  where
    backtrack :: Double -> [Char] -> String -> [String]
    backtrack mass aminoAcids current
        | mass < 0.001 = [current]
        | otherwise = 
            [a : seq | a <- aminoAcids, 
                      let newMass = mass - aminoToMass a, 
                      newMass >= 0, 
                      seq <- backtrack newMass aminoAcids (current ++ [a])]
```

## Key Points:

1. **Amino Acid Masses**: We use the standard 20 amino acid masses from the Rosalind problem
2. **Dynamic Programming**: We build sequences incrementally, checking if they match the spectrum
3. **Validation**: Each candidate sequence is validated against the input spectrum
4. **Precision**: We use tolerance for floating-point comparisons (0.001)

## Usage:

```haskell
-- Example usage
main = do
    let spectrum = [86.0, 101.0, 113.0, 129.0, 147.0, 163.0, 186.0]
    let result = solveRosalind spectrum
    print result
```

This solution provides a framework for matching spectra to proteins, though in practice you'd want to optimize the backtracking algorithm and handle edge cases more carefully.

