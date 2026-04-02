# Rosalind Problem: Inferring mRNA from Protein

## Problem Statement
Given an amino acid string, determine the number of possible mRNA strings from which the protein could have been translated, modulo 1000000.

## Solution Approach
1. Create a mapping from amino acids to the number of possible codons
2. For each amino acid in the protein string, multiply the result by the number of codons for that amino acid
3. Account for the stop codons (which are special cases)
4. Return the result modulo 1000000

## Haskell Implementation

```haskell
module InferringMRNAFromProtein where

import Data.List (group, sort)

-- Number of codons for each amino acid
codonCount :: Char -> Int
codonCount 'A' = 4  -- GCU, GCC, GCA, GCG
codonCount 'C' = 2  -- UGU, UGC
codonCount 'D' = 2  -- GAU, GAC
codonCount 'E' = 2  -- GAA, GAG
codonCount 'F' = 2  -- UUU, UUC
codonCount 'G' = 4  -- GGU, GGC, GGA, GGG
codonCount 'H' = 2  -- CAU, CAC
codonCount 'I' = 3  -- AUU, AUC, AUA
codonCount 'K' = 2  -- AAA, AAG
codonCount 'L' = 6  -- UUA, UUG, CUU, CUC, CUA, CUG
codonCount 'M' = 1  -- AUG
codonCount 'N' = 2  -- AAU, AAC
codonCount 'P' = 4  -- CCU, CCC, CCA, CCG
codonCount 'Q' = 2  -- CAA, CAG
codonCount 'R' = 6  -- CGU, CGC, CGA, CGG, AGA, AGG
codonCount 'S' = 6  -- UCU, UCC, UCA, UCG, AGU, AGC
codonCount 'T' = 4  -- ACU, ACC, ACA, ACG
codonCount 'V' = 4  -- GUU, GUC, GUA, GUG
codonCount 'W' = 1  -- UGG
codonCount 'Y' = 2  -- UAU, UAC
codonCount '*' = 3  -- UAA, UAG, UGA (stop codons)
codonCount _   = 0

-- Calculate the number of possible mRNA strings
inferringMRNAFromProtein :: String -> Int
inferringMRNAFromProtein protein = 
    let counts = map codonCount protein
        result = foldl (\acc x -> (acc * x) `mod` 1000000) 1 counts
    in result

-- Alternative implementation using product and map
inferringMRNAFromProtein' :: String -> Int
inferringMRNAFromProtein' protein = 
    let productOfCounts = product $ map codonCount protein
    in productOfCounts `mod` 1000000

-- Test function
testSolution :: IO ()
testSolution = do
    let testInput = "MA"
    let result = inferringMRNAFromProtein testInput
    putStrLn $ "Input: " ++ testInput
    putStrLn $ "Output: " ++ show result
```

## Explanation

The solution works as follows:

1. **Codon Count Mapping**: Each amino acid is mapped to the number of possible codons that can encode it:
   - Amino acids with 1 codon: M (AUG), W (UGG)
   - Amino acids with 2 codons: C, D, E, H, N, Q, Y, K, R, S, T, V
   - Amino acids with 3 codons: I
   - Amino acids with 4 codons: A, G, P, T, V
   - Amino acids with 6 codons: L, R, S
   - Stop codons (*): 3 codons (UAA, UAG, UGA)

2. **Calculation**: For each amino acid in the protein sequence, we multiply the current result by the number of possible codons for that amino acid.

3. **Modular Arithmetic**: Since the result can be very large, we take the modulo 1000000 at each step to prevent overflow.

4. **Stop Codons**: The special character `*` represents stop codons, which have 3 possible codons.

## Example Usage

```haskell
-- Example usage:
-- inferringMRNAFromProtein "MA"  -- Returns 4 (M has 1 codon, A has 4 codons, 1×4 = 4)
-- inferringMRNAFromProtein "M"   -- Returns 1 (M has 1 codon)
-- inferringMRNAFromProtein "A"   -- Returns 4 (A has 4 codons)
```

The time complexity is O(n) where n is the length of the protein string, and the space complexity is O(1).

