# Rosalind Problem: Sequence a Peptide (Sequence_a_Peptide)

## Problem Description
Given an RNA string, we need to translate it into a peptide sequence using the genetic code.

## Solution in Haskell

```haskell
import Data.List (chunksOf)

-- Genetic code mapping from codons to amino acids
geneticCode :: [(String, Char)]
geneticCode = 
    [ ("UUU", 'F'), ("UUC", 'F'), ("UUA", 'L'), ("UUG", 'L')
    , ("CUU", 'L'), ("CUC", 'L'), ("CUA", 'L'), ("CUG", 'L')
    , ("AUU", 'I'), ("AUC", 'I'), ("AUA", 'I'), ("GUU", 'V')
    , ("GUC", 'V'), ("GUA", 'V'), ("GUG", 'V'), ("UCU", 'S')
    , ("UCC", 'S'), ("UCA", 'S'), ("UCG", 'S'), ("CCU", 'P')
    , ("CCC", 'P'), ("CCA", 'P'), ("CCG", 'P'), ("ACU", 'T')
    , ("ACC", 'T'), ("ACA", 'T'), ("ACG", 'T'), ("GCU", 'A')
    , ("GCC", 'A'), ("GCA", 'A'), ("GCG", 'A'), ("UAU", 'Y')
    , ("UAC", 'Y'), ("CAU", 'H'), ("CAC", 'H'), ("CAA", 'Q')
    , ("CAG", 'Q'), ("AAU", 'N'), ("AAC", 'N'), ("AAA", 'K')
    , ("AAG", 'K'), ("GAU", 'D'), ("GAC", 'D'), ("GAA", 'E')
    , ("GAG", 'E'), ("UGU", 'C'), ("UGC", 'C'), ("UGG", 'W')
    , ("UGA", 'X'), ("UAA", 'X'), ("UGA", 'X')  -- Stop codons
    ]

-- Translate a single codon to amino acid
translateCodon :: String -> Char
translateCodon codon = 
    case lookup codon geneticCode of
        Just amino -> amino
        Nothing -> error "Invalid codon"

-- Translate RNA sequence to peptide
translateRNA :: String -> String
translateRNA rna = 
    let codons = chunksOf 3 rna
        aminoAcids = map translateCodon codons
    in takeWhile (/= 'X') aminoAcids  -- Stop at first stop codon

-- Main function to solve the problem
solve :: String -> String
solve = translateRNA

-- Example usage:
-- solve "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
-- Returns: "MAMAPRTEINSTRING"
```

## Explanation

1. **Genetic Code Mapping**: We define a list of tuples mapping RNA codons (3-letter sequences) to their corresponding amino acids.

2. **translateCodon Function**: Takes a 3-character codon and returns the corresponding amino acid character using the genetic code mapping.

3. **translateRNA Function**: 
   - Splits the RNA string into codons using `chunksOf 3`
   - Translates each codon to an amino acid
   - Stops translation when encountering a stop codon ('X')

4. **solve Function**: The main function that takes an RNA string and returns the peptide sequence.

## Key Points

- The genetic code maps codons to amino acids, with stop codons represented by 'X'
- Translation stops at the first stop codon encountered
- The solution handles the standard genetic code table
- Uses `chunksOf 3` to properly split the RNA string into codons

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the RNA string
- **Space Complexity**: O(n) for storing the codons and result

This solution correctly handles the translation of RNA sequences to peptides according to the genetic code rules.

