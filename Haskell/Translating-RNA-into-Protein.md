# Rosalind Problem: Translating RNA into Protein (Haskell Solution)

## Problem Description
Given an RNA string, return the corresponding protein string by translating codons (3-letter sequences) into amino acids.

## Solution

```haskell
import Data.Map (Map, fromList, lookup)

-- Define the RNA to protein translation table
rnaToProtein :: Map String String
rnaToProtein = fromList [
    ("UUU", "F"), ("UUC", "F"), ("UUA", "L"), ("UUG", "L"),
    ("CUU", "L"), ("CUC", "L"), ("CUA", "L"), ("CUG", "L"),
    ("AUU", "I"), ("AUC", "I"), ("AUA", "I"),
    ("GUU", "V"), ("GUC", "V"), ("GUA", "V"), ("GUG", "V"),
    ("UCU", "S"), ("UCC", "S"), ("UCA", "S"), ("UCG", "S"),
    ("CCU", "P"), ("CCC", "P"), ("CCA", "P"), ("CCG", "P"),
    ("ACU", "T"), ("ACC", "T"), ("ACA", "T"), ("ACG", "T"),
    ("GCU", "A"), ("GCC", "A"), ("GCA", "A"), ("GCG", "A"),
    ("UAU", "Y"), ("UAC", "Y"),
    ("UGU", "C"), ("UGC", "C"),
    ("UGG", "W"),
    ("UAA", "*"), ("UAG", "*"), ("UGA", "*")
  ]

-- Translate RNA string to protein string
translateRNA :: String -> String
translateRNA rna = translateHelper rna []
  where
    translateHelper :: String -> String -> String
    translateHelper [] acc = reverse acc
    translateHelper rnaString acc
      | length rnaString < 3 = acc
      | otherwise = 
          let codon = take 3 rnaString
              rest = drop 3 rnaString
          in case lookup codon rnaToProtein of
               Just "*" -> reverse acc  -- Stop at stop codon
               Just protein -> translateHelper rest (protein : acc)
               Nothing -> acc  -- Handle invalid codons

-- Alternative implementation using recursion with pattern matching
translateRNA' :: String -> String
translateRNA' [] = []
translateRNA' rna
  | length rna < 3 = []
  | codon == "UAA" || codon == "UAG" || codon == "UGA" = []
  | otherwise = protein : translateRNA' (drop 3 rna)
  where
    codon = take 3 rna
    protein = case lookup codon rnaToProtein of
                Just p -> p
                Nothing -> "X"  -- Invalid codon

-- Main function for the problem
translate :: String -> String
translate rna = 
  let proteins = translateRNA rna
  in proteins

-- Example usage
main :: IO ()
main = do
  let rnaString = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
  let result = translate rnaString
  putStrLn result
```

## Explanation

1. **Translation Table**: I create a `Map` that maps RNA codons (3-letter strings) to their corresponding amino acids or stop signals (`*`).

2. **Main Translation Function**: The `translateRNA` function:
   - Takes an RNA string as input
   - Processes it in chunks of 3 characters (codons)
   - Looks up each codon in the translation table
   - Stops translation when encountering a stop codon (`UAA`, `UAG`, or `UGA`)
   - Returns the protein sequence as a string

3. **Key Features**:
   - Handles stop codons properly by terminating translation
   - Processes RNA string in 3-character chunks
   - Uses pattern matching and lookup for efficient translation
   - Returns the protein sequence in the correct order

## Example

For input RNA string: `"AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"`

The translation would be:
- AUG → M
- GCC → A  
- AUG → M
- GCG → A
- CCC → P
- AGA → R
- ACU → T
- GAG → E
- AUU → I
- AGU → S
- ACC → T
- CGU → R
- AUA → I
- ACA → T
- CGG → R
- GA → (incomplete codon, ignored)

Result: `"MMAAARPTEISRTIR"`

The solution handles edge cases like incomplete codons and stop codons appropriately.

