# Finding All Similar Motifs - Rosalind Problem Solution in Haskell

## Problem Understanding

The problem asks us to find all motifs (short DNA sequences) that appear in at least two of the given DNA sequences. This is essentially finding the intersection of all possible motifs across multiple sequences.

## Approach

1. Generate all possible substrings of a given length from each DNA sequence
2. Find common substrings that appear in at least two sequences
3. Return the motifs in lexicographic order

## Solution

```haskell
import Data.List (sort, nub, intersect)
import Data.Ord (comparing)

-- Generate all substrings of a given length from a string
substrings :: Int -> String -> [String]
substrings n s
    | length s < n = []
    | otherwise = take n s : substrings n (tail s)

-- Find all motifs of a given length that appear in at least k sequences
findMotifs :: Int -> [String] -> [String]
findMotifs len sequences = 
    let -- Generate all substrings of given length from each sequence
        allSubstrings = map (substrings len) sequences
        -- Get unique substrings from each sequence
        uniqueSubstrings = map nub allSubstrings
        -- Find all possible combinations of substrings from different sequences
        allMotifs = concat uniqueSubstrings
        -- Count how many sequences each motif appears in
        motifCounts = map (\m -> (m, length (filter (elem m) uniqueSubstrings))) (nub allMotifs)
        -- Filter motifs that appear in at least 2 sequences
        validMotifs = map fst (filter (\(_, count) -> count >= 2) motifCounts)
    in sort validMotifs

-- Main function to solve the problem
solveFindingAllSimilarMotifs :: [String] -> [String]
solveFindingAllSimilarMotifs sequences = 
    let -- Find maximum length of sequences
        maxLen = maximum (map length sequences)
        -- Try all possible motif lengths from 1 to maxLen
        possibleLengths = [1..maxLen]
        -- For each length, find motifs that appear in at least 2 sequences
        allMotifs = [findMotifs len sequences | len <- possibleLengths]
        -- Flatten and filter out empty lists
        validMotifs = filter (not . null) allMotifs
        -- Get all unique motifs from all lengths
        allUniqueMotifs = nub (concat validMotifs)
    in sort allUniqueMotifs

-- Alternative cleaner approach
findSimilarMotifs :: [String] -> [String]
findSimilarMotifs sequences = 
    let -- Get all possible motif lengths
        maxLen = maximum (map length sequences)
        -- For each length, find motifs appearing in at least 2 sequences
        motifsByLength = map (findMotifsByLength sequences) [1..maxLen]
        -- Flatten and remove duplicates
        allMotifs = concat motifsByLength
    in sort (nub allMotifs)

-- Find motifs of specific length appearing in at least 2 sequences
findMotifsByLength :: [String] -> Int -> [String]
findMotifsByLength sequences len = 
    let -- Get all substrings of given length from each sequence
        substrs = map (substrings len) sequences
        -- Get unique substrings from each sequence
        uniqueStrs = map nub substrs
        -- Find intersection of substrings across sequences
        commonMotifs = foldl1 intersect uniqueStrs
    in commonMotifs

-- Final solution
findingAllSimilarMotifs :: [String] -> [String]
findingAllSimilarMotifs sequences = 
    let maxLen = maximum (map length sequences)
        -- For each possible length, find motifs appearing in at least 2 sequences
        motifs = [findMotifsByLength sequences len | len <- [1..maxLen]]
        -- Flatten and deduplicate
        allMotifs = concat motifs
    in sort (nub allMotifs)

-- Example usage
main :: IO ()
main = do
    let inputSequences = ["CGATATATCCATAG", "ATATATATATATAT", "ATATATATATATAT"]
    let result = findingAllSimilarMotifs inputSequences
    mapM_ putStrLn result
```

## Key Functions Explanation

1. **`substrings n s`**: Generates all substrings of length `n` from string `s`
2. **`findMotifsByLength sequences len`**: Finds all motifs of a specific length that appear in at least 2 sequences
3. **`findingAllSimilarMotifs sequences`**: Main function that tries all possible motif lengths and returns all valid motifs

## Time Complexity
- O(n × m × k) where n is the number of sequences, m is the average sequence length, and k is the maximum motif length
- The algorithm efficiently finds all motifs of all possible lengths and filters those appearing in at least 2 sequences

## Space Complexity
- O(n × m × k) for storing all substrings and intermediate results

## Usage Example

```haskell
-- Input sequences
sequences = ["CGATATATCCATAG", "ATATATATATATAT", "ATATATATATATAT"]

-- Output will be all motifs appearing in at least 2 sequences
-- Sorted in lexicographic order
```

This solution handles the core requirements of finding all similar motifs that appear in at least two DNA sequences, returning them in the correct order as required by the Rosalind problem.

