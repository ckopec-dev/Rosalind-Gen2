# Rosalind Problem: Error Correction in Reads (Haskell Solution)

## Problem Understanding

The problem requires us to identify and correct sequencing errors in DNA reads. Specifically:
- Given a collection of DNA sequences (reads)
- Find reads that differ by exactly one nucleotide from another read
- Correct the erroneous read by replacing it with the correct one

## Solution Approach

1. For each read, check if it's a correct read (appears in the dataset) or an erroneous one
2. If a read differs by exactly one nucleotide from a correct read, it's an error that needs correction
3. Output the corrected read pairs

## Haskell Implementation

```haskell
import Data.List
import Data.Ord

-- Function to calculate Hamming distance between two strings
hammingDistance :: String -> String -> Int
hammingDistance xs ys = sum $ zipWith (\x y -> if x == y then 0 else 1) xs ys

-- Function to find if a read differs by exactly one nucleotide from any correct read
findCorrection :: String -> [String] -> Maybe String
findCorrection read correctReads = 
    case filter (\correct -> hammingDistance read correct == 1) correctReads of
        [] -> Nothing
        (correct:_) -> Just correct

-- Main function to solve the error correction problem
errorCorrection :: [String] -> [(String, String)]
errorCorrection reads = 
    let correctReads = map head $ groupBy (==) $ sort reads
        allReads = nub reads
        erroneousReads = filter (\read -> not (read `elem` correctReads)) allReads
        corrections = map (\read -> (read, fromJust $ findCorrection read correctReads)) erroneousReads
    in corrections

-- Helper function to get the first element of a Maybe
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nothing"

-- Alternative cleaner implementation
errorCorrectionClean :: [String] -> [(String, String)]
errorCorrectionClean reads = 
    let correctReads = map head $ groupBy (==) $ sort reads
        erroneousReads = filter (\read -> not (read `elem` correctReads)) reads
        corrections = [(err, correct) | err <- erroneousReads, 
                                       correct <- correctReads, 
                                       hammingDistance err correct == 1]
    in corrections

-- Main function to process input and output results
solveErrorCorrection :: [String] -> [String]
solveErrorCorrection reads = 
    let corrections = errorCorrectionClean reads
        output = map (\(err, corr) -> err ++ " -> " ++ corr) corrections
    in output

-- More robust version that handles the exact problem requirements
correctErrors :: [String] -> [(String, String)]
correctErrors reads = 
    let -- Get all unique reads and their counts
        readCounts = map (\r -> (r, length $ filter (== r) reads)) $ nub reads
        -- Correct reads are those that appear more than once (or are unique but have corrections)
        correctReads = [read | (read, count) <- readCounts, count >= 1]
        -- Erroneous reads are those that appear only once
        erroneousReads = [read | (read, count) <- readCounts, count == 1]
        -- Find corrections for erroneous reads
        corrections = [(err, correct) | err <- erroneousReads, 
                                       correct <- correctReads, 
                                       hammingDistance err correct == 1]
    in corrections

-- Final clean implementation
errorCorrectionFinal :: [String] -> [(String, String)]
errorCorrectionFinal reads = 
    let uniqueReads = nub reads
        -- Group reads by their frequency (for correct vs erroneous)
        readGroups = groupBy (==) $ sort reads
        correctReads = map head readGroups
        -- Find reads that are not in the correct list (erroneous)
        erroneousReads = filter (\r -> not (r `elem` correctReads)) uniqueReads
        -- Find corrections
        corrections = [(err, correct) | err <- erroneousReads, 
                                       correct <- correctReads, 
                                       hammingDistance err correct == 1]
    in corrections

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let testReads = ["ACCAG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG"]
    let result = errorCorrectionFinal testReads
    mapM_ print result
```

## Key Features of the Solution

1. **Hamming Distance Calculation**: Computes the number of differences between two strings
2. **Error Detection**: Identifies reads that differ by exactly one nucleotide from correct reads
3. **Correction Logic**: Maps erroneous reads to their correct counterparts
4. **Clean Output Format**: Returns pairs of (erroneous_read, corrected_read)

## Usage Example

```haskell
-- Example input
reads = ["ACCAG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG", "ACCTG"]

-- The solution will find reads that differ by exactly one character
-- and return the corrected pairs
```

This solution efficiently handles the error correction problem by leveraging Haskell's powerful list processing capabilities and pattern matching to identify and correct sequencing errors in DNA reads.

