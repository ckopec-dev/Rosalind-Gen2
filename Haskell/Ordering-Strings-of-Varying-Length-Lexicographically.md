# Rosalind Problem: Ordering Strings of Varying Length Lexicographically

## Problem Description
Given a collection of strings of varying lengths, return them in lexicographic order (dictionary order).

## Solution in Haskell

```haskell
import Data.List (sort)

-- Function to sort strings lexicographically
lexicographicSort :: [String] -> [String]
lexicographicSort = sort

-- Main function to solve the problem
solve :: [String] -> [String]
solve strings = lexicographicSort strings

-- Example usage
main :: IO ()
main = do
    -- Example input
    let inputStrings = ["AAA", "AA", "A", "T", "C", "G"]
    let result = solve inputStrings
    print result
    
    -- Another example
    let inputStrings2 = ["ACGT", "ACG", "AC", "A", "T", "C", "G"]
    let result2 = solve inputStrings2
    print result2
```

## Alternative Implementation with Custom Comparison

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- Custom lexicographic comparison
lexicographicSortCustom :: [String] -> [String]
lexicographicSortCustom = sortBy compare

-- More explicit version using comparing
lexicographicSortExplicit :: [String] -> [String]
lexicographicSortExplicit = sortBy (comparing id)

-- For more complex scenarios where you need to handle specific ordering rules
-- This version handles the exact lexicographic ordering
lexicographicSortExact :: [String] -> [String]
lexicographicSortExact = sort

-- Example with sample data
sampleInput :: [String]
sampleInput = ["D", "C", "B", "A", "AA", "AB", "BA", "BB"]

sampleOutput :: [String]
sampleOutput = solve sampleInput

-- Function to read input from file (if needed)
readStringsFromFile :: FilePath -> IO [String]
readStringsFromFile filename = do
    content <- readFile filename
    return $ lines content

-- Function to write output to file
writeStringsToFile :: FilePath -> [String] -> IO ()
writeStringsToFile filename strings = do
    let output = unlines strings
    writeFile filename output
```

## Complete Working Solution

```haskell
import Data.List (sort)

-- Problem: Ordering Strings of Varying Length Lexicographically
-- Given: A collection of strings of varying lengths
-- Return: The strings in lexicographic order

orderingStrings :: [String] -> [String]
orderingStrings = sort

-- Example usage with test data
main :: IO ()
main = do
    -- Test with sample data
    let testStrings = ["D", "C", "B", "A", "AA", "AB", "BA", "BB"]
    let sortedStrings = orderingStrings testStrings
    putStrLn "Original strings:"
    mapM_ putStrLn testStrings
    putStrLn "\nSorted strings:"
    mapM_ putStrLn sortedStrings
    
    -- Another example
    let example = ["ACGT", "ACG", "AC", "A", "T", "C", "G"]
    let result = orderingStrings example
    putStrLn "\nExample:"
    mapM_ putStrLn result
```

## Key Points

1. **Lexicographic Order**: Haskell's `sort` function naturally implements lexicographic ordering for strings
2. **String Comparison**: Strings are compared character by character based on ASCII values
3. **Length Independence**: The sorting works regardless of string lengths
4. **Stability**: The sort is stable, maintaining relative order of equal elements

## Time Complexity
- **Time**: O(n × m × log n) where n is the number of strings and m is the average string length
- **Space**: O(n × m) for storing the strings

## Sample Input/Output

**Input**: `["D", "C", "B", "A", "AA", "AB", "BA", "BB"]`

**Output**: `["A", "AA", "AB", "B", "BA", "BB", "C", "D"]`

The solution leverages Haskell's built-in sorting functionality which handles lexicographic ordering naturally for strings.

