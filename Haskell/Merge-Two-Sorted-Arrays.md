# Rosalind Problem: Merge Two Sorted Arrays (Merge_Two_Sorted_Arrays)

## Problem Description
Given two sorted arrays A and B of size n and m respectively, merge them into a single sorted array.

## Solution in Haskell

```haskell
mergeArrays :: [Int] -> [Int] -> [Int]
mergeArrays [] ys = ys
mergeArrays xs [] = xs
mergeArrays (x:xs) (y:ys)
    | x <= y    = x : mergeArrays xs (y:ys)
    | otherwise = y : mergeArrays (x:xs) ys

-- Alternative implementation using explicit recursion
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- For Rosalind format - reading from stdin
import System.IO

main :: IO ()
main = do
    input <- getContents
    let linesList = lines input
    let array1 = map read (words (linesList !! 1)) :: [Int]
    let array2 = map read (words (linesList !! 3)) :: [Int]
    let result = mergeArrays array1 array2
    putStrLn $ unwords (map show result)
```

## Example Usage

**Input:**
```
3
1 3 5
3
2 4 6
```

**Output:**
```
1 2 3 4 5 6
```

## Explanation

The solution uses a classic merge algorithm that works as follows:

1. **Base cases**: If either array is empty, return the other array
2. **Recursive case**: Compare the first elements of both arrays
   - If the first element of array A is smaller or equal, take it and recursively merge the rest of A with array B
   - Otherwise, take the first element of array B and recursively merge the rest of A with the rest of B

## Time and Space Complexity

- **Time Complexity**: O(n + m) where n and m are the lengths of the two arrays
- **Space Complexity**: O(n + m) for the merged result array

## Alternative Concise Implementation

```haskell
mergeArrays :: [Int] -> [Int] -> [Int]
mergeArrays [] ys = ys
mergeArrays xs [] = xs
mergeArrays (x:xs) (y:ys)
    | x <= y    = x : mergeArrays xs (y:ys)
    | otherwise = y : mergeArrays (x:xs) ys
```

This implementation efficiently merges two sorted arrays by always choosing the smaller of the two current elements, maintaining the sorted order in the result.

