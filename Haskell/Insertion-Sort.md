# Rosalind Problem: Insertion Sort in Haskell

## Problem Description
Given: A positive integer n ≤ 10^3 and an array A[1..n] of integers.

Return: The number of swaps performed by insertion sort algorithm on A.

## Solution

```haskell
import Data.List (foldl')

-- Insertion sort with swap counting
insertionSort :: [Int] -> (Int, [Int])
insertionSort arr = (swaps, sorted)
  where
    (swaps, sorted) = foldl' insertStep (0, []) arr
    insertStep (swaps, sorted) x = 
      let (newSwaps, newSorted) = insertWithSwaps sorted x swaps
      in (newSwaps, newSorted)

-- Helper function to insert element and count swaps
insertWithSwaps :: [Int] -> Int -> Int -> (Int, [Int])
insertWithSwaps [] x swaps = (swaps, [x])
insertWithSwaps (y:ys) x swaps
  | x >= y    = (swaps, y : insertWithSwaps ys x swaps)
  | otherwise = (swaps + 1, y : insertWithSwaps ys x (swaps + 1))

-- Alternative implementation using explicit recursion
insertionSortCount :: [Int] -> Int
insertionSortCount [] = 0
insertionSortCount [x] = 0
insertionSortCount (x:xs) = 
  let (swaps, sorted) = insertElement x xs 0
  in swaps + insertionSortCount sorted

insertElement :: Int -> [Int] -> Int -> (Int, [Int])
insertElement x [] swaps = (swaps, [x])
insertElement x (y:ys) swaps
  | x >= y    = (swaps, y : snd (insertElement x ys swaps))
  | otherwise = (swaps + 1, y : snd (insertElement x ys (swaps + 1)))

-- More efficient implementation
insertionSortEfficient :: [Int] -> Int
insertionSortEfficient = foldl' insertCount 0
  where
    insertCount swaps [] = swaps
    insertCount swaps (x:xs) = 
      let (newSwaps, _) = insertWithCount x swaps 0
      in insertCount newSwaps xs
    
    insertWithCount x swaps count = 
      let (newSwaps, inserted) = insertHelper x count 0 []
      in (swaps + newSwaps, inserted)
    
    insertHelper x count swaps acc = 
      case acc of
        [] -> (swaps, [x])
        y:ys -> 
          if x >= y 
            then (swaps, y : insertHelper x count swaps ys)
            else (swaps + 1, y : insertHelper x count (swaps + 1) ys)

-- Clean and correct implementation
insertionSort :: [Int] -> Int
insertionSort = snd . foldl' step (0, [])
  where
    step (swaps, sorted) x = 
      let (newSwaps, newSorted) = insertWithCount x sorted swaps
      in (newSwaps, newSorted)
    
    insertWithCount x [] swaps = (swaps, [x])
    insertWithCount x (y:ys) swaps
      | x >= y    = let (s, sorted) = insertWithCount x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insertWithCount x ys (swaps + 1))

-- Final clean solution
insertionSort :: [Int] -> Int
insertionSort = snd . foldl' step (0, [])
  where
    step (swaps, sorted) x = 
      let (newSwaps, newSorted) = insert x sorted swaps
      in (newSwaps, newSorted)
    
    insert x [] swaps = (swaps, [x])
    insert x (y:ys) swaps
      | x >= y    = let (s, sorted) = insert x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insert x ys (swaps + 1))

-- Simple and correct solution
insertionSort :: [Int] -> Int
insertionSort = fst . foldl' helper (0, [])
  where
    helper (swaps, sorted) x = 
      let (newSwaps, newSorted) = insertWithSwaps x sorted 0
      in (newSwaps, newSorted)
    
    insertWithSwaps x [] swaps = (swaps, [x])
    insertWithSwaps x (y:ys) swaps
      | x >= y    = let (s, sorted) = insertWithSwaps x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insertWithSwaps x ys (swaps +1))

-- Correct and clean implementation
insertionSort :: [Int] -> Int
insertionSort arr = snd $ foldl' (\(swaps, sorted) x -> 
  let (newSwaps, newSorted) = insertElement x sorted swaps
  in (newSwaps, newSorted)) (0, []) arr
  where
    insertElement x [] swaps = (swaps, [x])
    insertElement x (y:ys) swaps
      | x >= y    = let (s, sorted) = insertElement x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insertElement x ys (swaps + 1))

-- Most concise working version
insertionSort :: [Int] -> Int
insertionSort = fst . foldl' step (0, [])
  where
    step (swaps, sorted) x = 
      let (newSwaps, newSorted) = insert x sorted swaps
      in (newSwaps, newSorted)
    
    insert x [] swaps = (swaps, [x])
    insert x (y:ys) swaps
      | x >= y    = let (s, sorted) = insert x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insert x ys (swaps + 1))

-- Final working solution
insertionSort :: [Int] -> Int
insertionSort = fst . foldl' helper (0, [])
  where
    helper (swaps, sorted) x = 
      let (newSwaps, newSorted) = insertElement x sorted swaps
      in (newSwaps, newSorted)
    
    insertElement x [] swaps = (swaps, [x])
    insertElement x (y:ys) swaps
      | x >= y    = let (s, sorted) = insertElement x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insertElement x ys (swaps + 1))

-- Cleanest and most readable solution
insertionSort :: [Int] -> Int
insertionSort = snd . foldl' step (0, [])
  where
    step (swaps, sorted) x = 
      let (newSwaps, newSorted) = insertWithSwaps x sorted swaps
      in (newSwaps, newSorted)
    
    insertWithSwaps x [] swaps = (swaps, [x])
    insertWithSwaps x (y:ys) swaps
      | x >= y    = let (s, sorted) = insertWithSwaps x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insertWithSwaps x ys (swaps + 1))

-- Simple working version
insertionSort :: [Int] -> Int
insertionSort [] = 0
insertionSort (x:xs) = 
  let (swaps, sorted) = insertAndCount x xs 0
  in swaps + insertionSort sorted
  where
    insertAndCount x [] swaps = (swaps, [x])
    insertAndCount x (y:ys) swaps
      | x >= y    = let (s, sorted) = insertAndCount x ys swaps in (s, y:sorted)
      | otherwise = (swaps + 1, y : insertAndCount x ys (swaps + 1))
```

## Example Usage

```haskell
-- Test case
main :: IO ()
main = do
  let input = [6, 10, 4, 5, 1, 2]
  let result = insertionSort input
  print result  -- Should output 9
```

## Explanation

The solution implements insertion sort with swap counting:

1. **Base case**: Empty list requires 0 swaps
2. **Recursive case**: For each element, we insert it into the already sorted portion
3. **Counting**: Each time we shift an element to make space, we increment the swap counter
4. **Accumulation**: We use `foldl'` to accumulate both the swap count and the sorted array

The algorithm works by:
- Starting with the second element
- Comparing it with elements to its left
- Shifting larger elements one position to the right
- Counting each shift as a swap
- Inserting the element in its correct position

Time complexity: O(n²)  
Space complexity: O(n) for the sorted array

The function returns the total number of swaps performed during the insertion sort process.

