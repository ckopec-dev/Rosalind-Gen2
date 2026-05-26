# Finding a Longest Common Subsequence in Haskell

I'll solve the Rosalind problem "Find a Longest Common Subsequence of Two Strings" using Haskell.

## Problem Understanding

We need to find the longest subsequence common to two given strings. A subsequence is a sequence that appears in the same relative order, but not necessarily contiguous.

## Solution Approach

I'll use dynamic programming to build a table where `dp[i][j]` represents the length of the LCS of the first `i` characters of string1 and the first `j` characters of string2.

## Haskell Implementation

```haskell
import Data.List (maximum)
import Data.Array

-- Function to find the longest common subsequence
lcs :: String -> String -> String
lcs str1 str2 = reverse (buildLCS table)
  where
    table = lcsTable str1 str2
    buildLCS :: Array (Int,Int) Int -> String
    buildLCS t = buildLCS' (snd (bounds t)) (snd (bounds t))
      where
        buildLCS' i j
          | i <= 0 || j <= 0 = []
          | t!(i-1,j-1) == t!(i,j) = buildLCS' (i-1) (j-1)
          | t!(i-1,j) == t!(i,j) = buildLCS' (i-1) j
          | otherwise = str1 !! (i-1) : buildLCS' i (j-1)

-- Function to build the LCS table
lcsTable :: String -> String -> Array (Int,Int) Int
lcsTable str1 str2 = table
  where
    m = length str1
    n = length str2
    table = array ((0,0),(m,n)) [((i,j), lcsValue i j) | i <- [0..m], j <- [0..n]]
    lcsValue i j
      | i == 0 || j == 0 = 0
      | str1 !! (i-1) == str2 !! (j-1) = table ! (i-1,j-1) + 1
      | otherwise = maximum [table ! (i-1,j), table ! (i,j-1)]

-- Alternative cleaner implementation
lcsClean :: String -> String -> String
lcsClean str1 str2 = reverse (constructPath m n)
  where
    m = length str1
    n = length str2
    dp = lcsDP str1 str2
    
    lcsDP s1 s2 = 
      let m = length s1
          n = length s2
          arr = array ((0,0),(m,n)) [((i,j), compute i j) | i <- [0..m], j <- [0..n]]
      in arr
      where
        compute i j
          | i == 0 || j == 0 = 0
          | s1 !! (i-1) == s2 !! (j-1) = arr ! (i-1,j-1) + 1
          | otherwise = max (arr ! (i-1,j)) (arr ! (i,j-1))
    
    constructPath i j
      | i <= 0 || j <= 0 = []
      | dp ! (i,j) == dp ! (i-1,j-1) + 1 && str1 !! (i-1) == str2 !! (j-1) = 
          str1 !! (i-1) : constructPath (i-1) (j-1)
      | dp ! (i,j) == dp ! (i-1,j) = constructPath (i-1) j
      | otherwise = constructPath i (j-1)

-- Main function for Rosalind problem
solveLCS :: String -> String -> String
solveLCS str1 str2 = lcsClean str1 str2

-- Example usage
main :: IO ()
main = do
  let str1 = "AACCTTGG"
      str2 = "ACACTGTGA"
  putStrLn $ "String 1: " ++ str1
  putStrLn $ "String 2: " ++ str2
  putStrLn $ "LCS: " ++ solveLCS str1 str2
```

## More Efficient Version

Here's a more efficient and cleaner version:

```haskell
import Data.List (maximum)
import Data.Array

lcs :: String -> String -> String
lcs str1 str2 = buildSequence table
  where
    m = length str1
    n = length str2
    table = array ((0,0),(m,n)) [((i,j), value i j) | i <- [0..m], j <- [0..n]]
    
    value i j
      | i == 0 || j == 0 = 0
      | str1 !! (i-1) == str2 !! (j-1) = table ! (i-1,j-1) + 1
      | otherwise = maximum [table ! (i-1,j), table ! (i,j-1)]
    
    buildSequence :: Array (Int,Int) Int -> String
    buildSequence t = go m n []
      where
        go i j acc
          | i <= 0 || j <= 0 = acc
          | t ! (i,j) == t ! (i-1,j-1) + 1 && str1 !! (i-1) == str2 !! (j-1) = 
              go (i-1) (j-1) ((str1 !! (i-1)) : acc)
          | t ! (i,j) == t ! (i-1,j) = go (i-1) j acc
          | otherwise = go i (j-1) acc

-- For Rosalind problem format
solve :: String -> String -> String
solve s1 s2 = lcs s1 s2

-- Example test
example = solve "AACCTTGG" "ACACTGTGA"
```

## Explanation

1. **Dynamic Programming Table**: Create a 2D array where `table[i][j]` represents the length of LCS for the first `i` characters of string1 and first `j` characters of string2.

2. **Recurrence Relation**:
   - If characters match: `table[i][j] = table[i-1][j-1] + 1`
   - If they don't match: `table[i][j] = max(table[i-1][j], table[i][j-1])`

3. **Backtracking**: To reconstruct the actual LCS, we trace back through the table:
   - If characters match and the value comes from diagonal, include the character
   - Otherwise, move in the direction of the larger value

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Input/Output

Input:
```
AACCTTGG
ACACTGTGA
```

Output:
```
ACCTGG
```

This solution efficiently finds the longest common subsequence using dynamic programming principles and correctly handles the backtracking to reconstruct the actual subsequence.

