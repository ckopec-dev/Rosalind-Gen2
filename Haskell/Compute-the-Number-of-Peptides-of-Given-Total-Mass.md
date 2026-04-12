# Rosalind Problem: Compute the Number of Peptides of Given Total Mass

## Problem Description
Given an integer m, compute the number of peptides of total mass m. The mass of a peptide is the sum of the masses of its amino acids.

## Solution in Haskell

```haskell
-- Compute the number of peptides of given total mass
countPeptides :: Int -> Int
countPeptides m = dp !! m
  where
    -- Masses of amino acids (rounded to integers)
    masses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 
              128, 129, 131, 137, 147, 156, 163, 186]
    
    -- Dynamic programming array
    dp = 1 : map count [1..m]
    
    count n = sum [dp !! (n - mass) | mass <- masses, n >= mass]

-- Alternative implementation using more explicit DP
countPeptides' :: Int -> Int
countPeptides' m = dpArray !! m
  where
    masses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 
              128, 129, 131, 137, 147, 156, 163, 186]
    
    -- Create DP array
    dpArray = 1 : [countMass i | i <- [1..m]]
    
    countMass n = sum [dpArray !! (n - mass) | mass <- masses, n >= mass]

-- More efficient version using explicit memoization
countPeptidesEfficient :: Int -> Int
countPeptidesEfficient m = result
  where
    masses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 
              128, 129, 131, 137, 147, 156, 163, 186]
    
    -- Initialize DP array
    dp = 1 : replicate m 0
    
    -- Fill the DP array
    result = dp !! m
    
    -- This is a simplified approach - we need to build the array properly
    dpArray = buildDP dp
    buildDP dpArray = dpArray
    
    -- Proper implementation
    dpArray = 1 : [sum [dpArray !! (i - mass) | mass <- masses, i >= mass] | i <- [1..m]]

-- Correct implementation
countPeptidesCorrect :: Int -> Int
countPeptidesCorrect m = dpArray !! m
  where
    masses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 
              128, 129, 131, 137, 147, 156, 163, 186]
    
    -- Build DP array bottom-up
    dp = 1 : [sum [dp !! (i - mass) | mass <- masses, i >= mass] | i <- [1..m]]
    dpArray = dp

-- Final clean implementation
countPeptidesFinal :: Int -> Int
countPeptidesFinal m = dp !! m
  where
    masses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 
              128, 129, 131, 137, 147, 156, 163, 186]
    
    dp = 1 : [sum [dp !! (i - mass) | mass <- masses, i >= mass] | i <- [1..m]]
```

## Usage Example

```haskell
-- Example usage
main :: IO ()
main = do
    -- For mass 1000
    let result = countPeptidesFinal 1000
    print result
    
    -- Test with smaller values
    print $ countPeptidesFinal 0   -- Should be 1
    print $ countPeptidesFinal 57  -- Should be 1
    print $ countPeptidesFinal 128 -- Should be 2 (57+71 or 128)
```

## Explanation

This is a classic dynamic programming problem similar to the coin change problem. The approach is:

1. **Base case**: There is exactly 1 way to make mass 0 (empty peptide)
2. **Recurrence relation**: For mass `i`, the number of ways is the sum of ways to make `i - mass` for each possible amino acid mass
3. **Build up**: Compute values from 0 to the target mass using previously computed values

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m is the target mass and n is the number of amino acids (18)
- **Space Complexity**: O(m) for the DP array

The solution correctly handles the peptide counting problem by treating it as a combination counting problem where each amino acid contributes to the total mass.

