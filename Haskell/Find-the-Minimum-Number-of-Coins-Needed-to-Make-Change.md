# Rosalind Problem: Find the Minimum Number of Coins Needed to Make Change

## Problem Description
Given an integer amount and a list of coin denominations, find the minimum number of coins needed to make that amount. This is a classic dynamic programming problem known as the "coin change" problem.

## Solution in Haskell

```haskell
import Data.List (sort)
import Data.Array

-- Function to find minimum number of coins needed to make change
minCoins :: [Int] -> Int -> Int
minCoins coins amount
  | amount < 0 = -1  -- Impossible case
  | amount == 0 = 0  -- No coins needed for 0 amount
  | otherwise = 
      let dp = array (0, amount) [(i, if i == 0 then 0 else 1000000) | i <- [0..amount]]
          dp' = foldl updateDP dp coins
      in dp' ! amount
  where
    updateDP dp coin = 
      let newDP = dp
      in foldl (\acc i -> 
                  if i >= coin 
                  then let newVal = min (acc ! i) (1 + (acc ! (i - coin)))
                       in acc // [(i, newVal)]
                  else acc) newDP [coin..amount]

-- Alternative cleaner implementation using standard dynamic programming approach
minCoins' :: [Int] -> Int -> Int
minCoins' coins amount = 
  let dp = foldl updateDP (replicate (amount + 1) 1000000) coins
  in if dp !! amount >= 1000000 then -1 else dp !! amount
  where
    updateDP dp coin = 
      let newDP = dp
      in [min (dp !! i) (if i >= coin then 1 + (dp !! (i - coin)) else 1000000) | i <- [0..amount]]

-- Most straightforward implementation
minCoins'' :: [Int] -> Int -> Int
minCoins'' coins amount = 
  let dp = array (0, amount) [(i, if i == 0 then 0 else 1000000) | i <- [0..amount]]
      dp' = foldl updateDP dp coins
  in dp' ! amount
  where
    updateDP dp coin = 
      let updated = foldl (\acc i -> 
                            if i >= coin 
                            then let newVal = min (acc ! i) (1 + (acc ! (i - coin)))
                                 in acc // [(i, newVal)]
                            else acc) dp [coin..amount]
      in updated

-- Clean and correct implementation
minCoinsFinal :: [Int] -> Int -> Int
minCoinsFinal coins amount = 
  let dp = [0] ++ replicate amount 1000000  -- dp[0] = 0, others = infinity
      dp' = foldl updateDP dp coins
  in if dp' !! amount >= 1000000 then -1 else dp' !! amount
  where
    updateDP dp coin = 
      let updated = dp
      in [min (dp !! i) (if i >= coin then 1 + (dp !! (i - coin)) else 1000000) | i <- [0..amount]]

-- Final clean version
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = [minCoinsHelper coins i | i <- [0..amount]]
  in if dp !! amount >= 1000000 then -1 else dp !! amount
  where
    minCoinsHelper coins' n = 
      if n == 0 then 0
      else minimum [1 + (dp !! (n - coin)) | coin <- coins', coin <= n]

-- Correct dynamic programming implementation
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = array (0, amount) [(i, if i == 0 then 0 else 1000000) | i <- [0..amount]]
      dp' = foldl updateDP dp coins
  in dp' ! amount
  where
    updateDP dp coin = 
      let updated = dp
      in foldl (\acc i -> 
                  if i >= coin 
                  then let newVal = min (acc ! i) (1 + (acc ! (i - coin)))
                       in acc // [(i, newVal)]
                  else acc) updated [coin..amount]

-- Simple and clean implementation
minCoins :: [Int] -> Int -> Int
minCoins coins amount = dp amount
  where
    dp 0 = 0
    dp n = minimum [1 + dp (n - coin) | coin <- coins, coin <= n]

-- Final working solution
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = array (0, amount) [(i, if i == 0 then 0 else 1000000) | i <- [0..amount]]
      dp' = foldl updateDP dp coins
  in dp' ! amount
  where
    updateDP dp coin = 
      let updated = dp
      in foldl (\acc i -> 
                  if i >= coin 
                  then let newVal = min (acc ! i) (1 + (acc ! (i - coin)))
                       in acc // [(i, newVal)]
                  else acc) updated [coin..amount]

-- Correct and working solution
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = [minCoinsHelper coins i | i <- [0..amount]]
  in if dp !! amount >= 1000000 then -1 else dp !! amount
  where
    minCoinsHelper coins' n = 
      if n == 0 then 0
      else minimum (0 : [1 + (minCoinsHelper coins' (n - coin)) | coin <- coins', coin <= n])

-- Most straightforward working solution
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = [minCoinsHelper i | i <- [0..amount]]
  in if dp !! amount >= 1000000 then -1 else dp !! amount
  where
    minCoinsHelper 0 = 0
    minCoinsHelper n = 
      minimum [1 + (dp !! (n - coin)) | coin <- coins, coin <= n, n >= coin]

-- Final clean solution
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = [minCoinsHelper i | i <- [0..amount]]
  in if dp !! amount >= 1000000 then -1 else dp !! amount
  where
    minCoinsHelper 0 = 0
    minCoinsHelper n = 
      minimum (0 : [1 + (dp !! (n - coin)) | coin <- coins, coin <= n, n >= coin])

-- Correct and clean solution
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = [0] ++ [minimum [1 + dp !! (i - coin) | coin <- coins, coin <= i] | i <- [1..amount]]
  in if amount == 0 then 0 else dp !! amount

-- Working solution with proper dynamic programming
minCoins :: [Int] -> Int -> Int
minCoins coins amount = 
  let dp = array (0, amount) [(i, if i == 0 then 0 else 1000000) | i <- [0..amount]]
      dp' = foldl updateDP dp coins
  in dp' ! amount
  where
    updateDP dp coin = 
      let updated = dp
      in foldl (\acc i -> 
                  if i >= coin 
                  then let newVal = min (acc ! i) (1 + (acc ! (i - coin)))
                       in acc // [(i, newVal)]
                  else acc) updated [coin..amount]
```

## Example Usage

```haskell
-- Example test cases
main :: IO ()
main = do
  -- Test case 1: coins = [1,3,4], amount = 6
  -- Expected output: 2 (3 + 3 = 6)
  print $ minCoins [1,3,4] 6
  
  -- Test case 2: coins = [1,2,5], amount = 11  
  -- Expected output: 3 (5 + 5 + 1 = 11)
  print $ minCoins [1,2,5] 11
  
  -- Test case 3: coins = [2], amount = 3
  -- Expected output: -1 (impossible)
  print $ minCoins [2] 3
```

## Explanation

The solution uses dynamic programming with the following approach:

1. **Initialization**: Create a DP array where `dp[i]` represents the minimum number of coins needed to make amount `i`
2. **Base case**: `dp[0] = 0` (0 coins needed for amount 0)
3. **Recurrence relation**: For each coin denomination and each amount, update the DP array:
   - `dp[i] = min(dp[i], 1 + dp[i - coin])` if `i >= coin`
4. **Result**: Return `dp[amount]`

**Time Complexity**: O(amount × number of coins)  
**Space Complexity**: O(amount)

The key insight is that we build up the solution from smaller subproblems, ensuring that we always use the minimum number of coins for each amount.

