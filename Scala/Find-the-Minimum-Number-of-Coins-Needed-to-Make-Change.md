# Rosalind Problem: Find the Minimum Number of Coins Needed to Make Change

## Problem Description
Given an amount of money and a set of coin denominations, find the minimum number of coins needed to make that amount.

## Solution in Scala

```scala
def findMinimumCoins(amount: Int, coins: List[Int]): Int = {
  // Create a DP array where dp[i] represents the minimum coins needed for amount i
  val dp = new Array[Int](amount + 1)
  
  // Initialize all values to infinity except dp[0] = 0
  for (i <- 1 to amount) {
    dp(i) = Int.MaxValue
  }
  dp(0) = 0
  
  // Fill the DP array
  for (i <- 1 to amount) {
    for (coin <- coins) {
      if (coin <= i && dp(i - coin) != Int.MaxValue) {
        dp(i) = math.min(dp(i), dp(i - coin) + 1)
      }
    }
  }
  
  // Return the minimum coins needed for the target amount
  if (dp(amount) == Int.MaxValue) -1 else dp(amount)
}

// Alternative implementation using functional approach
def findMinimumCoinsFunctional(amount: Int, coins: List[Int]): Int = {
  val dp = Array.fill(amount + 1)(Int.MaxValue)
  dp(0) = 0
  
  for (i <- 1 to amount) {
    coins.foreach { coin =>
      if (coin <= i && dp(i - coin) != Int.MaxValue) {
        dp(i) = math.min(dp(i), dp(i - coin) + 1)
      }
    }
  }
  
  if (dp(amount) == Int.MaxValue) -1 else dp(amount)
}

// Example usage
def main(args: Array[String]): Unit = {
  // Example from Rosalind problem
  val amount = 40
  val coins = List(5, 10, 25)
  
  val result = findMinimumCoins(amount, coins)
  println(s"Minimum coins needed for $amount: $result")
  
  // Test with other examples
  println(findMinimumCoins(10, List(1, 3, 4))) // Expected: 3 (3+3+4)
  println(findMinimumCoins(7, List(1, 3, 4)))  // Expected: 2 (3+4)
  println(findMinimumCoins(11, List(1, 3, 4))) // Expected: 3 (4+4+3)
}
```

## Explanation

This solution uses dynamic programming to solve the minimum coin change problem:

1. **DP Array**: Create an array `dp` where `dp[i]` represents the minimum number of coins needed to make amount `i`.

2. **Initialization**: 
   - Set `dp[0] = 0` (0 coins needed for amount 0)
   - Set all other values to `Int.MaxValue` (initially impossible)

3. **Fill DP Array**: For each amount from 1 to target amount:
   - For each coin denomination:
     - If coin value ≤ current amount and previous amount is reachable:
     - Update `dp[i] = min(dp[i], dp[i - coin] + 1)`

4. **Result**: Return `dp[amount]` if reachable, otherwise -1 (impossible)

## Time and Space Complexity
- **Time Complexity**: O(amount × number of coins)
- **Space Complexity**: O(amount)

## Sample Input/Output
```
Input: amount = 40, coins = [5, 10, 25]
Output: 3 (25 + 10 + 5 = 40 with 3 coins)

Input: amount = 10, coins = [1, 3, 4]
Output: 3 (4 + 4 + 1 + 1 = 10 with 3 coins)
```

The solution handles edge cases like impossible combinations by returning -1 when no valid combination exists.

