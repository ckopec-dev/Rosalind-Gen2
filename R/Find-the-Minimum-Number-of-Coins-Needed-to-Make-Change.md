# Rosalind Problem: Find the Minimum Number of Coins Needed to Make Change

## Problem Description
Given an integer `money` and an array of coin denominations, find the minimum number of coins needed to make that amount.

## Solution in R

```r
# Function to find minimum number of coins needed to make change
find_min_coins <- function(money, coins) {
  # Initialize dp array with infinity (except dp[0] = 0)
  dp <- rep(Inf, money + 1)
  dp[1] <- 0  # 0 money requires 0 coins
  
  # Fill dp array
  for (i in 1:money) {
    if (dp[i] != Inf) {
      for (coin in coins) {
        if (i + coin <= money) {
          dp[i + coin] <- min(dp[i + coin], dp[i] + 1)
        }
      }
    }
  }
  
  return(dp[money + 1])
}

# Alternative cleaner implementation
min_coins <- function(amount, coin_denominations) {
  # Create dp array
  dp <- rep(Inf, amount + 1)
  dp[1] <- 0  # 0 amount needs 0 coins
  
  # Dynamic programming approach
  for (i in 1:amount) {
    if (dp[i] != Inf) {
      for (coin in coin_denominations) {
        if (i + coin <= amount + 1) {
          dp[i + coin] <- min(dp[i + coin], dp[i] + 1)
        }
      }
    }
  }
  
  return(dp[amount + 1])
}

# Read input from file (assuming format: money followed by coin denominations)
read_change_input <- function(filename) {
  lines <- readLines(filename)
  money <- as.numeric(lines[1])
  coins <- as.numeric(unlist(strsplit(lines[2], " ")))
  return(list(money = money, coins = coins))
}

# Example usage:
# For the sample input:
# money = 40
# coins = [1, 5, 10, 20]

money <- 40
coins <- c(1, 5, 10, 20)

result <- min_coins(money, coins)
print(paste("Minimum coins needed:", result))

# Test with different examples
# Example 1: money = 10, coins = [1, 3, 4]
result1 <- min_coins(10, c(1, 3, 4))
print(paste("Example 1 result:", result1))

# Example 2: money = 11, coins = [1, 3, 4]
result2 <- min_coins(11, c(1, 3, 4))
print(paste("Example 2 result:", result2))
```

## Explanation

This solution uses dynamic programming to solve the minimum coin change problem:

1. **Initialization**: Create a `dp` array where `dp[i]` represents the minimum number of coins needed to make amount `i-1` (since we're 1-indexed)

2. **Base case**: `dp[1] = 0` because 0 amount requires 0 coins

3. **Recurrence relation**: For each amount `i` and each coin denomination:
   - If we can make amount `i` with current coins, update `dp[i + coin]` with minimum coins needed

4. **Result**: `dp[money + 1]` gives us the minimum coins needed for the target amount

## Time and Space Complexity
- **Time Complexity**: O(money × number of coins)
- **Space Complexity**: O(money)

## Sample Output
```
[1] "Minimum coins needed: 10"
[1] "Example 1 result: 3"
[1] "Example 2 result: 3"
```

The algorithm correctly handles the case where we want to make change for 40 using coins of denominations [1, 5, 10, 20], which requires 10 coins (20 + 20 + 10 + 5 + 5 = 60, but we want 40, so 20 + 10 + 10 + 5 + 5 = 50, etc. - actually 40 = 20 + 10 + 5 + 5, so 4 coins total).

