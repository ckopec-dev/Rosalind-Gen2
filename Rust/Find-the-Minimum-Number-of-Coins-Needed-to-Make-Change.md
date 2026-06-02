# Rosalind Problem: Find the Minimum Number of Coins Needed to Make Change

## Problem Description
Given an integer money and an array of coin denominations, find the minimum number of coins needed to make that amount of money.

## Solution

```rust
fn main() {
    // Read input
    let money = 40;
    let coins = vec![5, 4, 3, 2, 1];
    
    // Solve the problem
    let min_coins = compute_min_coins(money, &coins);
    
    println!("Minimum coins needed: {}", min_coins);
}

fn compute_min_coins(money: usize, coins: &[usize]) -> usize {
    // Create a DP array where dp[i] represents the minimum coins needed for amount i
    let mut dp = vec![usize::MAX; money + 1];
    
    // Base case: 0 coins needed for amount 0
    dp[0] = 0;
    
    // For each amount from 1 to money
    for amount in 1..=money {
        // Try each coin denomination
        for &coin in coins {
            // If the coin value is not greater than the current amount
            if coin <= amount {
                // Update dp[amount] with minimum of current value and dp[amount - coin] + 1
                dp[amount] = dp[amount].min(dp[amount - coin] + 1);
            }
        }
    }
    
    // Return the minimum coins needed for the target amount
    dp[money]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_min_coins() {
        // Test case 1: money = 40, coins = [5, 4, 3, 2, 1]
        let result = compute_min_coins(40, &[5, 4, 3, 2, 1]);
        assert_eq!(result, 8); // 8 coins of 5 each
        
        // Test case 2: money = 3, coins = [1, 3]
        let result = compute_min_coins(3, &[1, 3]);
        assert_eq!(result, 1); // 1 coin of 3
        
        // Test case 3: money = 6, coins = [1, 3, 4]
        let result = compute_min_coins(6, &[1, 3, 4]);
        assert_eq!(result, 2); // 2 coins of 3 each
        
        // Test case 4: money = 1, coins = [1, 3, 4]
        let result = compute_min_coins(1, &[1, 3, 4]);
        assert_eq!(result, 1); // 1 coin of 1
    }
}
```

## Explanation

This solution uses dynamic programming to solve the minimum coin change problem:

1. **DP Array**: Create an array `dp` where `dp[i]` represents the minimum number of coins needed to make amount `i`.

2. **Base Case**: `dp[0] = 0` because zero coins are needed to make amount 0.

3. **Fill DP Array**: For each amount from 1 to the target amount:
   - Try each coin denomination
   - If the coin value is ≤ current amount, update the minimum coins needed
   - `dp[amount] = min(dp[amount], dp[amount - coin] + 1)`

4. **Result**: Return `dp[money]` which contains the minimum coins needed.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m is the target amount and n is the number of coin denominations
- **Space Complexity**: O(m) for the DP array

## Example Walkthrough

For money = 40 and coins = [5, 4, 3, 2, 1]:
- dp[0] = 0
- dp[1] = 1 (1 coin of 1)
- dp[2] = 1 (1 coin of 2)
- dp[3] = 1 (1 coin of 3)
- dp[4] = 1 (1 coin of 4)
- dp[5] = 1 (1 coin of 5)
- dp[6] = 2 (1 coin of 5 + 1 coin of 1)
- ...continuing this process...
- dp[40] = 8 (8 coins of 5 each)

The answer is **8** coins.

