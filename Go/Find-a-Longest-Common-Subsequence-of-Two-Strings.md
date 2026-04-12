# Rosalind Problem: Find a Longest Common Subsequence of Two Strings

## Problem Description
Given two strings s and t, find a longest common subsequence of s and t.

## Solution in Go

```go
package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

func main() {
    // Read input from stdin
    scanner := bufio.NewScanner(os.Stdin)
    scanner.Scan()
    s := scanner.Text()
    scanner.Scan()
    t := scanner.Text()
    
    // Find longest common subsequence
    lcs := longestCommonSubsequence(s, t)
    fmt.Println(lcs)
}

func longestCommonSubsequence(s, t string) string {
    m, n := len(s), len(t)
    
    // Create DP table
    dp := make([][]int, m+1)
    for i := range dp {
        dp[i] = make([]int, n+1)
    }
    
    // Fill the DP table
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            if s[i-1] == t[j-1] {
                dp[i][j] = dp[i-1][j-1] + 1
            } else {
                dp[i][j] = max(dp[i-1][j], dp[i][j-1])
            }
        }
    }
    
    // Reconstruct the LCS
    lcs := make([]byte, 0)
    i, j := m, n
    
    for i > 0 && j > 0 {
        if s[i-1] == t[j-1] {
            lcs = append([]byte{s[i-1]}, lcs...)
            i--
            j--
        } else if dp[i-1][j] > dp[i][j-1] {
            i--
        } else {
            j--
        }
    }
    
    return string(lcs)
}

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}
```

## Approach Explanation

1. **Dynamic Programming Approach**: 
   - Create a 2D DP table where `dp[i][j]` represents the length of the LCS of `s[0:i]` and `t[0:j]`
   - Fill the table using the recurrence relation:
     - If `s[i-1] == t[j-1]`: `dp[i][j] = dp[i-1][j-1] + 1`
     - Otherwise: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

2. **Reconstruction**:
   - Start from `dp[m][n]` and work backwards
   - If characters match, include in LCS and move diagonally
   - Otherwise, move in the direction of the larger value

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

## Example
Input:
```
AGGTAB
GXTXAYB
```

Output:
```
GTAB
```

The algorithm correctly identifies "GTAB" as the longest common subsequence between "AGGTAB" and "GXTXAYB".

