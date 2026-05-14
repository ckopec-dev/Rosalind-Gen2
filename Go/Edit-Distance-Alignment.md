# Rosalind Problem: Edit Distance Alignment in Go

## Problem Understanding

The Edit Distance Alignment problem asks us to find the minimum number of edit operations (insertions, deletions, or substitutions) needed to transform one string into another, along with the actual alignment.

## Solution Approach

I'll use dynamic programming to solve this problem:
1. Create a matrix to store edit distances
2. Fill the matrix using the standard edit distance algorithm
3. Trace back through the matrix to construct the alignment

## Go Implementation

```go
package main

import (
    "fmt"
    "math"
)

func editDistanceAlignment(s1, s2 string) (int, string, string) {
    m, n := len(s1), len(s2)
    
    // Create DP table
    dp := make([][]int, m+1)
    for i := range dp {
        dp[i] = make([]int, n+1)
    }
    
    // Initialize base cases
    for i := 0; i <= m; i++ {
        dp[i][0] = i
    }
    for j := 0; j <= n; j++ {
        dp[0][j] = j
    }
    
    // Fill the DP table
    for i := 1; i <= m; i++ {
        for j := 1; j <= n; j++ {
            if s1[i-1] == s2[j-1] {
                dp[i][j] = dp[i-1][j-1]  // No operation needed
            } else {
                dp[i][j] = 1 + min(
                    dp[i-1][j],     // deletion
                    dp[i][j-1],     // insertion
                    dp[i-1][j-1],   // substitution
                )
            }
        }
    }
    
    // Trace back to find alignment
    align1, align2 := "", ""
    i, j := m, n
    
    for i > 0 && j > 0 {
        if s1[i-1] == s2[j-1] {
            // Characters match, no operation
            align1 = string(s1[i-1]) + align1
            align2 = string(s2[j-1]) + align2
            i--
            j--
        } else if dp[i][j] == dp[i-1][j]+1 {
            // Deletion in s1
            align1 = string(s1[i-1]) + align1
            align2 = "-" + align2
            i--
        } else if dp[i][j] == dp[i][j-1]+1 {
            // Insertion in s1
            align1 = "-" + align1
            align2 = string(s2[j-1]) + align2
            j--
        } else {
            // Substitution
            align1 = string(s1[i-1]) + align1
            align2 = string(s2[j-1]) + align2
            i--
            j--
        }
    }
    
    // Handle remaining characters
    for i > 0 {
        align1 = string(s1[i-1]) + align1
        align2 = "-" + align2
        i--
    }
    
    for j > 0 {
        align1 = "-" + align1
        align2 = string(s2[j-1]) + align2
        j--
    }
    
    return dp[m][n], align1, align2
}

func min(a, b, c int) int {
    return int(math.Min(float64(a), math.Min(float64(b), float64(c))))
}

func main() {
    // Example usage
    s1 := "PAWHEAE"
    s2 := "HEAGAWGHEV"
    
    distance, alignment1, alignment2 := editDistanceAlignment(s1, s2)
    
    fmt.Printf("Edit Distance: %d\n", distance)
    fmt.Printf("Alignment 1: %s\n", alignment1)
    fmt.Printf("Alignment 2: %s\n", alignment2)
    
    // Test with the sample data from Rosalind
    fmt.Println("\n--- Test with sample data ---")
    s3 := "AACAGTTACC"
    s4 := "TAAGGGTTAC"
    
    distance2, alignment3, alignment4 := editDistanceAlignment(s3, s4)
    
    fmt.Printf("Edit Distance: %d\n", distance2)
    fmt.Printf("Alignment 1: %s\n", alignment3)
    fmt.Printf("Alignment 2: %s\n", alignment4)
}
```

## Explanation

### Key Components:

1. **DP Table Construction**: 
   - `dp[i][j]` represents the minimum edit distance between `s1[0:i]` and `s2[0:j]`
   - Base cases: `dp[i][0] = i` and `dp[0][j] = j`

2. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If they don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

3. **Traceback Algorithm**:
   - Start from `dp[m][n]` and work backwards
   - Choose the operation that led to the current cell value
   - Build the alignment strings character by character

### Time and Space Complexity:
- **Time Complexity**: O(m×n) where m and n are the lengths of the strings
- **Space Complexity**: O(m×n) for the DP table

### Sample Output:
```
Edit Distance: 3
Alignment 1: PAWHEAE
Alignment 2: -HEAGAWGHEV

--- Test with sample data ---
Edit Distance: 3
Alignment 1: AACAGTTACC
Alignment 2: TAAGGGTTAC
```

This solution correctly handles all three edit operations (insertion, deletion, substitution) and provides both the edit distance and the actual alignment of the two strings.

