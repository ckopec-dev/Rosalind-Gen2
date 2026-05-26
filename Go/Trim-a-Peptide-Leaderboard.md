# Rosalind Problem: Trim_a_Peptide_Leaderboard

## Problem Description
Given a collection of peptides and their scores, trim the leaderboard to contain only the top N peptides by score.

## Solution

```go
package main

import (
    "fmt"
    "sort"
)

// PeptideScore represents a peptide with its score
type PeptideScore struct {
    Peptide string
    Score   int
}

// TrimPeptideLeaderboard trims the leaderboard to contain only the top N peptides
func TrimPeptideLeaderboard(leaderboard []PeptideScore, N int) []PeptideScore {
    // Sort the leaderboard in descending order by score
    sort.Slice(leaderboard, func(i, j int) bool {
        return leaderboard[i].Score > leaderboard[j].Score
    })
    
    // If N is greater than the length of leaderboard, return the entire leaderboard
    if N >= len(leaderboard) {
        return leaderboard
    }
    
    // Return the first N elements
    return leaderboard[:N]
}

// TrimPeptideLeaderboardWithTies trims the leaderboard and handles ties properly
func TrimPeptideLeaderboardWithTies(leaderboard []PeptideScore, N int) []PeptideScore {
    // Sort the leaderboard in descending order by score
    sort.Slice(leaderboard, func(i, j int) bool {
        return leaderboard[i].Score > leaderboard[j].Score
    })
    
    // If N is greater than the length of leaderboard, return the entire leaderboard
    if N >= len(leaderboard) {
        return leaderboard
    }
    
    // Find the score of the Nth element
    nthScore := leaderboard[N-1].Score
    
    // Include all peptides with score >= nthScore
    result := []PeptideScore{}
    for _, ps := range leaderboard {
        if ps.Score >= nthScore {
            result = append(result, ps)
        } else {
            break
        }
    }
    
    return result
}

func main() {
    // Example usage
    leaderboard := []PeptideScore{
        {"NQEL", 85},
        {"KLWL", 77},
        {"KLW", 77},
        {"YEEK", 75},
        {"NQ", 70},
        {"KWL", 65},
        {"EEK", 65},
        {"NL", 60},
        {"EL", 55},
        {"K", 50},
    }
    
    fmt.Println("Original leaderboard:")
    for _, ps := range leaderboard {
        fmt.Printf("%s: %d\n", ps.Peptide, ps.Score)
    }
    
    // Trim to top 5
    trimmed := TrimPeptideLeaderboard(leaderboard, 5)
    fmt.Println("\nTrimmed to top 5:")
    for _, ps := range trimmed {
        fmt.Printf("%s: %d\n", ps.Peptide, ps.Score)
    }
    
    // Trim with ties
    trimmedTies := TrimPeptideLeaderboardWithTies(leaderboard, 5)
    fmt.Println("\nTrimmed to top 5 (with ties):")
    for _, ps := range trimmedTies {
        fmt.Printf("%s: %d\n", ps.Peptide, ps.Score)
    }
}
```

## Explanation

The solution provides two functions to handle the trimming of peptide leaderboards:

1. **TrimPeptideLeaderboard**: Simply sorts the leaderboard by score in descending order and returns the first N elements.

2. **TrimPeptideLeaderboardWithTies**: Sorts the leaderboard and includes all peptides that have the same score as the Nth element, properly handling ties.

### Key Features:
- **Sorting**: Uses `sort.Slice` with a custom comparison function to sort by score in descending order
- **Edge Case Handling**: Handles cases where N is greater than the leaderboard size
- **Tie Handling**: The second function ensures that peptides with equal scores are properly included
- **Efficiency**: Time complexity is O(n log n) due to sorting, where n is the number of peptides

### Input Format:
The function expects a slice of `PeptideScore` structs, each containing a peptide string and its associated score.

### Output:
Returns a trimmed slice containing only the top N peptides (or all peptides if N exceeds the leaderboard size).

This solution correctly handles the Rosalind problem requirements for trimming peptide leaderboards while maintaining proper sorting and tie handling.

