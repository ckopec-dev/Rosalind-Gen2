# Rosalind Problem: Trim_a_Peptide_Leaderboard

## Problem Description
Given a collection of peptides and their scores, we need to trim the leaderboard to keep only the top N peptides based on their scores.

## Solution in Kotlin

```kotlin
fun trimLeaderboard(leaderboard: List<String>, scores: List<Double>, N: Int): List<String> {
    // Create a list of pairs (peptide, score) and sort by score in descending order
    val peptideScorePairs = leaderboard.zip(scores)
        .sortedByDescending { it.second }
    
    // Take the first N elements (or all if less than N)
    val trimmedPairs = peptideScorePairs.take(N)
    
    // Extract just the peptides from the trimmed pairs
    return trimmedPairs.map { it.first }
}

// Alternative implementation using a more explicit approach
fun trimLeaderboardExplicit(leaderboard: List<String>, scores: List<Double>, N: Int): List<String> {
    // Create a list of indices sorted by scores in descending order
    val sortedIndices = scores.indices
        .sortedByDescending { scores[it] }
    
    // Take the first N indices
    val topIndices = sortedIndices.take(N)
    
    // Get the corresponding peptides
    return topIndices.map { leaderboard[it] }
}

// Example usage
fun main() {
    val leaderboard = listOf("LAST", "ALST", "TSLA", "SLAT", "STAL")
    val scores = listOf(8.0, 7.0, 6.0, 5.0, 4.0)
    val N = 3
    
    val result = trimLeaderboard(leaderboard, scores, N)
    println(result) // Output: [LAST, ALST, TSLA]
}
```

## Explanation

The solution works as follows:

1. **Pair peptides with scores**: We zip the leaderboard peptides with their corresponding scores to create pairs.

2. **Sort by scores**: We sort these pairs in descending order of scores (highest first).

3. **Take top N**: We take the first N elements from the sorted list.

4. **Extract peptides**: We map the pairs back to just the peptide strings.

## Key Features

- **Efficient sorting**: Uses Kotlin's built-in sorting functions
- **Handles edge cases**: Works correctly when N is larger than the number of peptides
- **Type safety**: Uses proper Kotlin types for clarity
- **Functional approach**: Leverages Kotlin's functional programming features

## Time Complexity
- O(n log n) for sorting where n is the number of peptides
- O(N) for taking the first N elements
- Overall: O(n log n)

## Space Complexity
- O(n) for storing the pairs and intermediate results

This solution efficiently trims the peptide leaderboard to keep only the top-scoring peptides as required by the Rosalind problem.

