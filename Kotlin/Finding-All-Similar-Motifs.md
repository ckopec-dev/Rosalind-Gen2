# Rosalind Problem: Finding All Similar Motifs (Kotlin Solution)

## Problem Understanding

The problem asks us to find all motifs (short DNA sequences) that appear in at least two of the given DNA sequences. This is essentially finding common subsequences that occur in multiple sequences.

## Solution Approach

1. Generate all possible substrings of a given length from each DNA sequence
2. Count how many sequences each substring appears in
3. Return all substrings that appear in at least 2 sequences

## Kotlin Implementation

```kotlin
fun findAllSimilarMotifs(sequences: List<String>, motifLength: Int): Set<String> {
    val motifCount = mutableMapOf<String, Int>()
    
    // Generate all substrings of given length from each sequence
    for (sequence in sequences) {
        val substrings = mutableSetOf<String>()
        
        // Generate all substrings of motifLength from current sequence
        for (i in 0..sequence.length - motifLength) {
            val substring = sequence.substring(i, i + motifLength)
            substrings.add(substring)
        }
        
        // Count how many sequences each unique substring appears in
        for (substring in substrings) {
            motifCount[substring] = motifCount.getOrDefault(substring, 0) + 1
        }
    }
    
    // Return all motifs that appear in at least 2 sequences
    return motifCount.filter { it.value >= 2 }.keys.toSet()
}

// Alternative more concise implementation
fun findAllSimilarMotifsConcise(sequences: List<String>, motifLength: Int): Set<String> {
    val allMotifs = sequences.flatMap { sequence ->
        (0..sequence.length - motifLength)
            .map { sequence.substring(it, it + motifLength) }
            .toSet()
    }.groupingBy { it }.eachCount()
    
    return allMotifs.filter { it.value >= 2 }.keys.toSet()
}

// Main function to solve the problem
fun solveFindingAllSimilarMotifs(input: List<String>): List<String> {
    // Assuming first line contains motif length, rest are sequences
    val motifLength = input[0].toInt()
    val sequences = input.drop(1)
    
    return findAllSimilarMotifs(sequences, motifLength).toList()
}

// Example usage
fun main() {
    // Example input
    val input = listOf(
        "3",  // motif length
        "AAATTT",
        "AAATGG",
        "AAATCC"
    )
    
    val result = solveFindingAllSimilarMotifs(input)
    println("Similar motifs: $result")
    
    // Expected output: [AAA, AAT, AAG, AAC, AAT, AAT, TTT, TGG, TCC]
    // But since we want unique motifs appearing in at least 2 sequences
    // The actual result should be: [AAA, AAT]
}
```

## Explanation

The solution works as follows:

1. **Input Processing**: The first line contains the motif length, and subsequent lines contain DNA sequences.

2. **Substring Generation**: For each DNA sequence, we generate all possible substrings of the specified length.

3. **Counting Occurrences**: We count how many different sequences each unique substring appears in.

4. **Filtering**: We return only those substrings that appear in at least 2 sequences.

## Time and Space Complexity

- **Time Complexity**: O(n × m × k) where n is the number of sequences, m is the average sequence length, and k is the motif length
- **Space Complexity**: O(n × m × k) for storing all substrings

## Sample Test Case

For input:
```
3
AAATTT
AAATGG
AAATCC
```

Expected output:
```
[AAA, AAT]
```

This represents the motifs that appear in at least 2 of the 3 sequences.

## Key Features

1. Handles multiple DNA sequences
2. Finds motifs of specified length
3. Returns only unique motifs appearing in at least 2 sequences
4. Efficiently counts motif occurrences across sequences
5. Clean and readable Kotlin implementation

