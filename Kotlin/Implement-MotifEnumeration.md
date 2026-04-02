# Rosalind Problem: Implement_MotifEnumeration in Kotlin

## Problem Description
Given a collection of strings DNA and an integer k, find all k-mers that appear in every string with at most d mismatches.

## Solution

```kotlin
fun main() {
    // Example input
    val dnaStrings = listOf(
        "ATTTGGC",
        "ATCGGGN",
        "ATTTGGC"
    )
    val k = 3
    val d = 1
    
    val motifs = motifEnumeration(dnaStrings, k, d)
    println(motifs.joinToString(" "))
}

fun motifEnumeration(dnaStrings: List<String>, k: Int, d: Int): List<String> {
    val patterns = mutableSetOf<String>()
    val kmers = mutableListOf<String>()
    
    // Generate all k-mers from the first DNA string
    val firstString = dnaStrings[0]
    for (i in 0..firstString.length - k) {
        kmers.add(firstString.substring(i, i + k))
    }
    
    // For each k-mer, generate all d-mismatches and check if they appear in all strings
    for (pattern in kmers) {
        val neighbors = neighbors(pattern, d)
        for (neighbor in neighbors) {
            if (isPatternInAllStrings(neighbor, dnaStrings, k, d)) {
                patterns.add(neighbor)
            }
        }
    }
    
    return patterns.toList()
}

fun neighbors(pattern: String, d: Int): List<String> {
    if (d == 0) return listOf(pattern)
    
    val nucleotides = listOf('A', 'C', 'G', 'T')
    val neighbors = mutableListOf<String>()
    
    // Base case: if d = 0, return the pattern itself
    if (d == 0) {
        neighbors.add(pattern)
        return neighbors
    }
    
    // Recursive case
    val suffix = pattern.substring(1)
    val suffixNeighbors = neighbors(suffix, d)
    
    for (neighbor in suffixNeighbors) {
        if (hammingDistance(pattern.substring(0, 1), neighbor.substring(0, 1)) < d) {
            for (nucleotide in nucleotides) {
                neighbors.add(nucleotide.toString() + neighbor)
            }
        } else {
            neighbors.add(pattern.substring(0, 1) + neighbor)
        }
    }
    
    return neighbors
}

fun isPatternInAllStrings(pattern: String, dnaStrings: List<String>, k: Int, d: Int): Boolean {
    for (dna in dnaStrings) {
        if (!isPatternInString(pattern, dna, k, d)) {
            return false
        }
    }
    return true
}

fun isPatternInString(pattern: String, dna: String, k: Int, d: Int): Boolean {
    for (i in 0..dna.length - k) {
        val substring = dna.substring(i, i + k)
        if (hammingDistance(pattern, substring) <= d) {
            return true
        }
    }
    return false
}

fun hammingDistance(p: String, q: String): Int {
    var distance = 0
    for (i in p.indices) {
        if (p[i] != q[i]) {
            distance++
        }
    }
    return distance
}
```

## Alternative Implementation (More Efficient)

```kotlin
fun main() {
    // Example input
    val dnaStrings = listOf(
        "ATTTGGC",
        "ATCGGGN", 
        "ATTTGGC"
    )
    val k = 3
    val d = 1
    
    val motifs = motifEnumerationOptimized(dnaStrings, k, d)
    println(motifs.joinToString(" "))
}

fun motifEnumerationOptimized(dnaStrings: List<String>, k: Int, d: Int): List<String> {
    val allKmers = mutableSetOf<String>()
    val candidates = mutableSetOf<String>()
    
    // Generate all k-mers from all DNA strings
    for (dna in dnaStrings) {
        for (i in 0..dna.length - k) {
            allKmers.add(dna.substring(i, i + k))
        }
    }
    
    // For each k-mer, check if it appears in all strings with at most d mismatches
    for (pattern in allKmers) {
        if (isPatternInAllStrings(pattern, dnaStrings, k, d)) {
            candidates.add(pattern)
        }
    }
    
    return candidates.toList()
}

fun isPatternInAllStrings(pattern: String, dnaStrings: List<String>, k: Int, d: Int): Boolean {
    for (dna in dnaStrings) {
        if (!isPatternInString(pattern, dna, k, d)) {
            return false
        }
    }
    return true
}

fun isPatternInString(pattern: String, dna: String, k: Int, d: Int): Boolean {
    // Check all k-mers in dna string
    for (i in 0..dna.length - k) {
        val substring = dna.substring(i, i + k)
        if (hammingDistance(pattern, substring) <= d) {
            return true
        }
    }
    return false
}

fun hammingDistance(p: String, q: String): Int {
    var distance = 0
    for (i in p.indices) {
        if (p[i] != q[i]) {
            distance++
        }
    }
    return distance
}
```

## Explanation

This solution implements the Motif Enumeration algorithm to find all k-mers that appear in every DNA string with at most d mismatches:

1. **Input Processing**: Takes a list of DNA strings, k (k-mer length), and d (maximum mismatches)

2. **Pattern Generation**: 
   - Generate all k-mers from the first DNA string
   - For each k-mer, generate all possible patterns with up to d mismatches

3. **Validation**: 
   - Check if each pattern appears in all DNA strings with at most d mismatches
   - Return all valid patterns

4. **Helper Functions**:
   - `hammingDistance`: Calculates the number of differences between two strings
   - `isPatternInString`: Checks if a pattern exists in a DNA string with at most d mismatches
   - `isPatternInAllStrings`: Verifies if a pattern exists in all DNA strings

The time complexity is O(n × m × k × 4^k) where n is the number of DNA strings, m is the average length, and k is the k-mer length.

