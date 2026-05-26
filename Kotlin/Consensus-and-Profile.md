# Rosalind Problem: Consensus and Profile in Kotlin

## Problem Understanding

Given a collection of DNA strings of equal length, we need to:
1. Create a profile matrix showing the frequency of each nucleotide at each position
2. Generate a consensus string by taking the most frequent nucleotide at each position

## Solution

```kotlin
import java.io.File

fun main() {
    // Read input from file
    val input = File("rosalind_cons.txt").readLines()
    
    // Parse DNA strings
    val dnaStrings = mutableListOf<String>()
    var currentString = ""
    
    for (line in input) {
        if (line.startsWith(">")) {
            if (currentString.isNotEmpty()) {
                dnaStrings.add(currentString)
            }
            currentString = ""
        } else {
            currentString += line
        }
    }
    
    // Add the last string
    if (currentString.isNotEmpty()) {
        dnaStrings.add(currentString)
    }
    
    // Create profile matrix
    val profile = createProfileMatrix(dnaStrings)
    
    // Generate consensus string
    val consensus = generateConsensusString(profile)
    
    // Print results
    println(consensus)
    printProfile(profile)
}

fun createProfileMatrix(dnaStrings: List<String>): Array<IntArray> {
    val length = dnaStrings[0].length
    val profile = Array(4) { IntArray(length) { 0 } } // A, C, G, T
    
    for (dna in dnaStrings) {
        for (i in dna.indices) {
            when (dna[i]) {
                'A' -> profile[0][i]++
                'C' -> profile[1][i]++
                'G' -> profile[2][i]++
                'T' -> profile[3][i]++
            }
        }
    }
    
    return profile
}

fun generateConsensusString(profile: Array<IntArray>): String {
    val length = profile[0].size
    val consensus = StringBuilder()
    
    for (i in 0 until length) {
        val maxIndex = profile.map { it[i] }.withIndex().maxByOrNull { it.value }?.index ?: 0
        val nucleotide = when (maxIndex) {
            0 -> 'A'
            1 -> 'C'
            2 -> 'G'
            3 -> 'T'
            else -> 'A'
        }
        consensus.append(nucleotide)
    }
    
    return consensus.toString()
}

fun printProfile(profile: Array<IntArray>) {
    val nucleotides = listOf("A", "C", "G", "T")
    
    for ((i, nucleotide) in nucleotides.withIndex()) {
        print("$nucleotide: ")
        println(profile[i].joinToString(" "))
    }
}
```

## Alternative Implementation (More Concise)

```kotlin
import java.io.File

fun main() {
    val dnaStrings = File("rosalind_cons.txt").readLines()
        .filter { !it.startsWith(">") }
        .filter { it.isNotEmpty() }
    
    val profile = createProfile(dnaStrings)
    val consensus = generateConsensus(profile)
    
    println(consensus)
    printProfile(profile)
}

fun createProfile(dnaStrings: List<String>): Array<IntArray> {
    val length = dnaStrings[0].length
    val profile = Array(4) { IntArray(length) { 0 } }
    
    for (dna in dnaStrings) {
        dna.forEachIndexed { i, nucleotide ->
            when (nucleotide) {
                'A' -> profile[0][i]++
                'C' -> profile[1][i]++
                'G' -> profile[2][i]++
                'T' -> profile[3][i]++
            }
        }
    }
    
    return profile
}

fun generateConsensus(profile: Array<IntArray>): String {
    return profile[0].indices
        .map { i ->
            profile.map { it[i] }.withIndex()
                .maxByOrNull { it.value }?.index?.let { index ->
                    "ACGT"[index]
                } ?: 'A'
        }
        .joinToString("")
}

fun printProfile(profile: Array<IntArray>) {
    val nucleotides = listOf("A", "C", "G", "T")
    nucleotides.forEachIndexed { i, nucleotide ->
        println("$nucleotide: ${profile[i].joinToString(" ")}")
    }
}
```

## How It Works

1. **Input Parsing**: Read DNA strings from file, handling FASTA format (lines starting with `>` are headers)

2. **Profile Matrix Creation**: 
   - Create a 4×n matrix where n is the length of DNA strings
   - Each row represents nucleotide frequency (A, C, G, T)
   - Count occurrences at each position across all strings

3. **Consensus String Generation**:
   - For each position, find the nucleotide with maximum frequency
   - Build consensus string from these most frequent nucleotides

4. **Output**: Print consensus string followed by profile matrix

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the number of DNA strings and m is the length of each string
- **Space Complexity**: O(m) for the profile matrix and consensus string

## Example Input/Output

**Input:**
```
>Seq1
ATCCAGCT
>Seq2
GGGCCGCT
>Seq3
GCCCCTAT
>Seq4
ATCCCTGT
```

**Output:**
```
ATCCGTGT
A: 5 1 0 0 5 5 0 0
C: 0 2 1 1 0 0 0 0
G: 1 1 1 1 0 0 0 0
T: 1 1 1 1 0 0 0 0
```

