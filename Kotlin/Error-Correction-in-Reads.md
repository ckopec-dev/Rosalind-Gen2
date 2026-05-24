# Rosalind Problem: Error Correction in Reads (Kotlin Solution)

## Problem Understanding

The task is to identify and correct sequencing errors in DNA reads. Errors can occur as:
1. **Single base substitutions** (Hamming distance = 1)
2. **Reverse complements** of existing reads

We need to:
- Find reads that appear only once (potential errors)
- For each such read, check if it's a single base substitution from a read that appears more than once
- If so, output the corrected read and the original erroneous read

## Solution Approach

1. Count frequency of each read
2. Identify reads that appear only once (potential errors)
3. For each single-occurrence read, check if it's a Hamming distance of 1 from any frequent read
4. Also check reverse complements
5. Output corrections in required format

## Kotlin Implementation

```kotlin
fun main() {
    val reads = mutableListOf<String>()
    val input = generateSequence { readLine() }.takeWhile { it != null }.toList()
    
    var currentRead = ""
    for (line in input) {
        if (line.startsWith(">")) {
            if (currentRead.isNotEmpty()) {
                reads.add(currentRead)
            }
            currentRead = ""
        } else {
            currentRead += line
        }
    }
    if (currentRead.isNotEmpty()) {
        reads.add(currentRead)
    }
    
    // Count frequencies
    val freq = reads.groupingBy { it }.eachCount()
    
    // Find reads that appear only once (potential errors)
    val singleOccurrence = freq.filter { it.value == 1 }.keys.toList()
    
    // Find reads that appear more than once (correct reads)
    val frequentReads = freq.filter { it.value > 1 }.keys.toList()
    
    // Process each single occurrence read
    for (read in singleOccurrence) {
        // Check if it's a single base substitution from a frequent read
        for (correctRead in frequentReads) {
            if (hammingDistance(read, correctRead) == 1) {
                println("$read->${correctRead}")
                break
            }
        }
        
        // Check reverse complement
        val revComp = reverseComplement(read)
        for (correctRead in frequentReads) {
            if (hammingDistance(revComp, correctRead) == 1) {
                println("$read->${correctRead}")
                break
            }
        }
    }
}

fun hammingDistance(s1: String, s2: String): Int {
    if (s1.length != s2.length) return -1
    var distance = 0
    for (i in s1.indices) {
        if (s1[i] != s2[i]) {
            distance++
        }
    }
    return distance
}

fun reverseComplement(s: String): String {
    val complement = mapOf('A' to 'T', 'T' to 'A', 'C' to 'G', 'G' to 'C')
    return s.reversed().map { complement[it]!! }.joinToString("")
}
```

## Alternative Cleaner Implementation

```kotlin
fun main() {
    val reads = mutableListOf<String>()
    val input = generateSequence { readLine() }.takeWhile { it != null }.toList()
    
    // Parse FASTA format
    var currentRead = ""
    for (line in input) {
        if (line.startsWith(">")) {
            if (currentRead.isNotEmpty()) {
                reads.add(currentRead)
            }
            currentRead = ""
        } else {
            currentRead += line
        }
    }
    if (currentRead.isNotEmpty()) {
        reads.add(currentRead)
    }
    
    // Count frequencies
    val freq = reads.groupingBy { it }.eachCount()
    
    // Separate frequent and single occurrence reads
    val frequentReads = freq.filter { it.value > 1 }.keys.toList()
    val singleReads = freq.filter { it.value == 1 }.keys.toList()
    
    // Process each single occurrence read
    for (singleRead in singleReads) {
        // Check against frequent reads for single base substitution
        for (frequentRead in frequentReads) {
            if (hammingDistance(singleRead, frequentRead) == 1) {
                println("$singleRead->${frequentRead}")
                break
            }
        }
        
        // Check reverse complement against frequent reads
        val revComp = reverseComplement(singleRead)
        for (frequentRead in frequentReads) {
            if (hammingDistance(revComp, frequentRead) == 1) {
                println("$singleRead->${frequentRead}")
                break
            }
        }
    }
}

fun hammingDistance(s1: String, s2: String): Int {
    if (s1.length != s2.length) return -1
    return s1.zip(s2).count { it.first != it.second }
}

fun reverseComplement(s: String): String {
    val complement = mapOf('A' to 'T', 'T' to 'A', 'C' to 'G', 'G' to 'C')
    return s.reversed().map { complement[it]!! }.joinToString("")
}
```

## Key Features of the Solution

1. **FASTA Parsing**: Properly handles FASTA format input with > headers
2. **Frequency Counting**: Uses Kotlin's `groupingBy` and `eachCount()` for efficient counting
3. **Hamming Distance**: Calculates distance between two strings of equal length
4. **Reverse Complement**: Generates reverse complement of DNA sequences
5. **Error Correction Logic**: Checks both direct matches and reverse complement matches

## Time Complexity
- O(n × m) where n is the number of reads and m is the average read length
- Space complexity: O(n × m) for storing reads and frequencies

The solution correctly identifies sequencing errors and outputs the corrected pairs in the required format.

