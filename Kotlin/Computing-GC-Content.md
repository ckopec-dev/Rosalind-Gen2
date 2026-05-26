# Rosalind Problem: Computing GC Content - Kotlin Solution

## Problem Understanding

The task is to find the GC content (percentage of guanine and cytosine nucleotides) for each DNA sequence in a FASTA formatted dataset and identify which sequence has the highest GC content.

## Solution

```kotlin
import java.io.File
import kotlin.math.roundToInt

fun main() {
    // Read input file
    val inputFile = "rosalind_gc.txt"
    val sequences = readFastaFile(inputFile)
    
    // Find sequence with highest GC content
    val highestGC = sequences.maxByOrNull { it.gcContent }!!
    
    // Print result
    println(highestGC.id)
    println("%.6f".format(highestGC.gcContent))
}

data class DNASequence(
    val id: String,
    val sequence: String,
    val gcContent: Double
)

fun readFastaFile(filename: String): List<DNASequence> {
    val fileContent = File(filename).readText()
    val sequences = mutableListOf<DNASequence>()
    
    // Split by > to get individual sequences
    val sequenceBlocks = fileContent.split(">").filter { it.isNotBlank() }
    
    for (block in sequenceBlocks) {
        val lines = block.lines()
        if (lines.isEmpty()) continue
        
        val id = lines[0]
        val sequence = lines.drop(1).joinToString("")
        
        val gcCount = sequence.count { it == 'G' || it == 'C' }
        val gcContent = if (sequence.isNotEmpty()) {
            (gcCount.toDouble() / sequence.length) * 100
        } else {
            0.0
        }
        
        sequences.add(DNASequence(id, sequence, gcContent))
    }
    
    return sequences
}
```

## Alternative Implementation (More Robust)

```kotlin
import java.io.File

fun main() {
    val inputFile = "rosalind_gc.txt"
    val sequences = parseFastaFile(inputFile)
    
    val maxGCSeq = sequences.maxByOrNull { it.gcContent }!!
    
    println(maxGCSeq.id)
    println(String.format("%.6f", maxGCSeq.gcContent))
}

data class DNASequence(
    val id: String,
    val sequence: String,
    val gcContent: Double
)

fun parseFastaFile(filename: String): List<DNASequence> {
    val fileContent = File(filename).readText()
    val sequences = mutableListOf<DNASequence>()
    
    // Split by '>' to separate sequences
    val parts = fileContent.split(">").filter { it.isNotBlank() }
    
    for (part in parts) {
        val lines = part.lines()
        val id = lines[0].trim()
        val sequence = lines.drop(1).joinToString("").trim()
        
        val gcContent = calculateGCContent(sequence)
        
        sequences.add(DNASequence(id, sequence, gcContent))
    }
    
    return sequences
}

fun calculateGCContent(sequence: String): Double {
    if (sequence.isEmpty()) return 0.0
    
    val gcCount = sequence.count { it.uppercaseChar() in "GC" }
    return (gcCount.toDouble() / sequence.length) * 100
}
```

## Key Points

1. **Input Parsing**: The solution reads a FASTA formatted file where sequences are separated by `>`
2. **GC Content Calculation**: For each sequence, count 'G' and 'C' nucleotides and calculate percentage
3. **Output Format**: Print the sequence ID followed by GC content rounded to 6 decimal places
4. **Edge Cases**: Handles empty sequences and properly parses FASTA format

## Sample Input Format
```
>Rosalind_6404
 CCTGCGGAAGATCGGCACTAGAATCGACTCCTGAAGCGAATTCCTT
>Rosalind_5959
 ATATCCCTGTCTCGTACCCCGAGGTTTCTGCTCTTTGCGGTTTCTG
```

## Sample Output
```
Rosalind_6404
53.846154
```

The solution efficiently processes the FASTA format, calculates GC content for each sequence, and identifies the sequence with the maximum GC content.

