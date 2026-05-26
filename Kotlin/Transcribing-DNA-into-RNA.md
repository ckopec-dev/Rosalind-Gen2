# Rosalind Problem: Transcribing DNA into RNA

## Problem Statement
In order to translate a DNA sequence into RNA, we need to replace each DNA nucleotide with its RNA complement:
- G → C
- C → G
- T → A
- A → U

## Solution in Kotlin

```kotlin
fun transcribeDNAtoRNA(dna: String): String {
    return dna.map { nucleotide ->
        when (nucleotide) {
            'G' -> 'C'
            'C' -> 'G'
            'T' -> 'A'
            'A' -> 'U'
            else -> throw IllegalArgumentException("Invalid DNA nucleotide: $nucleotide")
        }
    }.joinToString("")
}

fun main() {
    // Example usage
    val dnaSequence = "GCTAGCT"
    val rnaSequence = transcribeDNAtoRNA(dnaSequence)
    println("DNA: $dnaSequence")
    println("RNA: $rnaSequence")
    
    // Test with the sample dataset
    val sampleDNA = "GATGGAACTTGACTACGTAAATT"
    val sampleRNA = transcribeDNAtoRNA(sampleDNA)
    println("Sample DNA: $sampleDNA")
    println("Sample RNA: $sampleRNA")
}
```

## Alternative Implementation Using String Replace

```kotlin
fun transcribeDNAtoRNAAlternative(dna: String): String {
    return dna.replace('G', 'C')
              .replace('C', 'G')
              .replace('T', 'A')
              .replace('A', 'U')
}

fun main() {
    val dnaSequence = "GATGGAACTTGACTACGTAAATT"
    val rnaSequence = transcribeDNAtoRNAAlternative(dnaSequence)
    println("RNA sequence: $rnaSequence")
}
```

## Explanation

The solution works by:
1. Taking a DNA string as input
2. Mapping each character to its RNA complement using a `when` expression
3. Joining the resulting characters back into a string

**Time Complexity:** O(n) where n is the length of the DNA string
**Space Complexity:** O(n) for the resulting RNA string

## Test Results
For the sample input `"GATGGAACTTGACTACGTAAATT"`:
- Expected output: `"GAUGGAACTTGACUACGUAAAUU"`

The first approach using `map` and `joinToString` is more readable and maintainable, while the alternative approach using multiple `replace` calls is more concise but potentially less efficient for very long strings.

