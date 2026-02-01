# Rosalind Problem: Sequence a Peptide (Sequence_a_Peptide)

## Problem Description
Given an RNA string, return the protein string encoded by it.

## Solution

```kotlin
fun main() {
    // Read input RNA sequence
    val rna = readLine()!!
    
    // Create codon to amino acid mapping
    val codonMap = mapOf(
        "UUU" to "F", "UUC" to "F", "UUA" to "L", "UUG" to "L",
        "CUU" to "L", "CUC" to "L", "CUA" to "L", "CUG" to "L",
        "AUU" to "I", "AUC" to "I", "AUA" to "I", "AUG" to "M",
        "GUU" to "V", "GUC" to "V", "GUA" to "V", "GUG" to "V",
        "UCU" to "S", "UCC" to "S", "UCA" to "S", "UCG" to "S",
        "CCU" to "P", "CCC" to "P", "CCA" to "P", "CCG" to "P",
        "ACU" to "T", "ACC" to "T", "ACA" to "T", "ACG" to "T",
        "GCU" to "A", "GCC" to "A", "GCA" to "A", "GCG" to "A",
        "UAU" to "Y", "UAC" to "Y", "UAA" to "", "UAG" to "",
        "CAU" to "H", "CAC" to "H", "CAA" to "Q", "CAG" to "Q",
        "AAU" to "N", "AAC" to "N", "AAA" to "K", "AAG" to "K",
        "GAU" to "D", "GAC" to "D", "GAA" to "E", "GAG" to "E",
        "UGU" to "C", "UGC" to "C", "UGA" to "", "UGG" to "W",
        "CGU" to "R", "CGC" to "R", "CGA" to "R", "CGG" to "R",
        "AGU" to "S", "AGC" to "S", "AGA" to "R", "AGG" to "R",
        "GGU" to "G", "GGC" to "G", "GGA" to "G", "GGG" to "G"
    )
    
    // Translate RNA to protein
    val protein = StringBuilder()
    
    // Process RNA in groups of 3 nucleotides (codons)
    for (i in 0 until rna.length step 3) {
        if (i + 3 <= rna.length) {
            val codon = rna.substring(i, i + 3)
            val aminoAcid = codonMap[codon]
            
            // Stop translation at stop codons
            if (aminoAcid.isNullOrEmpty()) {
                break
            }
            
            protein.append(aminoAcid)
        }
    }
    
    println(protein.toString())
}
```

## Explanation

1. **Input Reading**: Read the RNA sequence from standard input
2. **Codon Mapping**: Create a map that translates each 3-nucleotide codon to its corresponding amino acid
3. **Translation Process**: 
   - Process the RNA string in groups of 3 nucleotides (codons)
   - Look up each codon in the mapping table
   - Append the corresponding amino acid to the result
   - Stop translation when encountering a stop codon (UAA, UAG, or UGA)
4. **Output**: Print the resulting protein sequence

## Key Points

- The genetic code is read in triplets (codons)
- Stop codons (UAA, UAG, UGA) terminate translation
- The translation stops immediately when a stop codon is encountered
- The solution handles the standard genetic code table with all 20 amino acids and stop codons

## Example

Input: `AUGGCCAUGGCAAA`
Output: `MAMPA`

This represents the translation of the RNA sequence where:
- AUG → M (Methionine)
- GCC → A (Alanine) 
- AUG → M (Methionine)
- GCA → A (Alanine)
- AAA → K (Lysine)

