# Rosalind Problem RNA_Splicing Solution in Go

## Problem Understanding

RNA splicing involves removing introns (non-coding sequences) from pre-mRNA to form mature mRNA. Given a DNA string and a collection of intron strings, we need to:
1. Transcribe the DNA to RNA
2. Remove all introns from the RNA
3. Translate the remaining RNA to protein

## Solution

```go
package main

import (
    "fmt"
    "os"
    "strings"
)

// RNA codon table for translation
var codonTable = map[string]string{
    "UUU": "F", "UUC": "F", "UUA": "L", "UUG": "L",
    "CUU": "L", "CUC": "L", "CUA": "L", "CUG": "L",
    "AUU": "I", "AUC": "I", "AUA": "I", "UGG": "W",
    "GUU": "V", "GUC": "V", "GUA": "V", "GUG": "V",
    "UCU": "S", "UCC": "S", "UCA": "S", "UCG": "S",
    "CCU": "P", "CCC": "P", "CCA": "P", "CCG": "P",
    "ACU": "T", "ACC": "T", "ACA": "T", "ACG": "T",
    "GCU": "A", "GCC": "A", "GCA": "A", "GCG": "A",
    "UAU": "Y", "UAC": "Y", "UGU": "C", "UGC": "C",
    "UGA": "*", "UAA": "*", "UGG": "W",
    "GAU": "D", "GAC": "D", "GAA": "E", "GAG": "E",
    "GGA": "G", "GGC": "G", "GGA": "G", "GGG": "G",
}

// Transcribe DNA to RNA (T -> U)
func transcribe(dna string) string {
    return strings.ReplaceAll(dna, "T", "U")
}

// Remove introns from RNA sequence
func removeIntrons(rna string, introns []string) string {
    result := rna
    for _, intron := range introns {
        result = strings.ReplaceAll(result, intron, "")
    }
    return result
}

// Translate RNA to protein
func translate(rna string) string {
    var protein strings.Builder
    
    // Process RNA in groups of 3 nucleotides (codons)
    for i := 0; i < len(rna)-2; i += 3 {
        codon := rna[i : i+3]
        aminoAcid, exists := codonTable[codon]
        if !exists {
            break // Invalid codon
        }
        
        // Stop translation at stop codon
        if aminoAcid == "*" {
            break
        }
        
        protein.WriteString(aminoAcid)
    }
    
    return protein.String()
}

// Main function to solve RNA splicing problem
func solveRNA_Splicing(dna string, introns []string) string {
    // Step 1: Transcribe DNA to RNA
    rna := transcribe(dna)
    
    // Step 2: Remove introns
    rnaWithoutIntrons := removeIntrons(rna, introns)
    
    // Step 3: Translate to protein
    protein := translate(rnaWithoutIntrons)
    
    return protein
}

func main() {
    // Example input (replace with actual file reading)
    dna := "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"
    introns := []string{
        "ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG",
        "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG",
    }
    
    result := solveRNA_Splicing(dna, introns)
    fmt.Println(result)
    
    // For actual Rosalind input, you would read from file
    // Example of reading from file:
    /*
    if len(os.Args) < 2 {
        fmt.Println("Usage: go run main.go input.txt")
        return
    }
    
    content, err := os.ReadFile(os.Args[1])
    if err != nil {
        fmt.Printf("Error reading file: %v\n", err)
        return
    }
    
    // Parse input file format for Rosalind
    // ... parsing logic ...
    */
}
```

## Input Format

The input format for Rosalind's RNA_Splicing problem typically consists of:
- First line: DNA sequence (the main sequence)
- Subsequent lines: Introns (sequences to be removed)

## Example Input
```
ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
```

## Expected Output
The protein sequence after splicing and translation.

## Key Points

1. **Transcription**: DNA → RNA (T → U)
2. **Splicing**: Remove introns from the transcribed RNA
3. **Translation**: Convert RNA codons to amino acids using the genetic code
4. **Stop codons**: Translation stops when encountering a stop codon (*)
5. **Edge cases**: Handle invalid codons and ensure proper frame reading

## Time Complexity
- Transcription: O(n) where n is the length of DNA
- Intron removal: O(m × n) where m is the number of introns
- Translation: O(n/3) = O(n)

## Space Complexity
O(n) for storing the RNA and protein sequences.

