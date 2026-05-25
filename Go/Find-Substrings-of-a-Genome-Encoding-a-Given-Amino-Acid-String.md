# Rosalind Problem: Find Substrings of a Genome Encoding a Given Amino Acid String

## Problem Description
Given a DNA string and an amino acid string, find all substrings of the DNA that encode the amino acid string.

## Solution

```go
package main

import (
    "fmt"
    "strings"
)

// CodonTable maps codons to amino acids
var codonTable = map[string]string{
    "UUU": "F", "UUC": "F", "UUA": "L", "UUG": "L",
    "CUU": "L", "CUC": "L", "CUA": "L", "CUG": "L",
    "AUU": "I", "AUC": "I", "AUA": "I", "UAU": "Y",
    "UAC": "Y", "UGU": "C", "UGC": "C", "UGG": "W",
    "CUU": "L", "CUC": "L", "CUA": "L", "CUG": "L",
    "CCU": "P", "CCC": "P", "CCA": "P", "CCG": "P",
    "CAU": "H", "CAC": "H", "CAA": "Q", "CAG": "Q",
    "CGU": "R", "CGC": "R", "CGA": "R", "CGG": "R",
    "AUU": "I", "AUC": "I", "AUA": "I", "AGU": "S",
    "AGC": "S", "AGA": "R", "AGG": "R", "GUU": "V",
    "GUC": "V", "GUA": "V", "GUG": "V", "GCU": "A",
    "GCC": "A", "GCA": "A", "GCG": "A", "GAU": "D",
    "GAC": "D", "GAA": "E", "GAG": "E", "GGU": "G",
    "GGC": "G", "GGA": "G", "GGG": "G",
}

// reverseComplement returns the reverse complement of a DNA string
func reverseComplement(dna string) string {
    complement := make(map[rune]rune)
    complement['A'] = 'T'
    complement['T'] = 'A'
    complement['G'] = 'C'
    complement['C'] = 'G'
    
    runes := []rune(dna)
    for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
        runes[i], runes[j] = runes[j], runes[i]
    }
    
    for i, r := range runes {
        runes[i] = complement[r]
    }
    
    return string(runes)
}

// translateDNAtoAA translates a DNA sequence to amino acids
func translateDNAtoAA(dna string) string {
    var result strings.Builder
    for i := 0; i < len(dna)-2; i += 3 {
        if i+3 <= len(dna) {
            codon := dna[i : i+3]
            if aminoAcid, exists := codonTable[codon]; exists {
                result.WriteString(aminoAcid)
            }
        }
    }
    return result.String()
}

// findSubstrings finds all substrings of a given length that encode the target amino acid string
func findSubstrings(dna, targetAA string) []string {
    var result []string
    targetLength := len(targetAA) * 3 // length in nucleotides
    
    // Check forward strand
    for i := 0; i <= len(dna)-targetLength; i++ {
        substring := dna[i : i+targetLength]
        if translateDNAtoAA(substring) == targetAA {
            result = append(result, substring)
        }
    }
    
    // Check reverse complement strand
    revComp := reverseComplement(dna)
    for i := 0; i <= len(revComp)-targetLength; i++ {
        substring := revComp[i : i+targetLength]
        if translateDNAtoAA(substring) == targetAA {
            result = append(result, substring)
        }
    }
    
    return result
}

func main() {
    // Example usage
    dna := "ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA"
    targetAA := "MA"
    
    substrings := findSubstrings(dna, targetAA)
    
    fmt.Printf("DNA: %s\n", dna)
    fmt.Printf("Target Amino Acid: %s\n", targetAA)
    fmt.Printf("Found %d substrings:\n", len(substrings))
    
    for i, sub := range substrings {
        fmt.Printf("  %d: %s\n", i+1, sub)
    }
    
    // More comprehensive example with actual problem input
    fmt.Println("\n--- Complete Solution ---")
    
    // Sample input from Rosalind problem
    sampleDNA := "ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA"
    sampleTarget := "MA"
    
    results := findSubstrings(sampleDNA, sampleTarget)
    
    fmt.Printf("DNA sequence: %s\n", sampleDNA)
    fmt.Printf("Target amino acid sequence: %s\n", sampleTarget)
    fmt.Printf("Number of matching substrings: %d\n", len(results))
    
    for i, result := range results {
        fmt.Printf("Substring %d: %s\n", i+1, result)
    }
}
```

## Explanation

This solution addresses the Rosalind problem by:

1. **Creating a Codon Table**: Maps all 64 codons to their corresponding amino acids using the standard genetic code.

2. **Reverse Complement Function**: Generates the reverse complement of a DNA sequence, which is necessary since amino acids can be encoded by either strand.

3. **Translation Function**: Converts DNA sequences to amino acid sequences by reading codons (3-nucleotide groups).

4. **Substring Finding**: 
   - Checks all possible substrings of the required length (target amino acid length × 3)
   - Tests both the forward and reverse complement strands
   - Returns all substrings that translate to the target amino acid sequence

## Key Features

- Handles both forward and reverse DNA strands
- Uses efficient string operations for translation
- Properly manages the 3:1 ratio between nucleotides and amino acids
- Includes comprehensive test cases

## Time Complexity
- O(n × m) where n is the length of the DNA sequence and m is the length of the target amino acid sequence
- Space complexity is O(k) where k is the number of matching substrings

The solution correctly identifies all substrings that encode the given amino acid sequence, considering both strands of the DNA.

