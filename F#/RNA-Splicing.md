# Rosalind Problem RNA_Splicing Solution in F#

## Problem Understanding

RNA splicing involves removing introns (non-coding sequences) from pre-mRNA to form mature mRNA. Given a DNA string and a collection of intron sequences, we need to:
1. Transcribe DNA to RNA
2. Remove all introns from the RNA sequence
3. Translate the resulting mRNA to protein

## Solution

```fsharp
open System
open System.Text

// DNA to RNA transcription mapping
let dnaToRna (dna: string) : string =
    dna.Replace('T', 'U')

// RNA codon to amino acid mapping
let codonToAminoAcid : Map<string, string> = 
    Map [
        ("UUU", "F"); ("UUC", "F"); ("UUA", "L"); ("UUG", "L");
        ("CUU", "L"); ("CUC", "L"); ("CUA", "L"); ("CUG", "L");
        ("AUU", "I"); ("AUC", "I"); ("AUA", "I");
        ("GUU", "V"); ("GUC", "V"); ("GUA", "V"); ("GUG", "V");
        ("UCU", "S"); ("UCC", "S"); ("UCA", "S"); ("UCG", "S");
        ("CCU", "P"); ("CCC", "P"); ("CCA", "P"); ("CCG", "P");
        ("ACU", "T"); ("ACC", "T"); ("ACA", "T"); ("ACG", "T");
        ("GCU", "A"); ("GCC", "A"); ("GCA", "A"); ("GCG", "A");
        ("UAU", "Y"); ("UAC", "Y");
        ("UGU", "C"); ("UGC", "C");
        ("UGG", "W");
        ("UAA", ""); ("UAG", ""); ("UGA", "") // Stop codons
    ]

// Translate RNA to protein
let translateRna (rna: string) : string =
    let mutable protein = ""
    let mutable i = 0
    
    while i <= rna.Length - 3 do
        if i + 3 <= rna.Length then
            let codon = rna.Substring(i, 3)
            match Map.tryFind codon codonToAminoAcid with
            | Some aa when aa = "" -> 
                // Stop codon found, stop translation
                i <- rna.Length  // Break loop
            | Some aa -> 
                protein <- protein + aa
                i <- i + 3
            | None -> 
                // Invalid codon, should not happen in valid input
                i <- i + 3
        else
            i <- i + 1
    
    protein

// Remove introns from sequence
let removeIntrons (sequence: string) (introns: string list) : string =
    let mutable result = sequence
    for intron in introns do
        result <- result.Replace(intron, "")
    result

// Main solution function
let rnaSplicing (dna: string) (introns: string list) : string =
    // Transcribe DNA to RNA
    let rna = dnaToRna dna
    
    // Remove introns
    let rnaWithoutIntrons = removeIntrons rna introns
    
    // Translate to protein
    translateRna rnaWithoutIntrons

// Alternative cleaner implementation using List.fold
let rnaSplicingClean (dna: string) (introns: string list) : string =
    let rna = dnaToRna dna
    let rnaWithoutIntrons = 
        introns |> List.fold (fun acc intron -> acc.Replace(intron, "")) rna
    translateRna rnaWithoutIntrons

// Example usage with sample data
let sampleDna = "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"
let sampleIntrons = ["ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"]

// Test the solution
let result = rnaSplicingClean sampleDna sampleIntrons
printfn "Result: %s" result
```

## Explanation

1. **DNA to RNA Transcription**: Replace all 'T' nucleotides with 'U' (RNA uses uracil instead of thymine)

2. **Intron Removal**: Use string replacement to remove each intron sequence from the transcribed RNA

3. **Protein Translation**: 
   - Process RNA sequence in groups of 3 nucleotides (codons)
   - Map each codon to its corresponding amino acid using the genetic code
   - Stop translation when encountering a stop codon (UAA, UAG, UGA)

4. **Edge Cases Handled**:
   - Stop codons terminate translation
   - Proper handling of codon boundaries
   - Empty intron removal

## Time and Space Complexity

- **Time Complexity**: O(n + m) where n is the length of the DNA sequence and m is the total length of all introns
- **Space Complexity**: O(n) for storing the RNA sequence and protein output

## Usage Example

```fsharp
// Given DNA sequence and introns
let dna = "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"
let introns = ["ATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG"]

// Get the protein sequence
let protein = rnaSplicingClean dna introns
printfn "Protein sequence: %s" protein
```

This solution correctly handles the RNA splicing problem by first transcribing DNA to RNA, removing introns, and then translating the resulting mRNA to protein using the standard genetic code.

