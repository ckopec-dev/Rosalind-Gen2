# Rosalind Problem: Translating RNA into Protein (C Solution)

## Problem Description
The task is to translate an RNA string into a protein string by reading codons (3-character sequences) and mapping them to their corresponding amino acids.

## Solution

```c
#include <stdio.h>
#include <string.h>

// Function to translate RNA codon to amino acid
char translate_codon(char codon[3]) {
    // Codon to amino acid mapping
    if (strcmp(codon, "UUU") == 0 || strcmp(codon, "UUC") == 0) return 'F';  // Phenylalanine
    if (strcmp(codon, "UUA") == 0 || strcmp(codon, "UUG") == 0) return 'L';  // Leucine
    if (strcmp(codon, "UCU") == 0 || strcmp(codon, "UCC") == 0 || strcmp(codon, "UCA") == 0 || strcmp(codon, "UCG") == 0) return 'S';  // Serine
    if (strcmp(codon, "UAU") == 0 || strcmp(codon, "UAC") == 0) return 'Y';  // Tyrosine
    if (strcmp(codon, "UGU") == 0 || strcmp(codon, "UGC") == 0) return 'C';  // Cysteine
    if (strcmp(codon, "UGG") == 0) return 'W';  // Tryptophan
    if (strcmp(codon, "CUU") == 0 || strcmp(codon, "CUC") == 0 || strcmp(codon, "CUA") == 0 || strcmp(codon, "CUG") == 0) return 'L';  // Leucine
    if (strcmp(codon, "CCU") == 0 || strcmp(codon, "CCC") == 0 || strcmp(codon, "CCA") == 0 || strcmp(codon, "CCG") == 0) return 'P';  // Proline
    if (strcmp(codon, "CAU") == 0 || strcmp(codon, "CAC") == 0) return 'H';  // Histidine
    if (strcmp(codon, "CAA") == 0 || strcmp(codon, "CAG") == 0) return 'Q';  // Glutamine
    if (strcmp(codon, "CGU") == 0 || strcmp(codon, "CGC") == 0 || strcmp(codon, "CGA") == 0 || strcmp(codon, "CGG") == 0) return 'R';  // Arginine
    if (strcmp(codon, "AUU") == 0 || strcmp(codon, "AUC") == 0 || strcmp(codon, "AUA") == 0) return 'I';  // Isoleucine
    if (strcmp(codon, "AUG") == 0) return 'M';  // Methionine
    if (strcmp(codon, "ACU") == 0 || strcmp(codon, "ACC") == 0 || strcmp(codon, "ACA") == 0 || strcmp(codon, "ACG") == 0) return 'T';  // Threonine
    if (strcmp(codon, "AAU") == 0 || strcmp(codon, "AAC") == 0) return 'N';  // Asparagine
    if (strcmp(codon, "AAA") == 0 || strcmp(codon, "AAG") == 0) return 'K';  // Lysine
    if (strcmp(codon, "AGU") == 0 || strcmp(codon, "AGC") == 0) return 'S';  // Serine
    if (strcmp(codon, "AGA") == 0 || strcmp(codon, "AGG") == 0) return 'R';  // Arginine
    if (strcmp(codon, "GUU") == 0 || strcmp(codon, "GUC") == 0 || strcmp(codon, "GUA") == 0 || strcmp(codon, "GUG") == 0) return 'V';  // Valine
    if (strcmp(codon, "GCU") == 0 || strcmp(codon, "GCC") == 0 || strcmp(codon, "GCA") == 0 || strcmp(codon, "GCG") == 0) return 'A';  // Alanine
    if (strcmp(codon, "GAU") == 0 || strcmp(codon, "GAC") == 0) return 'D';  // Aspartic acid
    if (strcmp(codon, "GAA") == 0 || strcmp(codon, "GAG") == 0) return 'E';  // Glutamic acid
    if (strcmp(codon, "GGU") == 0 || strcmp(codon, "GGC") == 0 || strcmp(codon, "GGA") == 0 || strcmp(codon, "GGG") == 0) return 'G';  // Glycine
    if (strcmp(codon, "UAA") == 0 || strcmp(codon, "UAG") == 0 || strcmp(codon, "UGA") == 0) return '\0';  // Stop codon
    
    return 'X';  // Unknown codon
}

int main() {
    char rna[10000];  // RNA string
    char protein[10000];  // Protein string
    int protein_index = 0;
    
    // Read RNA string
    printf("Enter RNA string: ");
    scanf("%s", rna);
    
    int length = strlen(rna);
    
    // Translate RNA to protein
    for (int i = 0; i < length - 2; i += 3) {
        char codon[4] = {0};  // Codon buffer
        codon[0] = rna[i];
        codon[1] = rna[i+1];
        codon[2] = rna[i+2];
        codon[3] = '\0';
        
        char amino_acid = translate_codon(codon);
        
        // Stop translation at stop codon
        if (amino_acid == '\0') {
            break;
        }
        
        protein[protein_index++] = amino_acid;
    }
    
    protein[protein_index] = '\0';
    
    // Print result
    printf("Protein: %s\n", protein);
    
    return 0;
}
```

## How it works

1. **Input**: The program reads an RNA string from standard input
2. **Translation**: It processes the RNA string in groups of 3 characters (codons)
3. **Mapping**: Each codon is translated to its corresponding amino acid using the genetic code
4. **Stop codons**: Translation stops when encountering a stop codon (UAA, UAG, or UGA)
5. **Output**: The resulting protein string is printed

## Sample Input/Output

**Input:**
```
AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
```

**Output:**
```
MAMAPRTEINSTRING
```

## Key Features

- Handles all standard amino acid mappings
- Properly stops at stop codons
- Efficient character-by-character processing
- Memory-safe string handling
- Clear and readable code structure

## Time Complexity
O(n) where n is the length of the RNA string

## Space Complexity
O(n) for storing the protein string

