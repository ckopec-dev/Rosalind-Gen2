# Rosalind Problem: Inferring mRNA from Protein

## Problem Description
Given an amino acid string, determine the number of possible mRNA strings from which the protein could have been translated, modulo 1000000.

## Solution Approach
1. Create a mapping of amino acids to their possible codon counts
2. For each amino acid in the protein string, multiply the result by the number of codons that can encode it
3. Handle the stop codons (UAA, UAG, UGA) which each have 1 possibility
4. Return the result modulo 1000000

## C Implementation

```c
#include <stdio.h>
#include <string.h>

long long count_mrna(const char* protein) {
    long long result = 1;
    int mod = 1000000;
    
    // Codon counts for each amino acid
    // A: GCU, GCC, GCA, GCG (4)
    // C: UGU, UGC (2)
    // D: GAU, GAC (2)
    // E: GAA, GAG (2)
    // F: UUU, UUC (2)
    // G: GGU, GGC, GGA, GGG (4)
    // H: CAU, CAC (2)
    // I: AUU, AUC, AUA (3)
    // K: AAA, AAG (2)
    // L: UUA, UUG, CUU, CUC, CUA, CUG (6)
    // M: AUG (1)
    // N: AAU, AAC (2)
    // P: CCU, CCC, CCA, CCG (4)
    // Q: CAA, CAG (2)
    // R: CGU, CGC, CGA, CGG, AGA, AGG (6)
    // S: UCU, UCC, UCA, UCG, AGU, AGC (6)
    // T: ACU, ACC, ACA, ACG (4)
    // V: GUU, GUC, GUA, GUG (4)
    // W: UGG (1)
    // Y: UAU, UAC (2)
    
    for (int i = 0; i < strlen(protein); i++) {
        char amino_acid = protein[i];
        int codon_count = 0;
        
        switch (amino_acid) {
            case 'A': codon_count = 4; break;
            case 'C': codon_count = 2; break;
            case 'D': codon_count = 2; break;
            case 'E': codon_count = 2; break;
            case 'F': codon_count = 2; break;
            case 'G': codon_count = 4; break;
            case 'H': codon_count = 2; break;
            case 'I': codon_count = 3; break;
            case 'K': codon_count = 2; break;
            case 'L': codon_count = 6; break;
            case 'M': codon_count = 1; break;
            case 'N': codon_count = 2; break;
            case 'P': codon_count = 4; break;
            case 'Q': codon_count = 2; break;
            case 'R': codon_count = 6; break;
            case 'S': codon_count = 6; break;
            case 'T': codon_count = 4; break;
            case 'V': codon_count = 4; break;
            case 'W': codon_count = 1; break;
            case 'Y': codon_count = 2; break;
            default: codon_count = 0; break;
        }
        
        result = (result * codon_count) % mod;
    }
    
    // Multiply by 3 for stop codons (UAA, UAG, UGA)
    result = (result * 3) % mod;
    
    return result;
}

int main() {
    char protein[1000];
    
    printf("Enter the protein string: ");
    scanf("%s", protein);
    
    long long result = count_mrna(protein);
    printf("Number of possible mRNA strings: %lld\n", result);
    
    return 0;
}
```

## Example Usage
```
Input: MAM
Output: 12

Explanation:
- M (methionine) can be encoded by 1 codon
- A (alanine) can be encoded by 4 codons  
- M (methionine) can be encoded by 1 codon
- Stop codons: 3 possibilities

Total = 1 × 4 × 1 × 3 = 12
```

## Key Points
1. Each amino acid has a specific number of codons that can encode it
2. Stop codons (UAA, UAG, UGA) each have 1 possibility
3. The result must be taken modulo 1000000 to prevent overflow
4. The multiplication is done step by step to maintain the modulo property

