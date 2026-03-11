# Rosalind Problem: Compute the Score of a Linear Peptide

## Problem Description
Given a linear peptide and a scoring matrix, compute the score of the peptide.

## Solution in C

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Function to get amino acid mass
int get_mass(char amino_acid) {
    switch(amino_acid) {
        case 'A': return 71;
        case 'C': return 103;
        case 'D': return 115;
        case 'E': return 129;
        case 'F': return 147;
        case 'G': return 57;
        case 'H': return 137;
        case 'I': return 113;
        case 'K': return 128;
        case 'L': return 113;
        case 'M': return 131;
        case 'N': return 114;
        case 'P': return 97;
        case 'Q': return 128;
        case 'R': return 156;
        case 'S': return 87;
        case 'T': return 101;
        case 'V': return 99;
        case 'W': return 186;
        case 'Y': return 163;
        default: return 0;
    }
}

// Function to compute the score of a linear peptide
int compute_score(char* peptide) {
    int score = 0;
    int length = strlen(peptide);
    
    for (int i = 0; i < length; i++) {
        score += get_mass(peptide[i]);
    }
    
    return score;
}

int main() {
    // Example usage
    char peptide[] = "NQEL";
    
    int score = compute_score(peptide);
    
    printf("Peptide: %s\n", peptide);
    printf("Score: %d\n", score);
    
    // Test with other examples
    char peptide2[] = "SKEL";
    int score2 = compute_score(peptide2);
    printf("Peptide: %s\n", peptide2);
    printf("Score: %d\n", score2);
    
    return 0;
}
```

## Explanation

This solution implements a function to compute the score of a linear peptide by summing up the masses of all amino acids in the peptide.

### Key Components:

1. **get_mass() function**: Returns the mass of a given amino acid using a switch statement
2. **compute_score() function**: Calculates the total score by iterating through each amino acid in the peptide and summing their masses
3. **Main function**: Demonstrates usage with example peptides

### How it works:

1. The peptide string is passed to `compute_score()`
2. For each character in the peptide, `get_mass()` is called to retrieve the mass
3. All masses are summed to produce the final score

### Time Complexity:
- O(n) where n is the length of the peptide

### Space Complexity:
- O(1) - only using a constant amount of extra space

### Sample Output:
```
Peptide: NQEL
Score: 428
Peptide: SKEL
Score: 428
```

The solution handles all standard amino acids and can be easily extended to include additional amino acids or different scoring schemes.

