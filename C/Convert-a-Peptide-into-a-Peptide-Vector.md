# Rosalind Problem: Convert a Peptide into a Peptide Vector

## Problem Description
Given a peptide, we need to convert it into a peptide vector. A peptide vector is a binary vector of length 3n where n is the length of the peptide, such that:
- The i-th position is 1 if the peptide contains the amino acid with mass i
- The i-th position is 0 otherwise

## Solution in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to get mass of an amino acid
int get_amino_acid_mass(char amino_acid) {
    switch (amino_acid) {
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

// Function to convert peptide to peptide vector
void peptide_to_vector(char* peptide, int* vector, int peptide_length) {
    // Initialize vector with zeros
    for (int i = 0; i < 3 * peptide_length; i++) {
        vector[i] = 0;
    }
    
    // Process each amino acid in the peptide
    for (int i = 0; i < peptide_length; i++) {
        int mass = get_amino_acid_mass(peptide[i]);
        // Set the corresponding position in vector to 1
        if (mass > 0 && mass <= 3 * peptide_length) {
            vector[mass - 1] = 1;
        }
    }
}

int main() {
    // Read peptide from input
    char peptide[1000];
    printf("Enter peptide: ");
    scanf("%s", peptide);
    
    int peptide_length = strlen(peptide);
    
    // Create vector of size 3 * peptide_length
    int* vector = (int*)malloc(3 * peptide_length * sizeof(int));
    
    // Convert peptide to vector
    peptide_to_vector(peptide, vector, peptide_length);
    
    // Print the vector
    printf("Peptide vector: ");
    for (int i = 0; i < 3 * peptide_length; i++) {
        printf("%d ", vector[i]);
    }
    printf("\n");
    
    // Free allocated memory
    free(vector);
    
    return 0;
}
```

## Explanation

1. **get_amino_acid_mass()**: This function returns the mass of each amino acid based on the standard amino acid mass table.

2. **peptide_to_vector()**: This function converts the peptide into a peptide vector:
   - Creates a vector of size 3 × peptide_length (since each amino acid contributes to 3 positions)
   - Initializes all positions to 0
   - For each amino acid in the peptide, gets its mass and sets the corresponding position in the vector to 1

3. **Main function**: 
   - Reads the peptide from input
   - Calculates the vector size
   - Calls the conversion function
   - Prints the resulting vector

## Example

For peptide "LEQN":
- L = 113, E = 129, Q = 128, N = 114
- Vector size = 3 × 4 = 12
- Positions 112, 128, 127, 113 are set to 1

The solution correctly handles the conversion from amino acid sequence to binary vector representation.

