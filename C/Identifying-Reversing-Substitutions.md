# Rosalind Problem: Identifying Reversing Substitutions

## Problem Description
Given two DNA strings of equal length, find the positions where the strings differ and identify if the differences are reversals (A↔T, C↔G) or substitutions (any other difference).

## Solution in C

```c
#include <stdio.h>
#include <string.h>

int main() {
    char seq1[1000], seq2[1000];
    
    // Read the two DNA sequences
    printf("Enter first DNA sequence: ");
    scanf("%s", seq1);
    printf("Enter second DNA sequence: ");
    scanf("%s", seq2);
    
    int length = strlen(seq1);
    int reversals = 0;
    int substitutions = 0;
    
    printf("Position\tBase1\tBase2\tType\n");
    printf("--------\t-----\t-----\t----\n");
    
    // Compare each position
    for (int i = 0; i < length; i++) {
        if (seq1[i] != seq2[i]) {
            // Check if it's a reversal (A↔T or C↔G)
            if ((seq1[i] == 'A' && seq2[i] == 'T') || 
                (seq1[i] == 'T' && seq2[i] == 'A') ||
                (seq1[i] == 'C' && seq2[i] == 'G') || 
                (seq1[i] == 'G' && seq2[i] == 'C')) {
                printf("%d\t\t%c\t%c\tReversal\n", i+1, seq1[i], seq2[i]);
                reversals++;
            } else {
                printf("%d\t\t%c\t%c\tSubstitution\n", i+1, seq1[i], seq2[i]);
                substitutions++;
            }
        }
    }
    
    printf("\nSummary:\n");
    printf("Reversals: %d\n", reversals);
    printf("Substitutions: %d\n", substitutions);
    
    return 0;
}
```

## Alternative Implementation with File Input

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *file;
    char seq1[1000], seq2[1000];
    
    // Open input file
    file = fopen("rosalind_revc.txt", "r");
    if (file == NULL) {
        printf("Error opening file!\n");
        return 1;
    }
    
    // Read sequences from file
    fscanf(file, "%s", seq1);
    fscanf(file, "%s", seq2);
    fclose(file);
    
    int length = strlen(seq1);
    
    printf("Position\tBase1\tBase2\tType\n");
    printf("--------\t-----\t-----\t----\n");
    
    // Compare each position
    for (int i = 0; i < length; i++) {
        if (seq1[i] != seq2[i]) {
            // Check if it's a reversal
            if ((seq1[i] == 'A' && seq2[i] == 'T') || 
                (seq1[i] == 'T' && seq2[i] == 'A') ||
                (seq1[i] == 'C' && seq2[i] == 'G') || 
                (seq1[i] == 'G' && seq2[i] == 'C')) {
                printf("%d\t\t%c\t%c\tReversal\n", i+1, seq1[i], seq2[i]);
            } else {
                printf("%d\t\t%c\t%c\tSubstitution\n", i+1, seq1[i], seq2[i]);
            }
        }
    }
    
    return 0;
}
```

## Sample Input/Output

**Input:**
```
ATCGATCG
TAGCTAGC
```

**Output:**
```
Position	Base1	Base2	Type
--------	-----	-----	----
1		A	T	Reversal
2		T	A	Reversal
3		C	G	Reversal
4		G	C	Reversal
5		A	T	Reversal
6		T	A	Reversal
7		C	G	Reversal
8		G	C	Reversal

Summary:
Reversals: 8
Substitutions: 0
```

## Key Features of the Solution

1. **Input Handling**: Reads two DNA sequences from either console or file
2. **Position Tracking**: Shows the position (1-indexed) of each difference
3. **Type Classification**: 
   - Reversals: A↔T and C↔G pairs
   - Substitutions: All other differences
4. **Clear Output Format**: Tabular display with position, base values, and type
5. **Summary Statistics**: Counts of reversals and substitutions

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the DNA sequences
- **Space Complexity**: O(n) for storing the sequences

The solution correctly identifies the difference types as required for the Rosalind problem.

