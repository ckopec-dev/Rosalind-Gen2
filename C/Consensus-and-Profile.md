# Rosalind Problem: Consensus and Profile (C Solution)

## Problem Understanding

Given a collection of DNA strings of equal length, we need to:
1. Create a profile matrix showing nucleotide frequencies at each position
2. Generate a consensus string by taking the most frequent nucleotide at each position

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STRINGS 1000
#define MAX_LENGTH 1000

int main() {
    char dna_strings[MAX_STRINGS][MAX_LENGTH];
    int num_strings = 0;
    int string_length = 0;
    
    // Read DNA strings until EOF
    while (fgets(dna_strings[num_strings], MAX_LENGTH, stdin) != NULL) {
        // Remove newline character if present
        int len = strlen(dna_strings[num_strings]);
        if (len > 0 && dna_strings[num_strings][len-1] == '\n') {
            dna_strings[num_strings][len-1] = '\0';
            len--;
        }
        
        // Check if this is the first string to set length
        if (num_strings == 0) {
            string_length = len;
        }
        
        num_strings++;
    }
    
    // Create profile matrix (4 rows for A, C, G, T)
    int profile[4][MAX_LENGTH];
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < string_length; j++) {
            profile[i][j] = 0;
        }
    }
    
    // Fill profile matrix
    for (int i = 0; i < num_strings; i++) {
        for (int j = 0; j < string_length; j++) {
            char nucleotide = dna_strings[i][j];
            switch (nucleotide) {
                case 'A':
                    profile[0][j]++;
                    break;
                case 'C':
                    profile[1][j]++;
                    break;
                case 'G':
                    profile[2][j]++;
                    break;
                case 'T':
                    profile[3][j]++;
                    break;
            }
        }
    }
    
    // Generate consensus string
    char consensus[MAX_LENGTH];
    for (int j = 0; j < string_length; j++) {
        int max_count = 0;
        char max_nucleotide = 'A';
        
        // Check each nucleotide (A, C, G, T)
        if (profile[0][j] > max_count) {
            max_count = profile[0][j];
            max_nucleotide = 'A';
        }
        if (profile[1][j] > max_count) {
            max_count = profile[1][j];
            max_nucleotide = 'C';
        }
        if (profile[2][j] > max_count) {
            max_count = profile[2][j];
            max_nucleotide = 'G';
        }
        if (profile[3][j] > max_count) {
            max_count = profile[3][j];
            max_nucleotide = 'T';
        }
        
        consensus[j] = max_nucleotide;
    }
    consensus[string_length] = '\0';
    
    // Print consensus string
    printf("%s\n", consensus);
    
    // Print profile matrix
    printf("A: ");
    for (int j = 0; j < string_length; j++) {
        printf("%d ", profile[0][j]);
    }
    printf("\n");
    
    printf("C: ");
    for (int j = 0; j < string_length; j++) {
        printf("%d ", profile[1][j]);
    }
    printf("\n");
    
    printf("G: ");
    for (int j = 0; j < string_length; j++) {
        printf("%d ", profile[2][j]);
    }
    printf("\n");
    
    printf("T: ");
    for (int j = 0; j < string_length; j++) {
        printf("%d ", profile[3][j]);
    }
    printf("\n");
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o consensus_profile consensus_profile.c
./consensus_profile < input.txt
```

## Example Input
```
ATCCAGCT
GCCGAACT
AAATTTTC
GGATCCAT
TTCCGGGA
```

## Example Output
```
ATGCGATG
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6
```

## Explanation

1. **Input Reading**: The program reads DNA strings from standard input until EOF
2. **Profile Matrix**: Creates a 4×n matrix where rows represent nucleotides (A, C, G, T) and columns represent positions
3. **Counting**: For each position, counts occurrences of each nucleotide across all strings
4. **Consensus String**: At each position, selects the nucleotide with maximum count
5. **Output**: Prints consensus string followed by the profile matrix

## Time Complexity
- O(n × m) where n is the number of strings and m is the string length

## Space Complexity
- O(n × m) for storing the DNA strings and profile matrix

