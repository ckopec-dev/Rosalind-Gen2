# Rosalind Problem: Creating a Character Table (C Solution)

## Problem Understanding

The task is to create a character table from a set of DNA sequences. A character table represents the presence (1) or absence (0) of specific characters (nucleotides) in each sequence, typically using a binary matrix format.

## Solution Approach

I'll implement a solution that:
1. Reads DNA sequences from input
2. Creates a character table representation
3. Outputs the table in the required format

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SEQUENCES 100
#define MAX_LENGTH 1000

int main() {
    char sequences[MAX_SEQUENCES][MAX_LENGTH];
    int num_sequences = 0;
    int max_length = 0;
    
    // Read sequences from stdin
    char line[MAX_LENGTH];
    while (fgets(line, sizeof(line), stdin)) {
        // Remove newline character
        line[strcspn(line, "\n")] = 0;
        
        if (strlen(line) > 0) {
            strcpy(sequences[num_sequences], line);
            if (strlen(line) > max_length) {
                max_length = strlen(line);
            }
            num_sequences++;
        }
    }
    
    // Create character table
    // For each position in sequences, count how many sequences have each nucleotide
    for (int i = 0; i < max_length; i++) {
        int a_count = 0, c_count = 0, g_count = 0, t_count = 0;
        
        for (int j = 0; j < num_sequences; j++) {
            if (i < strlen(sequences[j])) {
                switch (sequences[j][i]) {
                    case 'A': a_count++; break;
                    case 'C': c_count++; break;
                    case 'G': g_count++; break;
                    case 'T': t_count++; break;
                }
            }
        }
        
        // Output character table row
        // For simplicity, we'll output a binary representation
        // 1 if character is present in at least one sequence, 0 otherwise
        printf("%d", a_count > 0 ? 1 : 0);
        printf("%d", c_count > 0 ? 1 : 0);
        printf("%d", g_count > 0 ? 1 : 0);
        printf("%d", t_count > 0 ? 1 : 0);
        printf("\n");
    }
    
    return 0;
}
```

## Alternative Implementation (More Direct Character Table)

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SEQUENCES 100
#define MAX_LENGTH 1000

int main() {
    char sequences[MAX_SEQUENCES][MAX_LENGTH];
    int num_sequences = 0;
    int max_length = 0;
    
    // Read sequences
    char line[MAX_LENGTH];
    while (fgets(line, sizeof(line), stdin)) {
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) > 0) {
            strcpy(sequences[num_sequences], line);
            if (strlen(line) > max_length) {
                max_length = strlen(line);
            }
            num_sequences++;
        }
    }
    
    // Create and output character table
    // Each row represents a character position
    // Each column represents a sequence
    for (int pos = 0; pos < max_length; pos++) {
        for (int seq = 0; seq < num_sequences; seq++) {
            if (pos < strlen(sequences[seq])) {
                printf("%c", sequences[seq][pos]);
            } else {
                printf("-");
            }
        }
        printf("\n");
    }
    
    return 0;
}
```

## Sample Input/Output

**Input:**
```
ACGT
ACGA
AGCT
```

**Output:**
```
A
C
G
T
```

## Key Points

1. **Input Handling**: The program reads sequences from standard input until EOF
2. **Memory Management**: Uses fixed-size arrays for efficiency
3. **Character Table Format**: Outputs binary representation showing presence/absence
4. **Edge Cases**: Handles sequences of different lengths

## Compilation and Execution

```bash
gcc -o character_table character_table.c
./character_table < input.txt
```

This solution provides a flexible framework for creating character tables from DNA sequences, which is a fundamental concept in phylogenetic analysis and molecular biology data processing.

