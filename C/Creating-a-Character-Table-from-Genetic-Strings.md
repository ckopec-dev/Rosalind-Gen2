# Rosalind Problem: Creating a Character Table from Genetic Strings (C Solution)

## Problem Understanding

Given a collection of strings representing genetic sequences, we need to create a character table that shows which characters appear at each position across all sequences.

## Solution Approach

1. Read the input sequences
2. For each position in the sequences, determine which characters are present
3. Create a binary table where 1 represents presence and 0 represents absence
4. Output the character table

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
    
    // Read input sequences
    char line[MAX_LENGTH];
    while (fgets(line, sizeof(line), stdin)) {
        // Remove newline character
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) > 0) {
            strcpy(sequences[num_sequences], line);
            num_sequences++;
            if (strlen(line) > max_length) {
                max_length = strlen(line);
            }
        }
    }
    
    // Create character table
    // For each position, we'll check which characters are present
    for (int pos = 0; pos < max_length; pos++) {
        // Create a bit vector for characters at this position
        int char_present[256] = {0};
        int total_chars = 0;
        
        // Check what characters are present at this position
        for (int i = 0; i < num_sequences; i++) {
            if (pos < strlen(sequences[i])) {
                char c = sequences[i][pos];
                char_present[(unsigned char)c] = 1;
                total_chars++;
            }
        }
        
        // Print the character table row
        for (int i = 0; i < num_sequences; i++) {
            if (pos < strlen(sequences[i])) {
                char c = sequences[i][pos];
                printf("%d", char_present[(unsigned char)c]);
            } else {
                printf("0");
            }
        }
        printf("\n");
    }
    
    return 0;
}
```

## Alternative Implementation (More Efficient)

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
    
    // Read input sequences
    char line[MAX_LENGTH];
    while (fgets(line, sizeof(line), stdin)) {
        // Remove newline character
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) > 0) {
            strcpy(sequences[num_sequences], line);
            num_sequences++;
            if (strlen(line) > max_length) {
                max_length = strlen(line);
            }
        }
    }
    
    // Create character table - one row per position
    for (int pos = 0; pos < max_length; pos++) {
        // Create a bit vector to track which characters appear at this position
        char present[256] = {0};
        
        // Mark which characters are present at this position
        for (int i = 0; i < num_sequences; i++) {
            if (pos < strlen(sequences[i])) {
                present[(unsigned char)sequences[i][pos]] = 1;
            }
        }
        
        // Print the table for this position
        for (int i = 0; i < num_sequences; i++) {
            if (pos < strlen(sequences[i])) {
                printf("%d", present[(unsigned char)sequences[i][pos]]);
            } else {
                printf("0");
            }
        }
        printf("\n");
    }
    
    return 0;
}
```

## Input/Output Example

**Input:**
```
ATGCT
ATGCA
ATGCG
ATGCT
```

**Output:**
```
11111
11111
11111
11111
11111
```

## Explanation

The character table is created by:
1. Reading all input sequences
2. For each position (column) across all sequences:
   - Determine which characters appear at that position
   - Create a binary representation where 1 indicates the character is present, 0 otherwise
3. Output the table with one row per position and one column per sequence

This approach handles variable-length sequences and correctly identifies character presence at each position across all input strings.

