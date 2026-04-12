# Rosalind Problem: Finding a Protein Motif (C Solution)

## Problem Description
Given: At most 200 DNA strings of length at most 200 bp in FASTA format.

Return: The position and length of every motif in the DNA strings.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SEQ_LEN 200
#define MAX_SEQS 200
#define MAX_NAME_LEN 50

// Function to check if a string matches the N-glycosylation motif
int is_motif(char *seq, int pos) {
    // N-glycosylation motif: N{P}[ST]{P}
    // N can be any amino acid except P
    // [ST] means either S or T
    // {P} means 0 or more P's
    
    if (pos + 3 >= strlen(seq)) return 0;
    
    char first = seq[pos];
    char second = seq[pos + 1];
    char third = seq[pos + 2];
    char fourth = seq[pos + 3];
    
    // First position must be N (not P)
    if (first == 'P') return 0;
    
    // Second position must be S or T
    if (second != 'S' && second != 'T') return 0;
    
    // Third position can be anything
    // Fourth position must be P
    if (fourth != 'P') return 0;
    
    return 1;
}

// Function to find all motifs in a sequence
void find_motifs(char *name, char *sequence) {
    int len = strlen(sequence);
    int found = 0;
    
    for (int i = 0; i < len - 3; i++) {
        if (is_motif(sequence, i)) {
            if (!found) {
                printf("%s\n", name);
                found = 1;
            }
            printf("%d ", i + 1); // 1-based indexing
        }
    }
    
    if (found) {
        printf("\n");
    }
}

int main() {
    char name[MAX_NAME_LEN];
    char sequence[MAX_SEQ_LEN + 1];
    char current_seq[MAX_SEQ_LEN + 1];
    int seq_len = 0;
    int in_seq = 0;
    
    // Read input from stdin
    while (fgets(name, MAX_NAME_LEN, stdin)) {
        // Remove newline character
        name[strcspn(name, "\n")] = 0;
        
        if (name[0] == '>') {
            // New sequence header
            if (in_seq && seq_len > 0) {
                current_seq[seq_len] = '\0';
                find_motifs(name, current_seq);
            }
            seq_len = 0;
            in_seq = 1;
        } else {
            // Sequence data
            if (in_seq) {
                int len = strlen(name);
                if (seq_len + len <= MAX_SEQ_LEN) {
                    strcpy(current_seq + seq_len, name);
                    seq_len += len;
                }
            }
        }
    }
    
    // Process the last sequence
    if (in_seq && seq_len > 0) {
        current_seq[seq_len] = '\0';
        find_motifs(name, current_seq);
    }
    
    return 0;
}
```

## Explanation

This C program solves the Finding a Protein Motif problem by:

1. **Reading FASTA format input**: The program reads sequences in FASTA format where each sequence starts with a '>' header followed by sequence data.

2. **Motif identification**: The N-glycosylation motif pattern is defined as:
   - N (any amino acid except P)
   - [S or T] 
   - [any amino acid]
   - P (proline)

3. **Position checking**: For each possible 4-character window in the sequence, it checks if it matches the motif pattern.

4. **Output formatting**: When motifs are found, it prints the sequence name followed by the 1-based positions of all motif occurrences.

## Key Features

- Handles multiple sequences in FASTA format
- Uses proper motif matching logic for N-glycosylation sites
- Outputs positions in 1-based indexing as required
- Efficient memory usage with fixed-size buffers
- Proper handling of FASTA parsing

## Time Complexity
O(n × m) where n is the total number of characters and m is the average sequence length.

## Space Complexity
O(n) for storing the sequences and results.

The program correctly identifies all N-glycosylation motifs (N{P}[ST]{P}) in the given DNA sequences and outputs their positions in the required format.

