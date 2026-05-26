# Rosalind Problem: Finding All Similar Motifs (C Solution)

## Problem Understanding

The problem asks us to find all motifs (short DNA sequences) that appear in all given DNA sequences. This is essentially finding the intersection of all possible motifs across multiple sequences.

## Approach

1. Read multiple DNA sequences
2. Generate all possible motifs of a given length from each sequence
3. Find motifs that appear in ALL sequences
4. Output the common motifs

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SEQ_LEN 1000
#define MAX_SEQS 100
#define MAX_MOTIF_LEN 100

// Function to check if a motif exists in a sequence
int motif_exists(char *sequence, char *motif, int seq_len, int motif_len) {
    for (int i = 0; i <= seq_len - motif_len; i++) {
        if (strncmp(&sequence[i], motif, motif_len) == 0) {
            return 1;
        }
    }
    return 0;
}

// Function to generate all motifs of given length from a sequence
void generate_motifs(char *sequence, int seq_len, int motif_len, char motifs[][MAX_MOTIF_LEN], int *count) {
    *count = 0;
    for (int i = 0; i <= seq_len - motif_len; i++) {
        strncpy(motifs[*count], &sequence[i], motif_len);
        motifs[*count][motif_len] = '\0';
        (*count)++;
    }
}

// Function to check if a motif exists in all sequences
int is_common_motif(char *motif, char sequences[][MAX_SEQ_LEN], int seq_count, int seq_lens[]) {
    for (int i = 0; i < seq_count; i++) {
        if (!motif_exists(sequences[i], motif, seq_lens[i], strlen(motif))) {
            return 0;
        }
    }
    return 1;
}

int main() {
    char sequences[MAX_SEQS][MAX_SEQ_LEN];
    int seq_lens[MAX_SEQS];
    int seq_count = 0;
    
    // Read sequences
    char line[MAX_SEQ_LEN];
    while (fgets(line, sizeof(line), stdin)) {
        // Remove newline character
        line[strcspn(line, "\n")] = 0;
        
        // Skip empty lines
        if (strlen(line) == 0) continue;
        
        // Check if this is a sequence (not a header)
        if (line[0] != '>') {
            strcpy(sequences[seq_count], line);
            seq_lens[seq_count] = strlen(line);
            seq_count++;
        }
    }
    
    // Find the longest common motif
    int max_motif_len = 0;
    for (int i = 0; i < seq_count; i++) {
        if (seq_lens[i] > max_motif_len) {
            max_motif_len = seq_lens[i];
        }
    }
    
    // Try all possible motif lengths from max_motif_len down to 1
    char common_motifs[MAX_SEQS][MAX_MOTIF_LEN];
    int common_count = 0;
    
    for (int motif_len = max_motif_len; motif_len >= 1; motif_len--) {
        // Generate all motifs of current length from first sequence
        char motifs[MAX_SEQS][MAX_MOTIF_LEN];
        int motif_count;
        
        generate_motifs(sequences[0], seq_lens[0], motif_len, motifs, &motif_count);
        
        // Check which motifs are common to all sequences
        for (int i = 0; i < motif_count; i++) {
            if (is_common_motif(motifs[i], sequences, seq_count, seq_lens)) {
                // Check if this motif is already in our results
                int already_found = 0;
                for (int j = 0; j < common_count; j++) {
                    if (strcmp(common_motifs[j], motifs[i]) == 0) {
                        already_found = 1;
                        break;
                    }
                }
                
                if (!already_found) {
                    strcpy(common_motifs[common_count], motifs[i]);
                    common_count++;
                }
            }
        }
        
        // If we found common motifs, we're done (since we want the longest)
        if (common_count > 0) {
            break;
        }
    }
    
    // Print results
    for (int i = 0; i < common_count; i++) {
        printf("%s\n", common_motifs[i]);
    }
    
    return 0;
}
```

## Alternative Implementation (More Efficient)

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SEQ_LEN 1000
#define MAX_SEQS 100

// Function to check if motif exists in all sequences
int is_in_all_sequences(char *motif, char sequences[][MAX_SEQ_LEN], int seq_count, int seq_lens[]) {
    int motif_len = strlen(motif);
    for (int i = 0; i < seq_count; i++) {
        int found = 0;
        for (int j = 0; j <= seq_lens[i] - motif_len; j++) {
            if (strncmp(&sequences[i][j], motif, motif_len) == 0) {
                found = 1;
                break;
            }
        }
        if (!found) return 0;
    }
    return 1;
}

int main() {
    char sequences[MAX_SEQS][MAX_SEQ_LEN];
    int seq_lens[MAX_SEQS];
    int seq_count = 0;
    
    // Read sequences
    char line[MAX_SEQ_LEN];
    while (fgets(line, sizeof(line), stdin)) {
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) == 0) continue;
        if (line[0] != '>') {
            strcpy(sequences[seq_count], line);
            seq_lens[seq_count] = strlen(line);
            seq_count++;
        }
    }
    
    // Find longest common motif
    int min_len = seq_lens[0];
    for (int i = 1; i < seq_count; i++) {
        if (seq_lens[i] < min_len) min_len = seq_lens[i];
    }
    
    // Try decreasing motif lengths
    char result[MAX_SEQ_LEN] = "";
    
    for (int len = min_len; len >= 1; len--) {
        // Check all motifs of current length in first sequence
        for (int i = 0; i <= seq_lens[0] - len; i++) {
            char motif[MAX_SEQ_LEN];
            strncpy(motif, &sequences[0][i], len);
            motif[len] = '\0';
            
            if (is_in_all_sequences(motif, sequences, seq_count, seq_lens)) {
                printf("%s\n", motif);
                strcpy(result, motif);
                // Continue to find all longest motifs
            }
        }
        
        // If we found at least one motif of this length, we're done
        if (strlen(result) > 0) {
            break;
        }
    }
    
    return 0;
}
```

## Key Points

1. **Input Handling**: The code reads DNA sequences, handling both FASTA format and plain sequences
2. **Motif Generation**: Generates all possible motifs of a given length from a sequence
3. **Commonality Check**: Verifies that a motif appears in all sequences
4. **Optimization**: Starts with the longest possible motifs and works downward to find the longest common motifs first

## Time Complexity
- O(n × m × k × l) where n is the number of sequences, m is the average sequence length, k is the motif length, and l is the number of motifs to check

## Space Complexity
- O(n × m) for storing the sequences and motifs

This solution efficiently finds all similar motifs that appear in every given DNA sequence.

