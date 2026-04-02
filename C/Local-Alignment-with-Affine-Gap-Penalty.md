# Rosalind Problem: Local Alignment with Affine Gap Penalty

I'll solve the Local Alignment with Affine Gap Penalty problem in C.

## Problem Understanding

This problem requires implementing the Smith-Waterman algorithm with affine gap penalties for local sequence alignment. Unlike global alignment with linear gap penalties, this uses affine gap penalties where opening a gap costs `gap_open` and extending an existing gap costs `gap_extend`.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_LEN 1000

// Structure to store alignment information
typedef struct {
    int score;
    int i;
    int j;
} ScoreCell;

// Function to get maximum of three integers
int max3(int a, int b, int c) {
    return fmax(a, fmax(b, c));
}

// Function to get maximum of two integers
int max2(int a, int b) {
    return fmax(a, b);
}

// Function to compute local alignment with affine gap penalty
void local_alignment_affine(char *seq1, char *seq2, int match, int mismatch, 
                           int gap_open, int gap_extend, 
                           int *max_score, int *max_i, int *max_j) {
    
    int m = strlen(seq1);
    int n = strlen(seq2);
    
    // Create 3D matrices for affine gap penalty
    // M[i][j] = match/mismatch score
    // Ix[i][j] = score when we are in gap extension in sequence 1
    // Iy[i][j] = score when we are in gap extension in sequence 2
    
    int **M = (int**)malloc((m + 1) * sizeof(int*));
    int **Ix = (int**)malloc((m + 1) * sizeof(int*));
    int **Iy = (int**)malloc((m + 1) * sizeof(int*));
    
    for (int i = 0; i <= m; i++) {
        M[i] = (int*)calloc(n + 1, sizeof(int));
        Ix[i] = (int*)calloc(n + 1, sizeof(int));
        Iy[i] = (int*)calloc(n + 1, sizeof(int));
    }
    
    // Initialize base cases
    for (int i = 0; i <= m; i++) {
        Ix[i][0] = -gap_open - (i * gap_extend);
        M[i][0] = -1000000;  // Large negative value
        Iy[i][0] = -1000000;
    }
    
    for (int j = 0; j <= n; j++) {
        Iy[0][j] = -gap_open - (j * gap_extend);
        M[0][j] = -1000000;
        Ix[0][j] = -1000000;
    }
    
    M[0][0] = 0;
    Ix[0][0] = -1000000;
    Iy[0][0] = -1000000;
    
    // Fill the matrices
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            // Calculate match/mismatch score
            int score = (seq1[i-1] == seq2[j-1]) ? match : mismatch;
            
            // Calculate M[i][j] - match/mismatch
            M[i][j] = max3(
                M[i-1][j-1] + score,  // match/mismatch
                Ix[i-1][j-1] + score, // match/mismatch from Ix
                Iy[i-1][j-1] + score  // match/mismatch from Iy
            );
            
            // Calculate Ix[i][j] - gap in sequence 1
            Ix[i][j] = max2(
                M[i-1][j] - gap_open - gap_extend,   // open gap
                Ix[i-1][j] - gap_extend              // extend gap
            );
            
            // Calculate Iy[i][j] - gap in sequence 2
            Iy[i][j] = max2(
                M[i][j-1] - gap_open - gap_extend,   // open gap
                Iy[i][j-1] - gap_extend              // extend gap
            );
            
            // Set negative scores to 0 for local alignment
            M[i][j] = max2(M[i][j], 0);
            Ix[i][j] = max2(Ix[i][j], 0);
            Iy[i][j] = max2(Iy[i][j], 0);
        }
    }
    
    // Find maximum score and its position
    *max_score = 0;
    *max_i = 0;
    *max_j = 0;
    
    for (int i = 0; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            int current_max = max3(M[i][j], Ix[i][j], Iy[i][j]);
            if (current_max > *max_score) {
                *max_score = current_max;
                *max_i = i;
                *max_j = j;
            }
        }
    }
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(M[i]);
        free(Ix[i]);
        free(Iy[i]);
    }
    free(M);
    free(Ix);
    free(Iy);
}

// Function to trace back the alignment
void traceback_affine(char *seq1, char *seq2, int match, int mismatch, 
                     int gap_open, int gap_extend, 
                     int max_score, int max_i, int max_j,
                     char *align1, char *align2) {
    
    int m = strlen(seq1);
    int n = strlen(seq2);
    
    // Create matrices for traceback
    int **M = (int**)malloc((m + 1) * sizeof(int*));
    int **Ix = (int**)malloc((m + 1) * sizeof(int*));
    int **Iy = (int**)malloc((m + 1) * sizeof(int*));
    
    for (int i = 0; i <= m; i++) {
        M[i] = (int*)calloc(n + 1, sizeof(int));
        Ix[i] = (int*)calloc(n + 1, sizeof(int));
        Iy[i] = (int*)calloc(n + 1, sizeof(int));
    }
    
    // Initialize base cases
    for (int i = 0; i <= m; i++) {
        Ix[i][0] = -gap_open - (i * gap_extend);
        M[i][0] = -1000000;
        Iy[i][0] = -1000000;
    }
    
    for (int j = 0; j <= n; j++) {
        Iy[0][j] = -gap_open - (j * gap_extend);
        M[0][j] = -1000000;
        Ix[0][j] = -1000000;
    }
    
    M[0][0] = 0;
    Ix[0][0] = -1000000;
    Iy[0][0] = -1000000;
    
    // Fill the matrices
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int score = (seq1[i-1] == seq2[j-1]) ? match : mismatch;
            
            M[i][j] = max3(
                M[i-1][j-1] + score,
                Ix[i-1][j-1] + score,
                Iy[i-1][j-1] + score
            );
            
            Ix[i][j] = max2(
                M[i-1][j] - gap_open - gap_extend,
                Ix[i-1][j] - gap_extend
            );
            
            Iy[i][j] = max2(
                M[i][j-1] - gap_open - gap_extend,
                Iy[i][j-1] - gap_extend
            );
            
            M[i][j] = max2(M[i][j], 0);
            Ix[i][j] = max2(Ix[i][j], 0);
            Iy[i][j] = max2(Iy[i][j], 0);
        }
    }
    
    // Traceback
    int i = max_i;
    int j = max_j;
    int align_len = 0;
    
    // Reset the alignment strings
    align1[0] = '\0';
    align2[0] = '\0';
    
    // Start from the maximum score position
    while (i > 0 || j > 0) {
        int current_score = max3(M[i][j], Ix[i][j], Iy[i][j]);
        
        if (current_score == M[i][j]) {
            // This came from a match/mismatch
            if (i > 0 && j > 0) {
                // Add characters to alignments
                align1[align_len] = seq1[i-1];
                align2[align_len] = seq2[j-1];
                align_len++;
                i--;
                j--;
            } else {
                // End of alignment
                break;
            }
        } else if (current_score == Ix[i][j]) {
            // This came from a gap in sequence 1
            if (i > 0) {
                align1[align_len] = seq1[i-1];
                align2[align_len] = '-';
                align_len++;
                i--;
            } else {
                break;
            }
        } else {
            // This came from a gap in sequence 2
            if (j > 0) {
                align1[align_len] = '-';
                align2[align_len] = seq2[j-1];
                align_len++;
                j--;
            } else {
                break;
            }
        }
    }
    
    // Reverse the alignments
    for (int k = 0; k < align_len/2; k++) {
        char temp1 = align1[k];
        char temp2 = align2[k];
        align1[k] = align1[align_len-1-k];
        align2[k] = align2[align_len-1-k];
        align1[align_len-1-k] = temp1;
        align2[align_len-1-k] = temp2;
    }
    
    align1[align_len] = '\0';
    align2[align_len] = '\0';
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(M[i]);
        free(Ix[i]);
        free(Iy[i]);
    }
    free(M);
    free(Ix);
    free(Iy);
}

int main() {
    // Read input sequences
    char seq1[MAX_LEN];
    char seq2[MAX_LEN];
    
    printf("Enter first sequence: ");
    fgets(seq1, MAX_LEN, stdin);
    seq1[strcspn(seq1, "\n")] = 0;  // Remove newline
    
    printf("Enter second sequence: ");
    fgets(seq2, MAX_LEN, stdin);
    seq2[strcspn(seq2, "\n")] = 0;  // Remove newline
    
    // Read scoring parameters
    int match, mismatch, gap_open, gap_extend;
    printf("Enter match score: ");
    scanf("%d", &match);
    printf("Enter mismatch score: ");
    scanf("%d", &mismatch);
    printf("Enter gap open penalty: ");
    scanf("%d", &gap_open);
    printf("Enter gap extend penalty: ");
    scanf("%d", &gap_extend);
    
    // Compute local alignment
    int max_score, max_i, max_j;
    local_alignment_affine(seq1, seq2, match, mismatch, gap_open, gap_extend, 
                          &max_score, &max_i, &max_j);
    
    // Print results
    printf("Maximum local alignment score: %d\n", max_score);
    printf("Position of maximum score: (%d, %d)\n", max_i, max_j);
    
    // Print alignment
    char align1[MAX_LEN * 2];
    char align2[MAX_LEN * 2];
    traceback_affine(seq1, seq2, match, mismatch, gap_open, gap_extend,
                     max_score, max_i, max_j, align1, align2);
    
    printf("Alignment:\n");
    printf("%s\n", align1);
    printf("%s\n", align2);
    
    return 0;
}
```

## Explanation

This solution implements the Smith-Waterman algorithm with affine gap penalties for local sequence alignment:

1. **Three Matrix Approach**: 
   - `M[i][j]`: Score when we're at position (i,j) with a match/mismatch
   - `Ix[i][j]`: Score when we're at position (i,j) with a gap in sequence 1
   - `Iy[i][j]`: Score when we're at position (i,j) with a gap in sequence 2

2. **Affine Gap Penalties**:
   - Opening a gap costs `gap_open`
   - Extending an existing gap costs `gap_extend`
   - The penalty is different for opening vs. extending gaps

3. **Local Alignment**:
   - Scores are set to 0 if they become negative
   - Maximum score is found across all positions

4. **Traceback**:
   - Reconstructs the optimal alignment by backtracking through the matrices
   - Handles the three different types of moves (match/mismatch, gap in seq1, gap in seq2)

## Input Format
The program expects:
1. Two sequences (strings)
2. Match score
3. Mismatch score  
4. Gap open penalty
5. Gap extend penalty

## Time Complexity
O(m×n) where m and n are the lengths of the sequences.

## Space Complexity
O(m×n) for the three matrices used in the dynamic programming approach.

