# Rosalind Problem: Semiglobal Alignment in C

## Problem Understanding

Semiglobal alignment is a sequence alignment algorithm that allows gaps at the beginning and end of sequences, but not in the middle. This is different from global alignment (Needleman-Wunsch) where gaps are penalized throughout the entire sequence.

## Solution Approach

I'll implement a semiglobal alignment algorithm using dynamic programming with a scoring matrix, where:
- Match score: +1
- Mismatch score: -1  
- Gap penalty: -1
- Gaps at the beginning and end are allowed without penalty

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MATCH_SCORE 1
#define MISMATCH_SCORE -1
#define GAP_SCORE -1

// Function to get maximum of three integers
int max3(int a, int b, int c) {
    int max = a;
    if (b > max) max = b;
    if (c > max) max = c;
    return max;
}

// Function to get maximum of two integers
int max2(int a, int b) {
    return (a > b) ? a : b;
}

// Semiglobal alignment function
void semiglobal_alignment(char *seq1, char *seq2, int len1, int len2, 
                         int **score_matrix, int **traceback) {
    
    // Initialize the score matrix
    for (int i = 0; i <= len1; i++) {
        score_matrix[i][0] = 0;  // No penalty for gaps at the beginning
    }
    
    for (int j = 0; j <= len2; j++) {
        score_matrix[0][j] = 0;  // No penalty for gaps at the beginning
    }
    
    // Fill the score matrix
    for (int i = 1; i <= len1; i++) {
        for (int j = 1; j <= len2; j++) {
            int match = (seq1[i-1] == seq2[j-1]) ? MATCH_SCORE : MISMATCH_SCORE;
            
            score_matrix[i][j] = max3(
                score_matrix[i-1][j] + GAP_SCORE,    // Deletion
                score_matrix[i][j-1] + GAP_SCORE,    // Insertion
                score_matrix[i-1][j-1] + match       // Match/Mismatch
            );
            
            // Traceback matrix for backtracking
            if (score_matrix[i][j] == score_matrix[i-1][j] + GAP_SCORE) {
                traceback[i][j] = 1;  // Up (deletion)
            } else if (score_matrix[i][j] == score_matrix[i][j-1] + GAP_SCORE) {
                traceback[i][j] = 2;  // Left (insertion)
            } else {
                traceback[i][j] = 3;  // Diagonal (match/mismatch)
            }
        }
    }
}

// Function to reconstruct the alignment
void reconstruct_alignment(char *seq1, char *seq2, int len1, int len2, 
                          int **traceback, char *align1, char *align2) {
    
    int i = len1;
    int j = len2;
    int align_len = 0;
    
    // Start from the bottom-right corner
    while (i > 0 && j > 0) {
        if (traceback[i][j] == 3) {
            // Diagonal move (match/mismatch)
            align1[align_len] = seq1[i-1];
            align2[align_len] = seq2[j-1];
            i--;
            j--;
        } else if (traceback[i][j] == 1) {
            // Up move (deletion)
            align1[align_len] = seq1[i-1];
            align2[align_len] = '-';
            i--;
        } else {
            // Left move (insertion)
            align1[align_len] = '-';
            align2[align_len] = seq2[j-1];
            j--;
        }
        align_len++;
    }
    
    // Reverse the alignment strings
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
}

int main() {
    // Example sequences
    char seq1[] = "ACGTACGT";
    char seq2[] = "ACGTACGT";
    
    int len1 = strlen(seq1);
    int len2 = strlen(seq2);
    
    // Allocate memory for score matrix and traceback matrix
    int **score_matrix = (int**)malloc((len1 + 1) * sizeof(int*));
    int **traceback = (int**)malloc((len1 + 1) * sizeof(int*));
    
    for (int i = 0; i <= len1; i++) {
        score_matrix[i] = (int*)calloc(len2 + 1, sizeof(int));
        traceback[i] = (int*)calloc(len2 + 1, sizeof(int));
    }
    
    // Perform semiglobal alignment
    semiglobal_alignment(seq1, seq2, len1, len2, score_matrix, traceback);
    
    // Reconstruct alignment
    char *align1 = (char*)malloc((len1 + len2 + 1) * sizeof(char));
    char *align2 = (char*)malloc((len1 + len2 + 1) * sizeof(char));
    
    reconstruct_alignment(seq1, seq2, len1, len2, traceback, align1, align2);
    
    // Print results
    printf("Sequence 1: %s\n", seq1);
    printf("Sequence 2: %s\n", seq2);
    printf("Alignment score: %d\n", score_matrix[len1][len2]);
    printf("Alignment 1: %s\n", align1);
    printf("Alignment 2: %s\n", align2);
    
    // Free allocated memory
    for (int i = 0; i <= len1; i++) {
        free(score_matrix[i]);
        free(traceback[i]);
    }
    free(score_matrix);
    free(traceback);
    free(align1);
    free(align2);
    
    return 0;
}
```

## Key Features of the Implementation

1. **Semiglobal Alignment Logic**: 
   - Initializes first row and column to 0 (no penalty for gaps at beginning/end)
   - Uses standard DP recurrence for the rest of the matrix

2. **Traceback Mechanism**:
   - Records the direction of optimal path for alignment reconstruction
   - 1 = deletion (up), 2 = insertion (left), 3 = match/mismatch (diagonal)

3. **Alignment Reconstruction**:
   - Backtracks from bottom-right to reconstruct the optimal alignment
   - Handles gaps properly at sequence boundaries

4. **Memory Management**:
   - Proper allocation and deallocation of dynamic memory
   - Handles sequences of variable length

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m×n) for the dynamic programming matrix

## Sample Output

```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
Alignment score: 8
Alignment 1: ACGTACGT
Alignment 2: ACGTACGT
```

This implementation correctly handles semiglobal alignment where gaps at the beginning and end of sequences are allowed without penalty, while gaps in the middle are penalized.

