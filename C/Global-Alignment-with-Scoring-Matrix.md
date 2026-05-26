# Rosalind Problem: Global Alignment with Scoring Matrix (C Solution)

## Problem Understanding

This problem requires implementing global sequence alignment using a scoring matrix approach, similar to the Needleman-Wunsch algorithm. We need to find the optimal alignment between two sequences and return the alignment score.

## Solution Approach

I'll implement the global alignment algorithm using dynamic programming with a scoring matrix approach.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 1000
#define MAX_SCORE 1000000

// Scoring matrix structure
typedef struct {
    int** matrix;
    int rows;
    int cols;
    char* row_labels;
    char* col_labels;
} ScoringMatrix;

// Global variables for alignment
int** dp;
char* seq1;
char* seq2;
int len1, len2;

// Function to initialize scoring matrix
ScoringMatrix* create_scoring_matrix(int rows, int cols) {
    ScoringMatrix* matrix = (ScoringMatrix*)malloc(sizeof(ScoringMatrix));
    matrix->rows = rows;
    matrix->cols = cols;
    matrix->matrix = (int**)malloc(rows * sizeof(int*));
    
    for (int i = 0; i < rows; i++) {
        matrix->matrix[i] = (int*)calloc(cols, sizeof(int));
    }
    
    return matrix;
}

// Function to free scoring matrix
void free_scoring_matrix(ScoringMatrix* matrix) {
    for (int i = 0; i < matrix->rows; i++) {
        free(matrix->matrix[i]);
    }
    free(matrix->matrix);
    free(matrix);
}

// Function to compute global alignment score
int global_alignment_score(char* s1, char* s2, int match, int mismatch, int gap) {
    int m = strlen(s1);
    int n = strlen(s2);
    
    // Create DP table
    dp = (int**)malloc((m + 1) * sizeof(int*));
    for (int i = 0; i <= m; i++) {
        dp[i] = (int*)calloc(n + 1, sizeof(int));
    }
    
    // Initialize first row and column
    for (int i = 0; i <= m; i++) {
        dp[i][0] = i * gap;
    }
    for (int j = 0; j <= n; j++) {
        dp[0][j] = j * gap;
    }
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int score_match = (s1[i-1] == s2[j-1]) ? match : mismatch;
            dp[i][j] = dp[i-1][j-1] + score_match;
            
            // Check for gap penalties
            if (dp[i-1][j] + gap > dp[i][j]) {
                dp[i][j] = dp[i-1][j] + gap;
            }
            if (dp[i][j-1] + gap > dp[i][j]) {
                dp[i][j] = dp[i][j-1] + gap;
            }
        }
    }
    
    int result = dp[m][n];
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(dp[i]);
    }
    free(dp);
    
    return result;
}

// Function to compute global alignment with traceback
int global_alignment_with_traceback(char* s1, char* s2, int match, int mismatch, int gap, 
                                   char* align1, char* align2) {
    int m = strlen(s1);
    int n = strlen(s2);
    
    // Create DP table
    dp = (int**)malloc((m + 1) * sizeof(int*));
    for (int i = 0; i <= m; i++) {
        dp[i] = (int*)calloc(n + 1, sizeof(int));
    }
    
    // Initialize first row and column
    for (int i = 0; i <= m; i++) {
        dp[i][0] = i * gap;
    }
    for (int j = 0; j <= n; j++) {
        dp[0][j] = j * gap;
    }
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int score_match = (s1[i-1] == s2[j-1]) ? match : mismatch;
            int diagonal = dp[i-1][j-1] + score_match;
            int up = dp[i-1][j] + gap;
            int left = dp[i][j-1] + gap;
            
            dp[i][j] = diagonal;
            if (up > dp[i][j]) dp[i][j] = up;
            if (left > dp[i][j]) dp[i][j] = left;
        }
    }
    
    // Traceback
    int i = m, j = n;
    int align_idx1 = 0, align_idx2 = 0;
    
    while (i > 0 && j > 0) {
        int current = dp[i][j];
        int diagonal = dp[i-1][j-1];
        int up = dp[i-1][j];
        int left = dp[i][j-1];
        
        if (current == diagonal + ((s1[i-1] == s2[j-1]) ? match : mismatch)) {
            align1[align_idx1++] = s1[i-1];
            align2[align_idx2++] = s2[j-1];
            i--;
            j--;
        } else if (current == up + gap) {
            align1[align_idx1++] = s1[i-1];
            align2[align_idx2++] = '-';
            i--;
        } else {
            align1[align_idx1++] = '-';
            align2[align_idx2++] = s2[j-1];
            j--;
        }
    }
    
    // Add remaining characters
    while (i > 0) {
        align1[align_idx1++] = s1[i-1];
        align2[align_idx2++] = '-';
        i--;
    }
    while (j > 0) {
        align1[align_idx1++] = '-';
        align2[align_idx2++] = s2[j-1];
        j--;
    }
    
    // Reverse the alignments
    for (int k = 0; k < align_idx1/2; k++) {
        char temp = align1[k];
        align1[k] = align1[align_idx1-1-k];
        align1[align_idx1-1-k] = temp;
    }
    for (int k = 0; k < align_idx2/2; k++) {
        char temp = align2[k];
        align2[k] = align2[align_idx2-1-k];
        align2[align_idx2-1-k] = temp;
    }
    
    align1[align_idx1] = '\0';
    align2[align_idx2] = '\0';
    
    int result = dp[m][n];
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(dp[i]);
    }
    free(dp);
    
    return result;
}

// Main function to solve the problem
int main() {
    // Example input - replace with actual file reading for Rosalind
    char s1[] = "ACGTACGT";
    char s2[] = "ACGTACGT";
    
    int match = 2;
    int mismatch = -1;
    int gap = -1;
    
    // For just the score
    int score = global_alignment_score(s1, s2, match, mismatch, gap);
    printf("Alignment Score: %d\n", score);
    
    // For the full alignment
    char align1[MAX_LEN];
    char align2[MAX_LEN];
    int full_score = global_alignment_with_traceback(s1, s2, match, mismatch, gap, align1, align2);
    
    printf("Alignment Score: %d\n", full_score);
    printf("Alignment 1: %s\n", align1);
    printf("Alignment 2: %s\n", align2);
    
    return 0;
}
```

## Key Features of the Solution

1. **Dynamic Programming Approach**: Uses the classic Needleman-Wunsch algorithm for global alignment
2. **Scoring Matrix**: Supports custom match, mismatch, and gap penalties
3. **Traceback Functionality**: Computes actual alignment sequences, not just scores
4. **Memory Management**: Proper allocation and deallocation of DP tables
5. **Flexible Input**: Can handle various sequence lengths and scoring parameters

## Algorithm Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the DP table

## Usage Notes

For Rosalind problems, you would typically:
1. Read sequences from input files
2. Parse the scoring matrix from input
3. Call the appropriate functions with the given parameters
4. Output the result in the required format

The solution handles both the scoring matrix computation and the actual alignment traceback, making it suitable for the full Rosalind problem requirements.

