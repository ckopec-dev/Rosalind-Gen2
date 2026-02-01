# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings

## Problem Description
Given two strings, find the highest-scoring overlap alignment where the first string is a prefix and the second string is a suffix.

## Solution in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_LEN 1000
#define GAP_PENALTY -2
#define MATCH_SCORE 1
#define MISMATCH_SCORE -1

int score(char a, char b) {
    if (a == b) return MATCH_SCORE;
    else return MISMATCH_SCORE;
}

void print_matrix(int **matrix, int m, int n) {
    for (int i = 0; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void traceback(int **matrix, char *str1, char *str2, int m, int n, 
               char *align1, char *align2, int *score) {
    int i = m, j = n;
    int align1_idx = 0, align2_idx = 0;
    
    while (i > 0 && j > 0) {
        if (matrix[i][j] == matrix[i-1][j-1] + score(str1[i-1], str2[j-1])) {
            align1[align1_idx++] = str1[i-1];
            align2[align2_idx++] = str2[j-1];
            i--;
            j--;
        } else if (matrix[i][j] == matrix[i-1][j] + GAP_PENALTY) {
            align1[align1_idx++] = str1[i-1];
            align2[align2_idx++] = '-';
            i--;
        } else {
            align1[align1_idx++] = '-';
            align2[align2_idx++] = str2[j-1];
            j--;
        }
    }
    
    // Handle remaining characters
    while (i > 0) {
        align1[align1_idx++] = str1[i-1];
        align2[align2_idx++] = '-';
        i--;
    }
    
    while (j > 0) {
        align1[align1_idx++] = '-';
        align2[align2_idx++] = str2[j-1];
        j--;
    }
    
    // Reverse the strings
    for (int k = 0; k < align1_idx/2; k++) {
        char temp = align1[k];
        align1[k] = align1[align1_idx-1-k];
        align1[align1_idx-1-k] = temp;
    }
    
    for (int k = 0; k < align2_idx/2; k++) {
        char temp = align2[k];
        align2[k] = align2[align2_idx-1-k];
        align2[align2_idx-1-k] = temp;
    }
    
    *score = matrix[m][n];
}

int main() {
    char str1[MAX_LEN], str2[MAX_LEN];
    
    // Read input strings
    printf("Enter first string: ");
    scanf("%s", str1);
    printf("Enter second string: ");
    scanf("%s", str2);
    
    int m = strlen(str1);
    int n = strlen(str2);
    
    // Create DP matrix
    int **matrix = (int**)malloc((m + 1) * sizeof(int*));
    for (int i = 0; i <= m; i++) {
        matrix[i] = (int*)malloc((n + 1) * sizeof(int));
    }
    
    // Initialize first row and column
    matrix[0][0] = 0;
    for (int i = 1; i <= m; i++) {
        matrix[i][0] = matrix[i-1][0] + GAP_PENALTY;
    }
    for (int j = 1; j <= n; j++) {
        matrix[0][j] = 0;  // No penalty for starting alignment
    }
    
    // Fill the DP matrix
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int match = matrix[i-1][j-1] + score(str1[i-1], str2[j-1]);
            int delete = matrix[i-1][j] + GAP_PENALTY;
            int insert = matrix[i][j-1] + GAP_PENALTY;
            
            matrix[i][j] = fmax(fmax(match, delete), insert);
        }
    }
    
    // Find maximum score in last row (overlap alignment)
    int max_score = 0;
    int max_j = 0;
    for (int j = 0; j <= n; j++) {
        if (matrix[m][j] > max_score) {
            max_score = matrix[m][j];
            max_j = j;
        }
    }
    
    // Traceback from the maximum position
    char align1[MAX_LEN * 2];
    char align2[MAX_LEN * 2];
    int final_score;
    
    // Reset matrix for traceback
    for (int i = 0; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            if (i == 0) matrix[i][j] = 0;
            else if (j == 0) matrix[i][j] = matrix[i-1][j] + GAP_PENALTY;
            else {
                int match = matrix[i-1][j-1] + score(str1[i-1], str2[j-1]);
                int delete = matrix[i-1][j] + GAP_PENALTY;
                int insert = matrix[i][j-1] + GAP_PENALTY;
                matrix[i][j] = fmax(fmax(match, delete), insert);
            }
        }
    }
    
    // Perform traceback from position (m, max_j)
    int i = m, j = max_j;
    int align1_idx = 0, align2_idx = 0;
    
    // Reset alignment strings
    memset(align1, 0, sizeof(align1));
    memset(align2, 0, sizeof(align2));
    
    while (i > 0 && j > 0) {
        if (matrix[i][j] == matrix[i-1][j-1] + score(str1[i-1], str2[j-1])) {
            align1[align1_idx++] = str1[i-1];
            align2[align2_idx++] = str2[j-1];
            i--;
            j--;
        } else if (matrix[i][j] == matrix[i-1][j] + GAP_PENALTY) {
            align1[align1_idx++] = str1[i-1];
            align2[align2_idx++] = '-';
            i--;
        } else {
            align1[align1_idx++] = '-';
            align2[align2_idx++] = str2[j-1];
            j--;
        }
    }
    
    // Handle remaining characters
    while (i > 0) {
        align1[align1_idx++] = str1[i-1];
        align2[align2_idx++] = '-';
        i--;
    }
    
    while (j > 0) {
        align1[align1_idx++] = '-';
        align2[align2_idx++] = str2[j-1];
        j--;
    }
    
    // Reverse the strings
    for (int k = 0; k < align1_idx/2; k++) {
        char temp = align1[k];
        align1[k] = align1[align1_idx-1-k];
        align1[align1_idx-1-k] = temp;
    }
    
    for (int k = 0; k < align2_idx/2; k++) {
        char temp = align2[k];
        align2[k] = align2[align2_idx-1-k];
        align2[align2_idx-1-k] = temp;
    }
    
    // Output results
    printf("Maximum overlap score: %d\n", max_score);
    printf("Alignment:\n");
    printf("%s\n", align1);
    printf("%s\n", align2);
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(matrix[i]);
    }
    free(matrix);
    
    return 0;
}
```

## Explanation

This solution implements the overlap alignment algorithm for two strings:

1. **Dynamic Programming Matrix**: Create an m×n matrix where m and n are the lengths of the two strings
2. **Initialization**: 
   - First row is initialized to 0 (overlap alignment)
   - First column is initialized with gap penalties
3. **Matrix Filling**: For each cell, compute the maximum score from three possible operations:
   - Match/Mismatch (diagonal)
   - Insert gap in first string (up)
   - Insert gap in second string (left)
4. **Overlap Alignment**: The alignment is considered an overlap if the first string is a prefix and the second string is a suffix
5. **Traceback**: Start from the maximum score in the last row and trace back to construct the alignment

## Key Features

- Uses standard dynamic programming approach for sequence alignment
- Implements overlap alignment where one string is prefix and other is suffix
- Handles gap penalties correctly
- Performs proper traceback to generate the actual alignment
- Includes proper memory management

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the strings
- **Space Complexity**: O(m×n) for the DP matrix

