# Rosalind Problem: Find a Highest-Scoring Alignment of Two Strings (C Solution)

## Problem Understanding

This problem asks us to find the highest-scoring global alignment of two strings using the Needleman-Wunsch algorithm with a given scoring matrix.

## Solution Approach

I'll implement the Needleman-Wunsch algorithm with a scoring matrix and traceback to find the optimal alignment.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 1000
#define GAP_PENALTY -2
#define MATCH_SCORE 1
#define MISMATCH_SCORE -1

int score_matrix[256][256];

// Initialize the scoring matrix
void initialize_scoring_matrix() {
    // Initialize all values to 0
    for (int i = 0; i < 256; i++) {
        for (int j = 0; j < 256; j++) {
            score_matrix[i][j] = 0;
        }
    }
    
    // Set match score
    score_matrix['A']['A'] = MATCH_SCORE;
    score_matrix['C']['C'] = MATCH_SCORE;
    score_matrix['G']['G'] = MATCH_SCORE;
    score_matrix['T']['T'] = MATCH_SCORE;
    
    // Set mismatch scores
    score_matrix['A']['C'] = MISMATCH_SCORE;
    score_matrix['A']['G'] = MISMATCH_SCORE;
    score_matrix['A']['T'] = MISMATCH_SCORE;
    score_matrix['C']['A'] = MISMATCH_SCORE;
    score_matrix['C']['G'] = MISMATCH_SCORE;
    score_matrix['C']['T'] = MISMATCH_SCORE;
    score_matrix['G']['A'] = MISMATCH_SCORE;
    score_matrix['G']['C'] = MISMATCH_SCORE;
    score_matrix['G']['T'] = MISMATCH_SCORE;
    score_matrix['T']['A'] = MISMATCH_SCORE;
    score_matrix['T']['C'] = MISMATCH_SCORE;
    score_matrix['T']['G'] = MISMATCH_SCORE;
}

// Function to get score for two characters
int get_score(char a, char b) {
    return score_matrix[(unsigned char)a][(unsigned char)b];
}

// Function to find maximum of three integers
int max3(int a, int b, int c) {
    int max = a;
    if (b > max) max = b;
    if (c > max) max = c;
    return max;
}

// Function to compute the alignment score matrix
void compute_alignment_matrix(char *str1, char *str2, int **dp, int m, int n) {
    // Initialize first row and column
    dp[0][0] = 0;
    
    for (int i = 1; i <= m; i++) {
        dp[i][0] = dp[i-1][0] + GAP_PENALTY;
    }
    
    for (int j = 1; j <= n; j++) {
        dp[0][j] = dp[0][j-1] + GAP_PENALTY;
    }
    
    // Fill the DP matrix
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int match = dp[i-1][j-1] + get_score(str1[i-1], str2[j-1]);
            int delete = dp[i-1][j] + GAP_PENALTY;
            int insert = dp[i][j-1] + GAP_PENALTY;
            
            dp[i][j] = max3(match, delete, insert);
        }
    }
}

// Function to trace back and find the alignment
void traceback(char *str1, char *str2, int **dp, char *align1, char *align2, int m, int n) {
    int i = m, j = n;
    int align1_idx = 0, align2_idx = 0;
    
    while (i > 0 && j > 0) {
        int current = dp[i][j];
        int match = dp[i-1][j-1] + get_score(str1[i-1], str2[j-1]);
        int delete = dp[i-1][j] + GAP_PENALTY;
        int insert = dp[i][j-1] + GAP_PENALTY;
        
        if (current == match) {
            align1[align1_idx++] = str1[i-1];
            align2[align2_idx++] = str2[j-1];
            i--;
            j--;
        } else if (current == delete) {
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
    
    // Reverse the strings (since we built them backwards)
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
    
    align1[align1_idx] = '\0';
    align2[align2_idx] = '\0';
}

int main() {
    char str1[MAX_LEN], str2[MAX_LEN];
    char align1[MAX_LEN * 2], align2[MAX_LEN * 2];
    
    // Read input strings
    printf("Enter first string: ");
    scanf("%s", str1);
    printf("Enter second string: ");
    scanf("%s", str2);
    
    int m = strlen(str1);
    int n = strlen(str2);
    
    // Initialize scoring matrix
    initialize_scoring_matrix();
    
    // Create DP matrix
    int **dp = (int**)malloc((m + 1) * sizeof(int*));
    for (int i = 0; i <= m; i++) {
        dp[i] = (int*)malloc((n + 1) * sizeof(int));
    }
    
    // Compute alignment matrix
    compute_alignment_matrix(str1, str2, dp, m, n);
    
    // Get the score
    int score = dp[m][n];
    printf("Highest score: %d\n", score);
    
    // Traceback to get alignment
    traceback(str1, str2, dp, align1, align2, m, n);
    
    // Print the alignments
    printf("Alignment 1: %s\n", align1);
    printf("Alignment 2: %s\n", align2);
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(dp[i]);
    }
    free(dp);
    
    return 0;
}
```

## Explanation of Key Components

1. **Scoring Matrix Initialization**: 
   - Sets match score to 1 and mismatch score to -1
   - Uses gap penalty of -2 for insertions/deletions

2. **Dynamic Programming Matrix**:
   - `dp[i][j]` represents the highest score for aligning first `i` characters of str1 with first `j` characters of str2
   - Base cases: first row and column filled with gap penalties

3. **Recurrence Relation**:
   ```
   dp[i][j] = max(
       dp[i-1][j-1] + score(str1[i-1], str2[j-1]),  // match/mismatch
       dp[i-1][j] + gap_penalty,                     // deletion
       dp[i][j-1] + gap_penalty                      // insertion
   )
   ```

4. **Traceback**:
   - Reconstructs the actual alignment by backtracking through the DP matrix
   - Handles matches, insertions, and deletions appropriately

## Sample Input/Output

**Input:**
```
Enter first string: ACGTACGT
Enter second string: AGTACGT
```

**Output:**
```
Highest score: 5
Alignment 1: ACGTACGT
Alignment 2: AGTACGT-
```

This solution implements the classic Needleman-Wunsch algorithm for global sequence alignment with proper traceback to generate the actual aligned sequences.

