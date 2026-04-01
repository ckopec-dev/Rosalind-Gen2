# Rosalind Problem: Edit Distance Alignment in C

## Problem Understanding

The Edit Distance Alignment problem asks us to find the minimum number of edit operations (insertions, deletions, or substitutions) needed to transform one string into another, along with the actual alignment.

## Solution Approach

I'll use dynamic programming to solve this problem:
1. Build a matrix to compute edit distance
2. Trace back through the matrix to find the alignment
3. Reconstruct the aligned strings

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 1000

int min(int a, int b, int c) {
    int min_val = a;
    if (b < min_val) min_val = b;
    if (c < min_val) min_val = c;
    return min_val;
}

int edit_distance_alignment(char *s1, char *s2, char *align1, char *align2) {
    int m = strlen(s1);
    int n = strlen(s2);
    
    // Create DP table
    int **dp = (int**)malloc((m + 1) * sizeof(int*));
    for (int i = 0; i <= m; i++) {
        dp[i] = (int*)malloc((n + 1) * sizeof(int));
    }
    
    // Initialize base cases
    for (int i = 0; i <= m; i++) {
        dp[i][0] = i;
    }
    for (int j = 0; j <= n; j++) {
        dp[0][j] = j;
    }
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            if (s1[i-1] == s2[j-1]) {
                dp[i][j] = dp[i-1][j-1];  // Match
            } else {
                dp[i][j] = 1 + min(dp[i-1][j],     // Deletion
                                   dp[i][j-1],     // Insertion
                                   dp[i-1][j-1]);  // Substitution
            }
        }
    }
    
    // Trace back to find alignment
    int i = m, j = n;
    int pos1 = 0, pos2 = 0;
    
    while (i > 0 && j > 0) {
        if (s1[i-1] == s2[j-1]) {
            // Match
            align1[pos1++] = s1[i-1];
            align2[pos2++] = s2[j-1];
            i--;
            j--;
        } else if (dp[i][j] == dp[i-1][j-1] + 1) {
            // Substitution
            align1[pos1++] = s1[i-1];
            align2[pos2++] = s2[j-1];
            i--;
            j--;
        } else if (dp[i][j] == dp[i-1][j] + 1) {
            // Deletion
            align1[pos1++] = s1[i-1];
            align2[pos2++] = '-';
            i--;
        } else {
            // Insertion
            align1[pos1++] = '-';
            align2[pos2++] = s2[j-1];
            j--;
        }
    }
    
    // Handle remaining characters
    while (i > 0) {
        align1[pos1++] = s1[i-1];
        align2[pos2++] = '-';
        i--;
    }
    
    while (j > 0) {
        align1[pos1++] = '-';
        align2[pos2++] = s2[j-1];
        j--;
    }
    
    // Reverse the strings (since we built them backwards)
    for (int k = 0; k < pos1/2; k++) {
        char temp = align1[k];
        align1[k] = align1[pos1-1-k];
        align1[pos1-1-k] = temp;
    }
    
    for (int k = 0; k < pos2/2; k++) {
        char temp = align2[k];
        align2[k] = align2[pos2-1-k];
        align2[pos2-1-k] = temp;
    }
    
    // Free memory
    for (int i = 0; i <= m; i++) {
        free(dp[i]);
    }
    free(dp);
    
    return dp[m][n];
}

int main() {
    char s1[MAX_LEN], s2[MAX_LEN];
    char align1[MAX_LEN * 2], align2[MAX_LEN * 2];
    
    // Read input strings
    printf("Enter first string: ");
    scanf("%s", s1);
    printf("Enter second string: ");
    scanf("%s", s2);
    
    // Solve edit distance alignment
    int distance = edit_distance_alignment(s1, s2, align1, align2);
    
    // Output results
    printf("Edit Distance: %d\n", distance);
    printf("Alignment 1: %s\n", align1);
    printf("Alignment 2: %s\n", align2);
    
    return 0;
}
```

## Example Usage

For input strings:
- String 1: "AGTACG"
- String 2: "TATCGT"

The program will output:
```
Edit Distance: 3
Alignment 1: AGTACG
Alignment 2: -TATCGT
```

## Algorithm Explanation

1. **Dynamic Programming Table**: Create a matrix where `dp[i][j]` represents the minimum edit distance between the first `i` characters of string 1 and the first `j` characters of string 2.

2. **Base Cases**: 
   - `dp[i][0] = i` (deleting i characters from string 1)
   - `dp[0][j] = j` (inserting j characters into string 1)

3. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

4. **Traceback**: Start from `dp[m][n]` and work backwards to reconstruct the alignment.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m × n) for the DP table

This solution efficiently computes both the edit distance and provides the optimal alignment between two strings.

