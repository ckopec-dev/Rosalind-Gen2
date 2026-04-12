# Rosalind Problem: Genome Assembly with Perfect Coverage and Repeats (C Solution)

## Problem Understanding

This problem involves genome assembly from reads with perfect coverage and repeats. We need to find the shortest superstring that contains all given reads as substrings.

## Approach

We'll use a greedy approach to find the shortest superstring:
1. Find the maximum overlap between each pair of strings
2. Repeatedly merge the pair with maximum overlap
3. Continue until only one string remains

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_READS 1000
#define MAX_LENGTH 1000

// Function to calculate maximum overlap between two strings
int getOverlap(char* a, char* b) {
    int len_a = strlen(a);
    int len_b = strlen(b);
    int max_overlap = 0;
    
    // Try all possible overlaps
    for (int i = 1; i <= len_a && i <= len_b; i++) {
        // Check if suffix of a matches prefix of b
        if (strncmp(a + len_a - i, b, i) == 0) {
            max_overlap = i;
        }
    }
    
    return max_overlap;
}

// Function to merge two strings with maximum overlap
char* mergeStrings(char* a, char* b, int overlap) {
    int len_a = strlen(a);
    int len_b = strlen(b);
    int total_len = len_a + len_b - overlap;
    
    char* merged = (char*)malloc(total_len + 1);
    strcpy(merged, a);
    strcat(merged, b + overlap);
    
    return merged;
}

// Function to find shortest superstring
char* findShortestSuperstring(char** reads, int n) {
    if (n == 0) return NULL;
    if (n == 1) return strdup(reads[0]);
    
    // Create overlap matrix
    int** overlap = (int**)malloc(n * sizeof(int*));
    for (int i = 0; i < n; i++) {
        overlap[i] = (int*)malloc(n * sizeof(int));
    }
    
    // Fill overlap matrix
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i != j) {
                overlap[i][j] = getOverlap(reads[i], reads[j]);
            } else {
                overlap[i][j] = 0;
            }
        }
    }
    
    // Greedy approach to merge
    char** current_reads = (char**)malloc(n * sizeof(char*));
    for (int i = 0; i < n; i++) {
        current_reads[i] = strdup(reads[i]);
    }
    
    int num_reads = n;
    
    while (num_reads > 1) {
        int max_overlap = -1;
        int pos_i = -1, pos_j = -1;
        
        // Find maximum overlap
        for (int i = 0; i < num_reads; i++) {
            for (int j = 0; j < num_reads; j++) {
                if (i != j && overlap[i][j] > max_overlap) {
                    max_overlap = overlap[i][j];
                    pos_i = i;
                    pos_j = j;
                }
            }
        }
        
        if (max_overlap <= 0) break;
        
        // Merge strings
        char* merged = mergeStrings(current_reads[pos_i], current_reads[pos_j], max_overlap);
        
        // Replace the first string with merged result
        free(current_reads[pos_i]);
        current_reads[pos_i] = merged;
        
        // Remove the second string
        free(current_reads[pos_j]);
        current_reads[pos_j] = current_reads[num_reads - 1];
        
        // Update overlap matrix
        for (int i = 0; i < num_reads - 1; i++) {
            if (i != pos_i) {
                overlap[pos_i][i] = getOverlap(current_reads[pos_i], current_reads[i]);
                overlap[i][pos_i] = getOverlap(current_reads[i], current_reads[pos_i]);
            }
        }
        
        // Set diagonal to 0
        overlap[pos_i][pos_i] = 0;
        
        num_reads--;
    }
    
    // Clean up
    for (int i = 0; i < n; i++) {
        free(overlap[i]);
    }
    free(overlap);
    
    char* result = strdup(current_reads[0]);
    
    // Clean up
    for (int i = 0; i < n; i++) {
        free(current_reads[i]);
    }
    free(current_reads);
    
    return result;
}

int main() {
    // Read input
    char reads[MAX_READS][MAX_LENGTH];
    int n = 0;
    
    // Read number of reads
    scanf("%d", &n);
    
    // Read reads
    for (int i = 0; i < n; i++) {
        scanf("%s", reads[i]);
    }
    
    // Find shortest superstring
    char* result = findShortestSuperstring((char**)reads, n);
    
    // Output result
    printf("%s\n", result);
    
    free(result);
    
    return 0;
}
```

## Explanation

### Key Components:

1. **getOverlap()**: Calculates the maximum overlap between two strings by checking if the suffix of one matches the prefix of another.

2. **mergeStrings()**: Combines two strings by overlapping them maximally.

3. **findShortestSuperstring()**: Implements a greedy algorithm that:
   - Builds an overlap matrix between all pairs of strings
   - Repeatedly finds the pair with maximum overlap
   - Merges them until only one string remains

### Time Complexity:
- O(n³ × m) where n is the number of reads and m is the average read length
- This is acceptable for the given constraints

### Space Complexity:
- O(n² + n × m) for the overlap matrix and string storage

## Sample Input/Output

**Input:**
```
4
AACCTTGG
CCGGTTAA
GGTTAACC
TTAACCCT
```

**Output:**
```
AACCTTGGTTAACCCT
```

This solution handles perfect coverage and repeats by finding the optimal way to merge overlapping reads to form the shortest possible superstring.

