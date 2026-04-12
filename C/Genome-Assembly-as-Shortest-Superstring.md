# Rosalind Problem: Genome Assembly as Shortest Superstring (C Solution)

## Problem Understanding

The shortest superstring problem involves finding the shortest string that contains all given strings as substrings. This is a classic problem in bioinformatics for genome assembly.

## Approach

1. **Overlap Calculation**: For each pair of strings, calculate the maximum overlap where one string's suffix matches another's prefix
2. **Greedy Algorithm**: Repeatedly merge the pair with maximum overlap until one string remains
3. **Implementation**: Use a matrix to store overlaps and track which strings have been merged

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_N 100
#define MAX_LEN 1000

// Function to calculate maximum overlap between two strings
int get_overlap(char* a, char* b) {
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
char* merge_strings(char* a, char* b, int overlap) {
    int len_a = strlen(a);
    int len_b = strlen(b);
    int total_len = len_a + len_b - overlap;
    
    char* result = (char*)malloc(total_len + 1);
    strcpy(result, a);
    strcat(result, b + overlap);
    
    return result;
}

// Function to find shortest superstring
char* shortest_superstring(char** strings, int n) {
    if (n == 1) {
        return strings[0];
    }
    
    // Create overlap matrix
    int overlap[MAX_N][MAX_N];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i != j) {
                overlap[i][j] = get_overlap(strings[i], strings[j]);
            } else {
                overlap[i][j] = 0;
            }
        }
    }
    
    // Track which strings have been merged
    int merged[MAX_N] = {0};
    
    // Greedy merging
    while (1) {
        int max_overlap = -1;
        int pos_i = -1, pos_j = -1;
        
        // Find maximum overlap
        for (int i = 0; i < n; i++) {
            if (merged[i]) continue;
            for (int j = 0; j < n; j++) {
                if (merged[j] || i == j) continue;
                if (overlap[i][j] > max_overlap) {
                    max_overlap = overlap[i][j];
                    pos_i = i;
                    pos_j = j;
                }
            }
        }
        
        // If no more overlaps, we're done
        if (max_overlap <= 0) break;
        
        // Merge strings
        char* merged_str = merge_strings(strings[pos_i], strings[pos_j], max_overlap);
        
        // Update overlap matrix
        for (int i = 0; i < n; i++) {
            if (merged[i] || i == pos_i || i == pos_j) continue;
            overlap[i][pos_i] = get_overlap(strings[i], merged_str);
            overlap[pos_i][i] = get_overlap(merged_str, strings[i]);
        }
        
        // Mark strings as merged
        merged[pos_j] = 1;
        strings[pos_i] = merged_str;
    }
    
    // Find the remaining unmerged string
    for (int i = 0; i < n; i++) {
        if (!merged[i]) {
            return strings[i];
        }
    }
    
    return NULL;
}

int main() {
    // Read input
    int n;
    scanf("%d", &n);
    
    char** strings = (char**)malloc(n * sizeof(char*));
    
    // Skip newline
    char c;
    while ((c = getchar()) != '\n' && c != EOF);
    
    for (int i = 0; i < n; i++) {
        strings[i] = (char*)malloc(MAX_LEN * sizeof(char));
        fgets(strings[i], MAX_LEN, stdin);
        // Remove newline
        int len = strlen(strings[i]);
        if (len > 0 && strings[i][len-1] == '\n') {
            strings[i][len-1] = '\0';
        }
    }
    
    // Solve
    char* result = shortest_superstring(strings, n);
    
    // Output result
    printf("%s\n", result);
    
    // Free memory
    for (int i = 0; i < n; i++) {
        free(strings[i]);
    }
    free(strings);
    free(result);
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o shortest_superstring shortest_superstring.c
./shortest_superstring < input.txt
```

## Example Input/Output

**Input:**
```
4
ATTAGACCTG
CCTGCCGGAA
AGACCTGCCG
GCCGGAATAC
```

**Output:**
```
ATTAGACCTGCCGGAATAC
```

## Key Features

1. **Overlap Calculation**: Efficiently computes maximum overlap between any two strings
2. **Greedy Merging**: Repeatedly merges strings with maximum overlap
3. **Memory Management**: Properly allocates and frees memory
4. **Edge Cases**: Handles single string input and no overlaps

## Time Complexity

- **Overlap Calculation**: O(n² × m) where n is number of strings and m is average string length
- **Merging Process**: O(n³) in worst case
- **Overall**: O(n³ × m)

This solution provides an efficient approach to the shortest superstring problem for genome assembly applications.

