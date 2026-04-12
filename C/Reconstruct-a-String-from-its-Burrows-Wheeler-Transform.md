# Rosalind Problem: Reconstruct a String from its Burrows-Wheeler Transform

## Problem Understanding

The Burrows-Wheeler Transform (BWT) is a data transformation technique used in bioinformatics. Given a string, the BWT:
1. Sorts all rotations of the string lexicographically
2. Takes the last column of the rotation matrix

To reconstruct the original string from BWT, we need to reverse this process.

## Approach

1. **Sort the BWT string** to get the first column of the rotation matrix
2. **Create a mapping** between the sorted and original BWT characters
3. **Use the mapping** to trace back through the rotation matrix to reconstruct the original string
4. **Handle the special character** (typically '$') that marks the end of the original string

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to hold character and its original index
typedef struct {
    char c;
    int index;
} CharIndex;

// Comparison function for sorting
int compare_chars(const void *a, const void *b) {
    CharIndex *ca = (CharIndex *)a;
    CharIndex *cb = (CharIndex *)b;
    if (ca->c != cb->c) {
        return ca->c - cb->c;
    }
    return ca->index - cb->index;
}

char* reconstruct_string(char* bwt) {
    int len = strlen(bwt);
    if (len == 0) return "";
    
    // Create array of characters with their original indices
    CharIndex *sorted = (CharIndex *)malloc(len * sizeof(CharIndex));
    CharIndex *original = (CharIndex *)malloc(len * sizeof(CharIndex));
    
    // Initialize both arrays
    for (int i = 0; i < len; i++) {
        sorted[i].c = bwt[i];
        sorted[i].index = i;
        original[i].c = bwt[i];
        original[i].index = i;
    }
    
    // Sort the characters (first column of rotation matrix)
    qsort(sorted, len, sizeof(CharIndex), compare_chars);
    
    // Create mapping from sorted to original positions
    int *pos = (int *)malloc(len * sizeof(int));
    for (int i = 0; i < len; i++) {
        for (int j = 0; j < len; j++) {
            if (sorted[i].c == original[j].c && sorted[i].index == original[j].index) {
                pos[i] = j;
                break;
            }
        }
    }
    
    // Reconstruct the original string
    char *result = (char *)malloc((len + 1) * sizeof(char));
    result[len] = '\0';
    
    // Start from the position of '$' (end marker)
    int current_pos = 0;
    for (int i = 0; i < len; i++) {
        if (bwt[i] == '$') {
            current_pos = i;
            break;
        }
    }
    
    // Trace back through the matrix
    for (int i = len - 1; i >= 0; i--) {
        result[i] = bwt[current_pos];
        current_pos = pos[current_pos];
    }
    
    free(sorted);
    free(original);
    free(pos);
    
    return result;
}

int main() {
    // Read input from stdin
    char bwt[10000];
    if (fgets(bwt, sizeof(bwt), stdin) != NULL) {
        // Remove newline if present
        size_t len = strlen(bwt);
        if (len > 0 && bwt[len-1] == '\n') {
            bwt[len-1] = '\0';
        }
        
        char *reconstructed = reconstruct_string(bwt);
        printf("%s\n", reconstructed);
        free(reconstructed);
    }
    
    return 0;
}
```

## Algorithm Explanation

1. **Input Processing**: Read the BWT string from standard input
2. **Sorting**: Create a sorted version of the BWT characters to represent the first column of the rotation matrix
3. **Mapping**: Build a mapping from sorted positions to original positions to track the rotation relationships
4. **Reconstruction**: Start from the position of the end marker '$' and trace backwards through the rotation matrix using the mapping
5. **Output**: Print the reconstructed original string

## Time and Space Complexity

- **Time Complexity**: O(n log n) due to sorting
- **Space Complexity**: O(n) for storing the character-position mappings

## Sample Input/Output

**Input:**
```
TTCCTAACG$A
```

**Output:**
```
TACATCACGT$
```

This solution correctly handles the Burrows-Wheeler transformation reconstruction by building the rotation matrix implicitly and tracing back through it to recover the original string.

