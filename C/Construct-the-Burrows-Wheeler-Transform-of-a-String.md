```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to compare two strings for qsort
int compare_strings(const void *a, const void *b) {
    return strcmp(*(const char **)a, *(const char **)b);
}

int main() {
    char input[1000];
    
    // Read input string
    fgets(input, sizeof(input), stdin);
    
    // Remove newline character if present
    input[strcspn(input, "\n")] = 0;
    
    int len = strlen(input);
    
    // Create array of all rotations
    char **rotations = (char **)malloc(len * sizeof(char *));
    for (int i = 0; i < len; i++) {
        rotations[i] = (char *)malloc((len + 1) * sizeof(char));
    }
    
    // Generate all rotations
    for (int i = 0; i < len; i++) {
        for (int j = 0; j < len; j++) {
            rotations[i][j] = input[(i + j) % len];
        }
        rotations[i][len] = '\0';
    }
    
    // Sort rotations lexicographically
    qsort(rotations, len, sizeof(char *), compare_strings);
    
    // Extract last column (BWT)
    char bwt[1000];
    for (int i = 0; i < len; i++) {
        bwt[i] = rotations[i][len - 1];
    }
    bwt[len] = '\0';
    
    // Print result
    printf("%s\n", bwt);
    
    // Free memory
    for (int i = 0; i < len; i++) {
        free(rotations[i]);
    }
    free(rotations);
    
    return 0;
}
```

