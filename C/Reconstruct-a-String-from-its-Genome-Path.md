# Rosalind Problem: Reconstruct a String from its Genome Path

## Problem Description
Given a sequence of k-mers that form a genome path, reconstruct the original DNA string. The genome path is a sequence of k-mers where each k-mer overlaps with the next one by k-1 nucleotides.

## Solution in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* reconstruct_string(char** path, int k, int path_length) {
    // Allocate memory for the result string
    // Length = (path_length - 1) * k + k = path_length * k - k + k = path_length * k
    char* result = (char*)malloc((path_length * k + 1) * sizeof(char));
    if (result == NULL) {
        return NULL;
    }
    
    // Copy the first k-mer
    strcpy(result, path[0]);
    
    // Append the last character of each subsequent k-mer
    for (int i = 1; i < path_length; i++) {
        // Append the last character of current k-mer
        strncat(result, &path[i][k-1], 1);
    }
    
    result[path_length * k] = '\0';  // Null terminate
    return result;
}

int main() {
    // Example input
    char* path[] = {"ACCGA", "CCGAA", "CGAAG", "GAAGC", "AAGCT"};
    int k = 5;
    int path_length = 5;
    
    // Reconstruct the string
    char* result = reconstruct_string(path, k, path_length);
    
    if (result != NULL) {
        printf("Reconstructed string: %s\n", result);
        free(result);
    } else {
        printf("Memory allocation failed\n");
        return 1;
    }
    
    return 0;
}
```

## Explanation

### Algorithm Approach
1. **Input**: A list of k-mers (genome path) where each k-mer overlaps with the next one by k-1 nucleotides
2. **Output**: The original DNA string that generated this path
3. **Strategy**: 
   - Start with the first k-mer
   - For each subsequent k-mer, append only the last character to the result
   - This works because of the overlapping property of the genome path

### Key Steps
1. **Memory Allocation**: Allocate sufficient memory for the reconstructed string
2. **Initialization**: Copy the first k-mer to the result
3. **Construction**: For each remaining k-mer, append only the last character
4. **Termination**: Add null terminator to make it a valid C string

### Time and Space Complexity
- **Time Complexity**: O(n × k) where n is the number of k-mers and k is the k-mer length
- **Space Complexity**: O(n × k) for storing the result string

### Example Walkthrough
Given path: `["ACCGA", "CCGAA", "CGAAG", "GAAGC", "AAGCT"]`
- Start with "ACCGA"
- Append last char of "CCGAA" → "ACCGAC"
- Append last char of "CGAAG" → "ACCGACC"
- Append last char of "GAAGC" → "ACCGACCG"
- Append last char of "AAGCT" → "ACCGACCGA"
- Final result: "ACCGACCGA"

## Sample Input/Output

**Input:**
```
ACCGA
CCGAA
CGAAG
GAAGC
AAGCT
```

**Output:**
```
ACCGACCGA
```

This solution efficiently reconstructs the original DNA string from its genome path representation.

