# Finding a Motif in DNA - Rosalind Problem Solution in C

## Problem Understanding
Given two DNA strings, find all locations where the pattern (motif) occurs in the text (DNA sequence). The positions should be 1-indexed.

## Solution

```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[10000];
    char pattern[10000];
    
    // Read the DNA sequence and pattern
    scanf("%s", text);
    scanf("%s", pattern);
    
    int text_len = strlen(text);
    int pattern_len = strlen(pattern);
    int positions[10000];  // Array to store positions
    int count = 0;
    
    // Search for pattern in text
    for (int i = 0; i <= text_len - pattern_len; i++) {
        // Check if pattern matches at position i
        if (strncmp(text + i, pattern, pattern_len) == 0) {
            positions[count] = i + 1;  // 1-indexed positions
            count++;
        }
    }
    
    // Print all positions
    for (int i = 0; i < count; i++) {
        printf("%d ", positions[i]);
    }
    printf("\n");
    
    return 0;
}
```

## Explanation

1. **Input Reading**: 
   - Read the DNA sequence (text) and pattern (motif) from standard input
   - Both strings are stored in character arrays

2. **Pattern Search**:
   - Iterate through the text from position 0 to (text_length - pattern_length)
   - Use `strncmp()` to compare substring of text with pattern
   - If match found, store the 1-indexed position in the positions array

3. **Output**:
   - Print all found positions separated by spaces
   - Positions are 1-indexed as required by the problem

## Example

**Input:**
```
GATATATGCATATACTT
ATAT
```

**Output:**
```
2 4 10
```

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the length of text and m is the length of pattern
- **Space Complexity**: O(k) where k is the number of matches found

## Alternative Implementation with Manual String Comparison

```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[10000];
    char pattern[10000];
    
    scanf("%s", text);
    scanf("%s", pattern);
    
    int text_len = strlen(text);
    int pattern_len = strlen(pattern);
    int positions[10000];
    int count = 0;
    
    // Manual pattern matching
    for (int i = 0; i <= text_len - pattern_len; i++) {
        int match = 1;
        for (int j = 0; j < pattern_len; j++) {
            if (text[i + j] != pattern[j]) {
                match = 0;
                break;
            }
        }
        if (match) {
            positions[count] = i + 1;
            count++;
        }
    }
    
    for (int i = 0; i < count; i++) {
        printf("%d ", positions[i]);
    }
    printf("\n");
    
    return 0;
}
```

This solution handles the Rosalind problem "Finding a Motif in DNA" efficiently and correctly.

