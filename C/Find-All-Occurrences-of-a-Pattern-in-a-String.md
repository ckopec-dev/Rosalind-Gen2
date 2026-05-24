# Rosalind Problem: Find All Occurrences of a Pattern in a String (C Solution)

## Problem Description
Given two strings `pattern` and `text`, return all starting positions where `pattern` appears in `text` as a substring.

## Solution

```c
#include <stdio.h>
#include <string.h>

int main() {
    char pattern[10000];
    char text[10000];
    
    // Read input
    scanf("%s", pattern);
    scanf("%s", text);
    
    int pattern_len = strlen(pattern);
    int text_len = strlen(text);
    
    // Find all occurrences
    for (int i = 0; i <= text_len - pattern_len; i++) {
        // Check if pattern matches at position i
        if (strncmp(text + i, pattern, pattern_len) == 0) {
            printf("%d ", i + 1);  // 1-indexed output
        }
    }
    
    printf("\n");
    
    return 0;
}
```

## Explanation

1. **Input Reading**: We read two strings - `pattern` and `text` - from standard input.

2. **Length Calculation**: We calculate the lengths of both strings using `strlen()`.

3. **Pattern Matching**: We iterate through the text from position 0 to `text_len - pattern_len` (inclusive) to ensure we don't go out of bounds.

4. **String Comparison**: For each position `i`, we use `strncmp()` to compare the substring of `text` starting at position `i` with `pattern` for `pattern_len` characters.

5. **Output**: When a match is found, we print the 1-indexed position (adding 1 to the 0-indexed `i`).

## Example

**Input:**
```
ATAT
GATATATGCATATACTT
```

**Output:**
```
2 4 10
```

## Time Complexity
- **Time**: O(n × m) where n is the length of text and m is the length of pattern
- **Space**: O(1) additional space (excluding input storage)

## Alternative Implementation (More Efficient)

```c
#include <stdio.h>
#include <string.h>

int main() {
    char pattern[10000];
    char text[10000];
    
    scanf("%s", pattern);
    scanf("%s", text);
    
    int pattern_len = strlen(pattern);
    int text_len = strlen(text);
    int found = 0;
    
    for (int i = 0; i <= text_len - pattern_len; i++) {
        int match = 1;
        for (int j = 0; j < pattern_len; j++) {
            if (text[i + j] != pattern[j]) {
                match = 0;
                break;
            }
        }
        if (match) {
            printf("%d ", i + 1);
            found = 1;
        }
    }
    
    if (!found) {
        printf("No matches found\n");
    } else {
        printf("\n");
    }
    
    return 0;
}
```

This alternative implementation manually compares characters instead of using `strncmp()`, which might be more educational for understanding the underlying algorithm.

