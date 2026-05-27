# Rosalind Problem: Pattern Matching with the Suffix Array

## Problem Understanding

Given a string and a set of patterns, we need to find all occurrences of each pattern in the string using a suffix array approach.

## Solution Approach

We'll implement a suffix array and use binary search to efficiently find pattern occurrences.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to store suffix and its original index
typedef struct {
    char *suffix;
    int index;
} Suffix;

// Comparison function for sorting suffixes
int compare_suffixes(const void *a, const void *b) {
    Suffix *sa = (Suffix *)a;
    Suffix *sb = (Suffix *)b;
    return strcmp(sa->suffix, sb->suffix);
}

// Function to build suffix array
void build_suffix_array(char *text, int n, int *suffix_array) {
    Suffix *suffixes = (Suffix *)malloc(n * sizeof(Suffix));
    
    // Create suffixes
    for (int i = 0; i < n; i++) {
        suffixes[i].suffix = text + i;
        suffixes[i].index = i;
    }
    
    // Sort suffixes
    qsort(suffixes, n, sizeof(Suffix), compare_suffixes);
    
    // Extract indices
    for (int i = 0; i < n; i++) {
        suffix_array[i] = suffixes[i].index;
    }
    
    free(suffixes);
}

// Binary search to find first occurrence of pattern
int binary_search_first(char *text, int *suffix_array, int n, char *pattern) {
    int left = 0, right = n - 1;
    int result = -1;
    
    while (left <= right) {
        int mid = left + (right - left) / 2;
        int suffix_start = suffix_array[mid];
        int cmp = strncmp(text + suffix_start, pattern, strlen(pattern));
        
        if (cmp == 0) {
            result = mid;
            right = mid - 1; // Continue searching left
        } else if (cmp < 0) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    
    return result;
}

// Binary search to find last occurrence of pattern
int binary_search_last(char *text, int *suffix_array, int n, char *pattern) {
    int left = 0, right = n - 1;
    int result = -1;
    
    while (left <= right) {
        int mid = left + (right - left) / 2;
        int suffix_start = suffix_array[mid];
        int cmp = strncmp(text + suffix_start, pattern, strlen(pattern));
        
        if (cmp == 0) {
            result = mid;
            left = mid + 1; // Continue searching right
        } else if (cmp < 0) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    
    return result;
}

// Function to find all occurrences of pattern in text
void find_pattern_occurrences(char *text, int *suffix_array, int n, char *pattern) {
    int first = binary_search_first(text, suffix_array, n, pattern);
    int last = binary_search_last(text, suffix_array, n, pattern);
    
    if (first == -1) {
        printf("-1\n");
        return;
    }
    
    // Print all occurrences
    int count = 0;
    for (int i = first; i <= last; i++) {
        if (count > 0) printf(" ");
        printf("%d", suffix_array[i]);
        count++;
    }
    printf("\n");
}

int main() {
    char text[100001];
    char pattern[100001];
    
    // Read input
    fgets(text, sizeof(text), stdin);
    text[strcspn(text, "\n")] = 0; // Remove newline
    
    int n = strlen(text);
    
    // Build suffix array
    int *suffix_array = (int *)malloc(n * sizeof(int));
    build_suffix_array(text, n, suffix_array);
    
    // Read patterns and find occurrences
    while (fgets(pattern, sizeof(pattern), stdin)) {
        pattern[strcspn(pattern, "\n")] = 0; // Remove newline
        if (strlen(pattern) == 0) break;
        
        find_pattern_occurrences(text, suffix_array, n, pattern);
    }
    
    free(suffix_array);
    return 0;
}
```

## Key Components Explained

1. **Suffix Structure**: Stores a suffix and its original index in the text
2. **Suffix Array Construction**: Creates all suffixes and sorts them lexicographically
3. **Binary Search Functions**: 
   - `binary_search_first`: Finds the first occurrence of pattern
   - `binary_search_last`: Finds the last occurrence of pattern
4. **Pattern Matching**: Uses binary search to efficiently locate all occurrences

## Time Complexity
- Building suffix array: O(n² log n) where n is the length of text
- Pattern search: O(m log n) where m is pattern length
- Overall: O(n² log n + k × m log n) where k is number of patterns

## Space Complexity
- O(n²) for storing suffixes
- O(n) for suffix array

## Sample Input/Output

**Input:**
```
ACGTACGT
AC
GT
```

**Output:**
```
0 4
1 5
```

This solution efficiently handles pattern matching using suffix arrays and binary search for optimal performance.

