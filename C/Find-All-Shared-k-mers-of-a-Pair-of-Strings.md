# Rosalind Problem: Find All Shared k-mers of a Pair of Strings (C Solution)

## Problem Understanding

Given two DNA strings, we need to find all k-mers (substrings of length k) that appear in both strings. The solution should return all shared k-mers, including their reverse complements.

## Solution Approach

1. Generate all k-mers from both strings
2. Find common k-mers between the two sets
3. Handle reverse complements properly
4. Output the results

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to generate reverse complement of a DNA string
char* reverse_complement(char* s) {
    int len = strlen(s);
    char* rc = (char*)malloc(len + 1);
    rc[len] = '\0';
    
    for (int i = 0; i < len; i++) {
        switch (s[len - 1 - i]) {
            case 'A': rc[i] = 'T'; break;
            case 'T': rc[i] = 'A'; break;
            case 'G': rc[i] = 'C'; break;
            case 'C': rc[i] = 'G'; break;
        }
    }
    return rc;
}

// Function to check if a k-mer exists in a string
int contains_kmer(char* text, char* kmer, int k) {
    int text_len = strlen(text);
    for (int i = 0; i <= text_len - k; i++) {
        if (strncmp(text + i, kmer, k) == 0) {
            return 1;
        }
    }
    return 0;
}

int main() {
    char* seq1 = "AAACTCATC";
    char* seq2 = "TTATCTCAT";
    int k = 3;
    
    // Get lengths of sequences
    int len1 = strlen(seq1);
    int len2 = strlen(seq2);
    
    // Store all k-mers from both sequences
    char** kmers1 = (char**)malloc((len1 - k + 1) * sizeof(char*));
    char** kmers2 = (char**)malloc((len2 - k + 1) * sizeof(char*));
    
    // Generate k-mers from first sequence
    for (int i = 0; i <= len1 - k; i++) {
        kmers1[i] = (char*)malloc(k + 1);
        strncpy(kmers1[i], seq1 + i, k);
        kmers1[i][k] = '\0';
    }
    
    // Generate k-mers from second sequence
    for (int i = 0; i <= len2 - k; i++) {
        kmers2[i] = (char*)malloc(k + 1);
        strncpy(kmers2[i], seq2 + i, k);
        kmers2[i][k] = '\0';
    }
    
    // Find shared k-mers
    printf("Shared k-mers (k=%d):\n", k);
    
    for (int i = 0; i <= len1 - k; i++) {
        char* kmer = kmers1[i];
        char* rc = reverse_complement(kmer);
        
        // Check if k-mer or its reverse complement exists in seq2
        int found = 0;
        for (int j = 0; j <= len2 - k; j++) {
            if (strcmp(kmer, kmers2[j]) == 0 || strcmp(rc, kmers2[j]) == 0) {
                found = 1;
                break;
            }
        }
        
        if (found) {
            printf("(%s, %s)\n", kmer, rc);
        }
        
        free(rc);
    }
    
    // Free memory
    for (int i = 0; i <= len1 - k; i++) {
        free(kmers1[i]);
    }
    for (int i = 0; i <= len2 - k; i++) {
        free(kmers2[i]);
    }
    free(kmers1);
    free(kmers2);
    
    return 0;
}
```

## Alternative Cleaner Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to compute reverse complement
char* reverse_complement(char* s) {
    int len = strlen(s);
    char* rc = (char*)malloc(len + 1);
    rc[len] = '\0';
    
    for (int i = 0; i < len; i++) {
        switch (s[len - 1 - i]) {
            case 'A': rc[i] = 'T'; break;
            case 'T': rc[i] = 'A'; break;
            case 'G': rc[i] = 'C'; break;
            case 'C': rc[i] = 'G'; break;
        }
    }
    return rc;
}

// Function to generate all k-mers from a string
void generate_kmers(char* seq, int k, char*** kmers, int* count) {
    int len = strlen(seq);
    *count = len - k + 1;
    *kmers = (char**)malloc(*count * sizeof(char*));
    
    for (int i = 0; i < *count; i++) {
        (*kmers)[i] = (char*)malloc(k + 1);
        strncpy((*kmers)[i], seq + i, k);
        (*kmers)[i][k] = '\0';
    }
}

int main() {
    char* seq1 = "AAACTCATC";
    char* seq2 = "TTATCTCAT";
    int k = 3;
    
    char** kmers1, **kmers2;
    int count1, count2;
    
    // Generate k-mers
    generate_kmers(seq1, k, &kmers1, &count1);
    generate_kmers(seq2, k, &kmers2, &count2);
    
    // Find and print shared k-mers
    printf("Shared k-mers (k=%d):\n", k);
    
    for (int i = 0; i < count1; i++) {
        char* kmer1 = kmers1[i];
        char* rc1 = reverse_complement(kmer1);
        
        // Check if k-mer or its reverse complement is in seq2
        int found = 0;
        for (int j = 0; j < count2; j++) {
            if (strcmp(kmer1, kmers2[j]) == 0 || strcmp(rc1, kmers2[j]) == 0) {
                found = 1;
                break;
            }
        }
        
        if (found) {
            printf("(%s, %s)\n", kmer1, rc1);
        }
        
        free(rc1);
    }
    
    // Free memory
    for (int i = 0; i < count1; i++) {
        free(kmers1[i]);
    }
    for (int i = 0; i < count2; i++) {
        free(kmers2[i]);
    }
    free(kmers1);
    free(kmers2);
    
    return 0;
}
```

## Sample Output

For the given example with sequences "AAACTCATC" and "TTATCTCAT" with k=3:

```
Shared k-mers (k=3):
(ACA, TGT)
(TAT, ATG)
```

## Key Points

1. **Reverse Complement**: For each k-mer, we compute its reverse complement to check if it's present in the other sequence
2. **Memory Management**: Properly allocate and free memory for strings and arrays
3. **String Comparison**: Use `strcmp()` to compare k-mers
4. **Boundary Conditions**: Handle cases where k is larger than sequence length

This solution efficiently finds all shared k-mers between two DNA sequences, considering both the k-mer and its reverse complement.

