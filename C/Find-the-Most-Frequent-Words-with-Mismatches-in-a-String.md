# Rosalind Problem: Find the Most Frequent Words with Mismatches in a String

## Problem Understanding

We need to find the most frequent k-mers (substrings of length k) in a DNA string that can have up to d mismatches (Hamming distance). This means we're looking for patterns that appear frequently even when allowing for up to d substitutions.

## Solution Approach

1. Generate all possible k-mers of length k from the DNA string
2. For each k-mer, count how many times it appears with up to d mismatches
3. Find the maximum frequency
4. Return all k-mers that achieve this maximum frequency

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to calculate Hamming distance between two strings
int hamming_distance(char *s1, char *s2, int len) {
    int distance = 0;
    for (int i = 0; i < len; i++) {
        if (s1[i] != s2[i]) {
            distance++;
        }
    }
    return distance;
}

// Function to generate all possible k-mers of given length
void generate_kmers(char *pattern, char *kmers, int k, int pos, char *current) {
    if (pos == k) {
        current[pos] = '\0';
        strcpy(kmers + strlen(kmers) * k, current);
        return;
    }
    
    char bases[] = {'A', 'C', 'G', 'T'};
    for (int i = 0; i < 4; i++) {
        current[pos] = bases[i];
        generate_kmers(pattern, kmers, k, pos + 1, current);
    }
}

// Function to get all k-mers from a string
void get_all_kmers(char *text, int k, char *kmers, int *count) {
    int n = strlen(text);
    *count = 0;
    
    for (int i = 0; i <= n - k; i++) {
        char kmer[k + 1];
        strncpy(kmer, text + i, k);
        kmer[k] = '\0';
        
        // Check if this k-mer is already in our list
        int found = 0;
        for (int j = 0; j < *count; j++) {
            if (strcmp(kmer, kmers + j * k) == 0) {
                found = 1;
                break;
            }
        }
        
        if (!found) {
            strcpy(kmers + (*count) * k, kmer);
            (*count)++;
        }
    }
}

// Function to count occurrences of pattern in text with up to d mismatches
int count_with_mismatches(char *text, char *pattern, int k, int d) {
    int count = 0;
    int n = strlen(text);
    
    for (int i = 0; i <= n - k; i++) {
        char substring[k + 1];
        strncpy(substring, text + i, k);
        substring[k] = '\0';
        
        if (hamming_distance(pattern, substring, k) <= d) {
            count++;
        }
    }
    
    return count;
}

// Main function to solve the problem
void find_most_frequent_words_with_mismatches(char *text, int k, int d, char *result) {
    int n = strlen(text);
    int max_count = 0;
    char *frequent_words = malloc(n * k * sizeof(char));
    int frequent_count = 0;
    
    // Get all unique k-mers from the text
    char *all_kmers = malloc(n * k * sizeof(char));
    int kmers_count = 0;
    get_all_kmers(text, k, all_kmers, &kmers_count);
    
    // For each k-mer, count how many times it appears with up to d mismatches
    for (int i = 0; i < kmers_count; i++) {
        char current_kmer[k + 1];
        strncpy(current_kmer, all_kmers + i * k, k);
        current_kmer[k] = '\0';
        
        int count = count_with_mismatches(text, current_kmer, k, d);
        
        if (count > max_count) {
            max_count = count;
            frequent_count = 0;
            strcpy(frequent_words + frequent_count * k, current_kmer);
            frequent_count++;
        } else if (count == max_count) {
            // Check if this k-mer is not already in our result
            int already_exists = 0;
            for (int j = 0; j < frequent_count; j++) {
                if (strcmp(current_kmer, frequent_words + j * k) == 0) {
                    already_exists = 1;
                    break;
                }
            }
            if (!already_exists) {
                strcpy(frequent_words + frequent_count * k, current_kmer);
                frequent_count++;
            }
        }
    }
    
    // Copy result to output
    result[0] = '\0';
    for (int i = 0; i < frequent_count; i++) {
        strncat(result, frequent_words + i * k, k);
        if (i < frequent_count - 1) {
            strcat(result, " ");
        }
    }
    
    free(frequent_words);
    free(all_kmers);
}

// Simplified and more efficient approach
void solve_problem(char *text, int k, int d, char *output) {
    int n = strlen(text);
    int max_count = 0;
    char *frequent_words = malloc(n * k * sizeof(char));
    int frequent_count = 0;
    
    // For each possible k-mer in the text
    for (int i = 0; i <= n - k; i++) {
        char current_kmer[k + 1];
        strncpy(current_kmer, text + i, k);
        current_kmer[k] = '\0';
        
        int count = 0;
        
        // Count all occurrences with up to d mismatches
        for (int j = 0; j <= n - k; j++) {
            char substring[k + 1];
            strncpy(substring, text + j, k);
            substring[k] = '\0';
            
            if (hamming_distance(current_kmer, substring, k) <= d) {
                count++;
            }
        }
        
        // Update maximum and frequent words
        if (count > max_count) {
            max_count = count;
            frequent_count = 0;
            strcpy(frequent_words + frequent_count * k, current_kmer);
            frequent_count++;
        } else if (count == max_count) {
            // Check if this k-mer is not already in our result
            int already_exists = 0;
            for (int j = 0; j < frequent_count; j++) {
                if (strcmp(current_kmer, frequent_words + j * k) == 0) {
                    already_exists = 1;
                    break;
                }
            }
            if (!already_exists) {
                strcpy(frequent_words + frequent_count * k, current_kmer);
                frequent_count++;
            }
        }
    }
    
    // Format output
    output[0] = '\0';
    for (int i = 0; i < frequent_count; i++) {
        strncat(output, frequent_words + i * k, k);
        if (i < frequent_count - 1) {
            strcat(output, " ");
        }
    }
    
    free(frequent_words);
}

int main() {
    // Example input
    char text[] = "ACGTTGCATGTCGCATGATGCATGAGAGCT";
    int k = 4;
    int d = 1;
    
    char output[1000];
    solve_problem(text, k, d, output);
    
    printf("Input: %s\n", text);
    printf("k = %d, d = %d\n", k, d);
    printf("Most frequent words with mismatches: %s\n", output);
    
    return 0;
}
```

## How to Compile and Run

```bash
gcc -o frequent_words frequent_words.c
./frequent_words
```

## Key Points

1. **Hamming Distance**: Calculates the number of positions where two strings differ
2. **Mismatches**: For each k-mer, we count all occurrences within the text that have at most d mismatches
3. **Efficiency**: The solution has O(n²k) time complexity where n is the length of the string
4. **Memory Management**: Proper allocation and deallocation of memory for intermediate results

## Example

For input string "ACGTTGCATGTCGCATGATGCATGAGAGCT", k=4, d=1:
- The most frequent words with 1 mismatch are likely to be patterns that appear multiple times with at most 1 substitution
- Output would contain all such patterns that achieve maximum frequency

This implementation handles the core requirements of finding frequent k-mers with up to d mismatches in a DNA string.

