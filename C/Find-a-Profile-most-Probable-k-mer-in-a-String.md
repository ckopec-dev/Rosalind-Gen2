# Rosalind Problem: Find a Profile-most Probable k-mer in a String (C Solution)

## Problem Understanding

Given a string Text, an integer k, and a 4 × k matrix Profile, we need to find the k-mer in Text that has the highest probability according to the profile.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Function to convert nucleotide to index (A=0, C=1, G=2, T=3)
int nucleotide_to_index(char nucleotide) {
    switch (nucleotide) {
        case 'A': return 0;
        case 'C': return 1;
        case 'G': return 2;
        case 'T': return 3;
        default: return -1;
    }
}

// Function to find the profile-most probable k-mer
char* profile_most_probable_kmer(char* text, int k, double** profile) {
    int text_length = strlen(text);
    double max_probability = -1.0;
    char* result = (char*)malloc((k + 1) * sizeof(char));
    result[k] = '\0';
    
    // Iterate through all possible k-mers in the text
    for (int i = 0; i <= text_length - k; i++) {
        double probability = 1.0;
        char* kmer = (char*)malloc((k + 1) * sizeof(char));
        kmer[k] = '\0';
        
        // Extract k-mer
        for (int j = 0; j < k; j++) {
            kmer[j] = text[i + j];
        }
        
        // Calculate probability of this k-mer
        for (int j = 0; j < k; j++) {
            int index = nucleotide_to_index(kmer[j]);
            probability *= profile[index][j];
        }
        
        // Update maximum probability and result
        if (probability > max_probability) {
            max_probability = probability;
            strcpy(result, kmer);
        }
        
        free(kmer);
    }
    
    return result;
}

int main() {
    // Example input
    char text[] = "ACGTACGT";
    int k = 3;
    
    // Profile matrix (4 rows for A, C, G, T; k columns for each position)
    double profile[4][3] = {
        {0.2, 0.2, 0.3},  // A probabilities
        {0.4, 0.3, 0.1},  // C probabilities
        {0.3, 0.3, 0.5},  // G probabilities
        {0.1, 0.2, 0.1}   // T probabilities
    };
    
    // Convert 2D array to pointer array for function call
    double* profile_ptr[4];
    for (int i = 0; i < 4; i++) {
        profile_ptr[i] = profile[i];
    }
    
    // Find profile-most probable k-mer
    char* result = profile_most_probable_kmer(text, k, profile_ptr);
    
    printf("Text: %s\n", text);
    printf("k: %d\n", k);
    printf("Profile-most probable k-mer: %s\n", result);
    
    free(result);
    return 0;
}
```

## Explanation

1. **nucleotide_to_index()**: Converts nucleotides A, C, G, T to indices 0, 1, 2, 3 respectively for matrix access.

2. **profile_most_probable_kmer()**: 
   - Iterates through all possible k-mers in the input text
   - For each k-mer, calculates the probability using the profile matrix
   - The probability is calculated by multiplying the profile probabilities for each nucleotide at each position
   - Keeps track of the k-mer with the maximum probability

3. **Main function**:
   - Sets up example input (text, k, and profile matrix)
   - Converts the 2D array to pointer array format for function call
   - Calls the function and prints the result

## Time Complexity
- O(n × k) where n is the length of the text and k is the k-mer length
- For each of the (n-k+1) possible k-mers, we perform k multiplications

## Space Complexity
- O(k) for storing the result k-mer

## Sample Output
```
Text: ACGTACGT
k: 3
Profile-most probable k-mer: ACG
```

This solution handles the core requirements of finding the profile-most probable k-mer in a string using a given profile matrix.

