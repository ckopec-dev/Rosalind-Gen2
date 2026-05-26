# Rosalind Problem: Find the Reverse Complement of a String (C Solution)

## Problem Understanding
The reverse complement of a DNA string is formed by reversing the string and then taking the complement of each nucleotide (A↔T, C↔G).

## Solution

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char complement(char nucleotide) {
    switch (nucleotide) {
        case 'A': return 'T';
        case 'T': return 'A';
        case 'C': return 'G';
        case 'G': return 'C';
        default: return nucleotide; // Handle invalid characters
    }
}

void reverse_complement(char *dna_string, char *result) {
    int length = strlen(dna_string);
    
    // Build reverse complement from right to left
    for (int i = 0; i < length; i++) {
        result[i] = complement(dna_string[length - 1 - i]);
    }
    result[length] = '\0'; // Null terminate the result
}

int main() {
    char dna_string[1000];
    char result[1000];
    
    // Read input DNA string
    printf("Enter DNA string: ");
    scanf("%s", dna_string);
    
    // Calculate reverse complement
    reverse_complement(dna_string, result);
    
    // Print result
    printf("Reverse complement: %s\n", result);
    
    return 0;
}
```

## Example Usage

**Input:**
```
AAAACCCGGT
```

**Output:**
```
Reverse complement: ACCGGGTTTT
```

## How It Works

1. **Complement Function**: Maps each nucleotide to its complement:
   - A → T
   - T → A  
   - C → G
   - G → C

2. **Reverse Complement Function**: 
   - Takes the input DNA string
   - Processes it from right to left (reversing)
   - Applies complement to each nucleotide
   - Stores result in the output array

3. **Main Function**:
   - Reads the DNA string from user input
   - Calls reverse_complement function
   - Prints the final result

## Alternative Version (More Robust)

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char complement(char nucleotide) {
    switch (nucleotide) {
        case 'A': return 'T';
        case 'T': return 'A';
        case 'C': return 'G';
        case 'G': return 'C';
        default: 
            fprintf(stderr, "Invalid nucleotide: %c\n", nucleotide);
            exit(1);
    }
}

char* reverse_complement(char *dna_string) {
    int length = strlen(dna_string);
    char *result = (char*)malloc((length + 1) * sizeof(char));
    
    if (result == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    
    for (int i = 0; i < length; i++) {
        result[i] = complement(dna_string[length - 1 - i]);
    }
    result[length] = '\0';
    
    return result;
}

int main() {
    char dna_string[1000];
    
    printf("Enter DNA string: ");
    if (scanf("%s", dna_string) != 1) {
        fprintf(stderr, "Error reading input\n");
        return 1;
    }
    
    char *result = reverse_complement(dna_string);
    printf("Reverse complement: %s\n", result);
    
    free(result);
    return 0;
}
```

This solution handles the reverse complement problem efficiently with O(n) time complexity where n is the length of the DNA string.

