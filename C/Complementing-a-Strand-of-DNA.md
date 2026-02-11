# Rosalind Problem: Complementing a Strand of DNA (C Solution)

## Problem Understanding

Given a DNA string, we need to find its reverse complement. The reverse complement is formed by:
1. Taking the complement of each nucleotide (A↔T, C↔G)
2. Reversing the resulting string

## Solution Approach

1. Read the input DNA string
2. For each character, replace it with its complement
3. Reverse the complemented string
4. Output the result

## C Implementation

```c
#include <stdio.h>
#include <string.h>

int main() {
    char dna[1000];
    char complement[1000];
    
    // Read the DNA string
    scanf("%s", dna);
    
    int length = strlen(dna);
    
    // Generate complement and reverse it in one pass
    for (int i = 0; i < length; i++) {
        switch (dna[length - 1 - i]) {
            case 'A':
                complement[i] = 'T';
                break;
            case 'T':
                complement[i] = 'A';
                break;
            case 'C':
                complement[i] = 'G';
                break;
            case 'G':
                complement[i] = 'C';
                break;
            default:
                complement[i] = dna[length - 1 - i]; // Handle invalid characters
        }
    }
    
    complement[length] = '\0'; // Null terminate the string
    
    // Print the result
    printf("%s\n", complement);
    
    return 0;
}
```

## Alternative Implementation (More Readable)

```c
#include <stdio.h>
#include <string.h>

char get_complement(char nucleotide) {
    switch (nucleotide) {
        case 'A': return 'T';
        case 'T': return 'A';
        case 'C': return 'G';
        case 'G': return 'C';
        default: return nucleotide;
    }
}

int main() {
    char dna[1000];
    char complement[1000];
    
    // Read the DNA string
    scanf("%s", dna);
    
    int length = strlen(dna);
    
    // Generate reverse complement
    for (int i = 0; i < length; i++) {
        complement[i] = get_complement(dna[length - 1 - i]);
    }
    
    complement[length] = '\0'; // Null terminate
    
    // Print the result
    printf("%s\n", complement);
    
    return 0;
}
```

## Sample Input/Output

**Input:**
```
AAAACCCGGT
```

**Output:**
```
ACCGGGTTTT
```

## Explanation

For the input "AAAACCCGGT":
1. Reverse the string: "TGGCCCAAAA"
2. Complement each nucleotide:
   - T → A
   - G → C  
   - G → C
   - C → G
   - C → G
   - C → G
   - A → T
   - A → T
   - A → T
   - A → T
3. Result: "ACCGGGTTTT"

## Time and Space Complexity

- **Time Complexity:** O(n) where n is the length of the DNA string
- **Space Complexity:** O(n) for storing the complement string

The solution efficiently handles the reverse complement operation in a single pass through the string.

