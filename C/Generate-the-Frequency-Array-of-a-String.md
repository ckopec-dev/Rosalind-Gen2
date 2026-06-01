# Rosalind Problem: Generate the Frequency Array of a String (C Solution)

## Problem Description
Given a DNA string of length at most 1000 nucleotides, return the frequency array of nucleotides A, C, G, and T.

## Solution

```c
#include <stdio.h>
#include <string.h>

int main() {
    char dna[1001];  // DNA string (max 1000 characters + null terminator)
    int freq[4] = {0};  // Frequency array for A, C, G, T
    
    // Read the DNA string
    scanf("%s", dna);
    
    // Count frequency of each nucleotide
    int length = strlen(dna);
    for (int i = 0; i < length; i++) {
        switch (dna[i]) {
            case 'A':
                freq[0]++;
                break;
            case 'C':
                freq[1]++;
                break;
            case 'G':
                freq[2]++;
                break;
            case 'T':
                freq[3]++;
                break;
        }
    }
    
    // Print the frequency array
    printf("%d %d %d %d\n", freq[0], freq[1], freq[2], freq[3]);
    
    return 0;
}
```

## Explanation

1. **Input Reading**: We read a DNA string using `scanf("%s", dna)` which stores the string in the `dna` array.

2. **Frequency Counting**: We initialize a frequency array `freq` with 4 elements (for A, C, G, T) and iterate through each character of the DNA string.

3. **Character Switch**: For each character in the DNA string, we use a switch statement to increment the appropriate counter:
   - 'A' → increment `freq[0]`
   - 'C' → increment `freq[1]`
   - 'G' → increment `freq[2]`
   - 'T' → increment `freq[3]`

4. **Output**: We print the four frequencies separated by spaces in the order A, C, G, T.

## Example

**Input:**
```
ACGTACGT
```

**Output:**
```
2 2 2 2
```

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the DNA string
- **Space Complexity**: O(1) for the frequency array (fixed size of 4) plus O(n) for the input string

The solution efficiently processes the DNA string in a single pass and outputs the required frequency counts in the correct order.

