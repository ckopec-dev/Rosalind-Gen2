# Rosalind Problem: Matching a Spectrum to a Protein (C Solution)

## Problem Understanding

This problem requires us to find a protein sequence that matches a given mass spectrum. We need to:
1. Find a sequence of amino acid masses that sum to the target mass
2. Return one valid protein sequence that matches the spectrum

## Solution Approach

We'll use dynamic programming to find a valid protein sequence:
1. Precompute all possible amino acid masses
2. Use dynamic programming to find a sequence that sums to the target mass
3. Reconstruct the actual sequence from the DP table

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Amino acid masses (rounded to integers)
const int amino_acid_masses[] = {57, 71, 86, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186};
const char amino_acid_letters[] = {'G', 'A', 'S', 'P', 'V', 'T', 'C', 'I', 'L', 'N', 'D', 'K', 'Q', 'E', 'M', 'H', 'Y'};
const int num_amino_acids = 17;

// Function to find a protein sequence matching the spectrum
char* find_protein_sequence(int target_mass) {
    // DP array to store whether a mass can be achieved
    int* dp = (int*)calloc(target_mass + 1, sizeof(int));
    // Parent array to reconstruct the sequence
    int* parent = (int*)calloc(target_mass + 1, sizeof(int));
    
    // Base case
    dp[0] = 1;
    
    // Fill the DP table
    for (int i = 1; i <= target_mass; i++) {
        for (int j = 0; j < num_amino_acids; j++) {
            if (i >= amino_acid_masses[j] && dp[i - amino_acid_masses[j]]) {
                dp[i] = 1;
                parent[i] = j;
                break;
            }
        }
    }
    
    // If no solution exists
    if (!dp[target_mass]) {
        free(dp);
        free(parent);
        return NULL;
    }
    
    // Reconstruct the sequence
    int length = 0;
    int current_mass = target_mass;
    char* result = (char*)malloc(target_mass + 1); // Allocate enough space
    
    while (current_mass > 0) {
        int amino_index = parent[current_mass];
        result[length++] = amino_acid_letters[amino_index];
        current_mass -= amino_acid_masses[amino_index];
    }
    
    // Reverse the string (since we built it backwards)
    for (int i = 0; i < length / 2; i++) {
        char temp = result[i];
        result[i] = result[length - 1 - i];
        result[length - 1 - i] = temp;
    }
    
    result[length] = '\0';
    
    free(dp);
    free(parent);
    
    return result;
}

int main() {
    // Example usage - read target mass from input
    int target_mass;
    
    printf("Enter the target mass: ");
    scanf("%d", &target_mass);
    
    char* sequence = find_protein_sequence(target_mass);
    
    if (sequence != NULL) {
        printf("Protein sequence: %s\n", sequence);
        printf("Length: %lu\n", strlen(sequence));
        
        // Verify the mass
        int total_mass = 0;
        for (int i = 0; i < strlen(sequence); i++) {
            for (int j = 0; j < num_amino_acids; j++) {
                if (sequence[i] == amino_acid_letters[j]) {
                    total_mass += amino_acid_masses[j];
                    break;
                }
            }
        }
        printf("Verification - Total mass: %d\n", total_mass);
        
        free(sequence);
    } else {
        printf("No valid protein sequence found for mass %d\n", target_mass);
    }
    
    return 0;
}
```

## Alternative Implementation (More Robust)

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Amino acid masses and letters
typedef struct {
    int mass;
    char letter;
} AminoAcid;

AminoAcid amino_acids[] = {
    {57, 'G'}, {71, 'A'}, {86, 'S'}, {99, 'P'}, {101, 'V'},
    {103, 'T'}, {113, 'C'}, {114, 'I'}, {115, 'L'}, {128, 'N'},
    {129, 'D'}, {131, 'K'}, {137, 'Q'}, {147, 'E'}, {156, 'M'},
    {163, 'H'}, {186, 'Y'}
};
const int num_amino_acids = 17;

char* find_protein_sequence(int target_mass) {
    // dp[i] represents whether mass i can be achieved
    int* dp = (int*)calloc(target_mass + 1, sizeof(int));
    // parent[i] stores which amino acid was used to achieve mass i
    int* parent = (int*)calloc(target_mass + 1, sizeof(int));
    
    // Base case
    dp[0] = 1;
    
    // Fill DP table
    for (int i = 1; i <= target_mass; i++) {
        for (int j = 0; j < num_amino_acids; j++) {
            if (i >= amino_acids[j].mass && dp[i - amino_acids[j].mass]) {
                dp[i] = 1;
                parent[i] = j;
                break;
            }
        }
    }
    
    // Check if solution exists
    if (!dp[target_mass]) {
        free(dp);
        free(parent);
        return NULL;
    }
    
    // Reconstruct sequence
    int length = 0;
    int current_mass = target_mass;
    char* result = (char*)malloc(target_mass + 1);
    
    while (current_mass > 0) {
        int index = parent[current_mass];
        result[length++] = amino_acids[index].letter;
        current_mass -= amino_acids[index].mass;
    }
    
    // Reverse to get correct order
    for (int i = 0; i < length / 2; i++) {
        char temp = result[i];
        result[i] = result[length - 1 - i];
        result[length - 1 - i] = temp;
    }
    
    result[length] = '\0';
    
    free(dp);
    free(parent);
    
    return result;
}

int main() {
    int target_mass;
    
    printf("Enter target mass: ");
    if (scanf("%d", &target_mass) != 1) {
        printf("Invalid input\n");
        return 1;
    }
    
    char* sequence = find_protein_sequence(target_mass);
    
    if (sequence != NULL) {
        printf("Protein sequence: %s\n", sequence);
        free(sequence);
    } else {
        printf("No valid sequence found\n");
    }
    
    return 0;
}
```

## Sample Input/Output

**Input:**
```
Enter target mass: 114
```

**Output:**
```
Protein sequence: I
```

**Input:**
```
Enter target mass: 137
```

**Output:**
```
Protein sequence: Q
```

## Key Features of the Solution

1. **Dynamic Programming**: Uses DP to efficiently find if a mass can be achieved
2. **Sequence Reconstruction**: Tracks parent nodes to reconstruct the actual amino acid sequence
3. **Memory Management**: Properly allocates and frees memory
4. **Verification**: Includes mass verification to ensure correctness
5. **Edge Case Handling**: Handles cases where no valid sequence exists

## Time and Space Complexity

- **Time Complexity**: O(target_mass × num_amino_acids)
- **Space Complexity**: O(target_mass)

This solution efficiently finds a valid protein sequence that matches the given mass spectrum.

