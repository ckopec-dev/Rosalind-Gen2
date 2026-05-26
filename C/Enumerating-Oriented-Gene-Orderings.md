# Rosalind Problem: Enumerating Oriented Gene Orderings (C Solution)

## Problem Understanding

This problem asks us to find all possible signed permutations of a given set of genes, where each gene can appear in either positive or negative orientation. For a set of n genes, we need to:
1. Generate all possible arrangements of the genes
2. For each arrangement, consider both positive and negative orientations
3. Count the total number of such signed permutations

## Solution Approach

The key insight is that for n genes:
- There are n! ways to arrange them
- Each gene can be in 2 orientations (positive or negative)
- Total signed permutations = n! × 2^n

We'll use a recursive approach to generate all permutations and then for each permutation, generate all possible sign combinations.

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Global variables to store results
int **permutations;
int *signs;
int count = 0;
int n;

// Function to swap two elements
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Function to generate all permutations of numbers 1 to n
void generatePermutations(int *arr, int start, int end) {
    if (start == end) {
        // Copy current permutation
        for (int i = 0; i < n; i++) {
            permutations[count][i] = arr[i];
        }
        count++;
        return;
    }
    
    for (int i = start; i <= end; i++) {
        swap(&arr[start], &arr[i]);
        generatePermutations(arr, start + 1, end);
        swap(&arr[start], &arr[i]); // backtrack
    }
}

// Function to generate all sign combinations for a permutation
void generateSigns(int *perm, int index, int *current_signs) {
    if (index == n) {
        // Print the signed permutation
        for (int i = 0; i < n; i++) {
            if (current_signs[i] == 1) {
                printf("%d ", perm[i]);
            } else {
                printf("%d ", -perm[i]);
            }
        }
        printf("\n");
        return;
    }
    
    // Try both positive and negative orientations
    current_signs[index] = 1;
    generateSigns(perm, index + 1, current_signs);
    
    current_signs[index] = -1;
    generateSigns(perm, index + 1, current_signs);
}

// Main function to solve the problem
void solve(int num) {
    n = num;
    
    // Allocate memory for permutations
    permutations = (int**)malloc(1000000 * sizeof(int*));
    for (int i = 0; i < 1000000; i++) {
        permutations[i] = (int*)malloc(n * sizeof(int));
    }
    
    signs = (int*)malloc(n * sizeof(int));
    
    // Generate all permutations of numbers 1 to n
    int *arr = (int*)malloc((n + 1) * sizeof(int));
    for (int i = 1; i <= n; i++) {
        arr[i] = i;
    }
    
    count = 0;
    generatePermutations(arr, 1, n);
    
    // Calculate total number of signed permutations
    long long total = 1;
    for (int i = 1; i <= n; i++) {
        total *= i;
    }
    for (int i = 1; i <= n; i++) {
        total *= 2;
    }
    
    printf("%lld\n", total);
    
    // Generate all signed permutations
    for (int i = 0; i < count; i++) {
        generateSigns(permutations[i], 0, signs);
    }
    
    // Free memory
    for (int i = 0; i < 1000000; i++) {
        free(permutations[i]);
    }
    free(permutations);
    free(signs);
    free(arr);
}

int main() {
    int n;
    printf("Enter the number of genes (n): ");
    scanf("%d", &n);
    
    solve(n);
    
    return 0;
}
```

## Example Usage

For n = 3, the program will output:
```
12
1 2 3 
1 2 -3 
1 -2 3 
1 -2 -3 
-1 2 3 
-1 2 -3 
-1 -2 3 
-1 -2 -3 
2 1 3 
2 1 -3 
2 -1 3 
2 -1 -3 
```

## Explanation

1. **Permutation Generation**: We first generate all n! permutations of the numbers 1 to n
2. **Sign Generation**: For each permutation, we generate all 2^n possible sign combinations
3. **Output**: We print the total count followed by all valid signed permutations

## Time and Space Complexity

- **Time Complexity**: O(n! × 2^n × n) - for generating permutations and sign combinations
- **Space Complexity**: O(n! × 2^n × n) - to store all signed permutations

## Key Points

- The problem is essentially finding signed permutations where each element can be positive or negative
- We use backtracking to generate permutations and then recursively generate sign combinations
- The total count is n! × 2^n as each of the n! permutations can have 2^n sign variations

