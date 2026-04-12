# Rosalind Problem: Introduction to Set Operations (C Solution)

## Problem Understanding

This problem requires implementing basic set operations (union, intersection, difference, and symmetric difference) on two sets of integers, where the sets are represented as arrays in C.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>

// Function to check if an element exists in a set
int contains(int *set, int size, int element) {
    for (int i = 0; i < size; i++) {
        if (set[i] == element) {
            return 1;
        }
    }
    return 0;
}

// Function to compute union of two sets
void union_sets(int *set1, int size1, int *set2, int size2, int *result, int *result_size) {
    *result_size = 0;
    
    // Add all elements from set1
    for (int i = 0; i < size1; i++) {
        result[*result_size] = set1[i];
        (*result_size)++;
    }
    
    // Add elements from set2 that are not already in result
    for (int i = 0; i < size2; i++) {
        if (!contains(result, *result_size, set2[i])) {
            result[*result_size] = set2[i];
            (*result_size)++;
        }
    }
}

// Function to compute intersection of two sets
void intersection_sets(int *set1, int size1, int *set2, int size2, int *result, int *result_size) {
    *result_size = 0;
    
    // Check each element of set1 to see if it's in set2
    for (int i = 0; i < size1; i++) {
        if (contains(set2, size2, set1[i])) {
            result[*result_size] = set1[i];
            (*result_size)++;
        }
    }
}

// Function to compute difference (set1 - set2)
void difference_sets(int *set1, int size1, int *set2, int size2, int *result, int *result_size) {
    *result_size = 0;
    
    // Check each element of set1 to see if it's NOT in set2
    for (int i = 0; i < size1; i++) {
        if (!contains(set2, size2, set1[i])) {
            result[*result_size] = set1[i];
            (*result_size)++;
        }
    }
}

// Function to compute symmetric difference
void symmetric_difference_sets(int *set1, int size1, int *set2, int size2, int *result, int *result_size) {
    *result_size = 0;
    
    // Add elements from set1 that are not in set2
    for (int i = 0; i < size1; i++) {
        if (!contains(set2, size2, set1[i])) {
            result[*result_size] = set1[i];
            (*result_size)++;
        }
    }
    
    // Add elements from set2 that are not in set1
    for (int i = 0; i < size2; i++) {
        if (!contains(set1, size1, set2[i])) {
            result[*result_size] = set2[i];
            (*result_size)++;
        }
    }
}

// Function to print a set
void print_set(int *set, int size) {
    printf("{");
    for (int i = 0; i < size; i++) {
        printf("%d", set[i]);
        if (i < size - 1) {
            printf(", ");
        }
    }
    printf("}\n");
}

int main() {
    // Read the universal set size
    int n;
    scanf("%d", &n);
    
    // Read first set
    int size1;
    scanf("%d", &size1);
    int *set1 = (int*)malloc(size1 * sizeof(int));
    for (int i = 0; i < size1; i++) {
        scanf("%d", &set1[i]);
    }
    
    // Read second set
    int size2;
    scanf("%d", &size2);
    int *set2 = (int*)malloc(size2 * sizeof(int));
    for (int i = 0; i < size2; i++) {
        scanf("%d", &set2[i]);
    }
    
    // Allocate memory for results
    int *union_result = (int*)malloc((size1 + size2) * sizeof(int));
    int *intersection_result = (int*)malloc((size1 < size2 ? size1 : size2) * sizeof(int));
    int *difference1_result = (int*)malloc(size1 * sizeof(int));
    int *difference2_result = (int*)malloc(size2 * sizeof(int));
    int *symmetric_diff_result = (int*)malloc((size1 + size2) * sizeof(int));
    
    int union_size, intersection_size, diff1_size, diff2_size, sym_diff_size;
    
    // Compute all set operations
    union_sets(set1, size1, set2, size2, union_result, &union_size);
    intersection_sets(set1, size1, set2, size2, intersection_result, &intersection_size);
    difference_sets(set1, size1, set2, size2, difference1_result, &diff1_size);
    difference_sets(set2, size2, set1, size1, difference2_result, &diff2_size);
    symmetric_difference_sets(set1, size1, set2, size2, symmetric_diff_result, &sym_diff_size);
    
    // Print results
    print_set(union_result, union_size);
    print_set(intersection_result, intersection_size);
    print_set(difference1_result, diff1_size);
    print_set(difference2_result, diff2_size);
    print_set(symmetric_diff_result, sym_diff_size);
    
    // Free allocated memory
    free(set1);
    free(set2);
    free(union_result);
    free(intersection_result);
    free(difference1_result);
    free(difference2_result);
    free(symmetric_diff_result);
    
    return 0;
}
```

## Explanation

This solution implements all the basic set operations required for the Rosalind problem:

1. **Union**: Combines all elements from both sets, removing duplicates
2. **Intersection**: Finds elements that exist in both sets
3. **Difference**: Finds elements in the first set that are not in the second set
4. **Symmetric Difference**: Finds elements that are in either set but not in both

## Key Features

- Uses helper functions to check set membership (`contains`)
- Implements each set operation separately
- Proper memory management with malloc/free
- Clean output formatting matching Rosalind's expected format
- Handles edge cases like empty sets

## Time Complexity
- Each operation: O(n × m) where n and m are the sizes of the sets
- Space complexity: O(n + m) for storing results

The solution correctly handles the input format expected by Rosalind and produces the exact output format required.

