```c
#include <stdio.h>
#include <stdlib.h>

int* merge_arrays(int* arr1, int len1, int* arr2, int len2, int* result_len) {
    int i = 0, j = 0, k = 0;
    int* result = (int*)malloc((len1 + len2) * sizeof(int));
    
    // Merge the two sorted arrays
    while (i < len1 && j < len2) {
        if (arr1[i] <= arr2[j]) {
            result[k++] = arr1[i++];
        } else {
            result[k++] = arr2[j++];
        }
    }
    
    // Copy remaining elements from arr1 (if any)
    while (i < len1) {
        result[k++] = arr1[i++];
    }
    
    // Copy remaining elements from arr2 (if any)
    while (j < len2) {
        result[k++] = arr2[j++];
    }
    
    *result_len = len1 + len2;
    return result;
}

int main() {
    // Read input
    int len1, len2;
    scanf("%d", &len1);
    
    int* arr1 = (int*)malloc(len1 * sizeof(int));
    for (int i = 0; i < len1; i++) {
        scanf("%d", &arr1[i]);
    }
    
    scanf("%d", &len2);
    
    int* arr2 = (int*)malloc(len2 * sizeof(int));
    for (int i = 0; i < len2; i++) {
        scanf("%d", &arr2[i]);
    }
    
    // Merge arrays
    int result_len;
    int* result = merge_arrays(arr1, len1, arr2, len2, &result_len);
    
    // Print result
    for (int i = 0; i < result_len; i++) {
        printf("%d ", result[i]);
    }
    printf("\n");
    
    // Free memory
    free(arr1);
    free(arr2);
    free(result);
    
    return 0;
}
```

This C solution implements the merge operation for two sorted arrays as required by the Rosalind problem. Here's how it works:

1. **Input Reading**: The program reads the lengths of both arrays followed by the array elements themselves.

2. **Merge Function**: The `merge_arrays` function takes two sorted arrays and merges them into a single sorted array:
   - Uses three pointers (i, j, k) to traverse both input arrays and build the result
   - Compares elements from both arrays and places the smaller one in the result
   - Handles remaining elements from either array after one is exhausted

3. **Memory Management**: 
   - Dynamically allocates memory for input arrays and result array
   - Properly frees all allocated memory before program termination

4. **Output**: Prints the merged sorted array elements separated by spaces.

The time complexity is O(n + m) where n and m are the lengths of the input arrays, and the space complexity is O(n + m) for the result array.

