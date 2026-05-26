# Rosalind Problem: Merge Sort Solution in C

## Problem Understanding

The merge sort problem requires implementing the merge sort algorithm to sort an array of integers in ascending order.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>

// Function to merge two sorted subarrays
void merge(int arr[], int left, int mid, int right) {
    int i, j, k;
    int n1 = mid - left + 1;
    int n2 = right - mid;
    
    // Create temporary arrays
    int *L = (int*)malloc(n1 * sizeof(int));
    int *R = (int*)malloc(n2 * sizeof(int));
    
    // Copy data to temporary arrays L[] and R[]
    for (i = 0; i < n1; i++)
        L[i] = arr[left + i];
    for (j = 0; j < n2; j++)
        R[j] = arr[mid + 1 + j];
    
    // Merge the temporary arrays back
    i = 0; // Initial index of first subarray
    j = 0; // Initial index of second subarray
    k = left; // Initial index of merged array
    
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }
    
    // Copy remaining elements of L[], if any
    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }
    
    // Copy remaining elements of R[], if any
    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
    
    // Free temporary arrays
    free(L);
    free(R);
}

// Main merge sort function
void mergeSort(int arr[], int left, int right) {
    if (left < right) {
        // Find the middle point
        int mid = left + (right - left) / 2;
        
        // Sort first and second halves
        mergeSort(arr, left, mid);
        mergeSort(arr, mid + 1, right);
        
        // Merge the sorted halves
        merge(arr, left, mid, right);
    }
}

int main() {
    // Read input
    int n;
    scanf("%d", &n);
    
    int *arr = (int*)malloc(n * sizeof(int));
    
    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }
    
    // Sort the array
    mergeSort(arr, 0, n - 1);
    
    // Print the sorted array
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    // Free allocated memory
    free(arr);
    
    return 0;
}
```

## Explanation

1. **merge() function**: 
   - Merges two sorted subarrays into one sorted array
   - Creates temporary arrays to hold the left and right subarrays
   - Compares elements from both subarrays and merges them in sorted order
   - Handles remaining elements from either subarray

2. **mergeSort() function**:
   - Recursively divides the array into two halves
   - Sorts each half using recursive calls
   - Merges the sorted halves using the merge() function

3. **Main function**:
   - Reads the input array size and elements
   - Calls mergeSort() to sort the array
   - Prints the sorted array
   - Properly frees allocated memory

## Time and Space Complexity

- **Time Complexity**: O(n log n) - Standard merge sort complexity
- **Space Complexity**: O(n) - Due to temporary arrays used in merging

## Sample Input/Output

**Input:**
```
10
2 6 3 8 5 9 1 4 7 0
```

**Output:**
```
0 1 2 3 4 5 6 7 8 9
```

