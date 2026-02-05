# Rosalind Problem: Implement_CycleToChromosome

## Problem Description
Given a sequence of integers representing a cycle in the genome graph, we need to convert it to a chromosome representation.

In the genome graph, a cycle is represented as a sequence of integers where consecutive integers form edges. The cycleToChromosome function converts this cycle into a chromosome by:
1. Taking consecutive pairs of integers from the cycle
2. Converting each pair into a chromosome element by taking the minimum of the two integers and adding 1
3. For each pair (a,b), if a < b, then the chromosome element is a, otherwise it's b

Wait, let me re-read the problem carefully. Actually, this is about converting a cycle in the genome graph to a chromosome. The correct approach is:

## Solution

```c
#include <stdio.h>
#include <stdlib.h>

// Function to convert cycle to chromosome
int* cycleToChromosome(int* cycle, int n) {
    int* chromosome = (int*)malloc(n * sizeof(int));
    
    // Process pairs of consecutive elements in the cycle
    for (int i = 0; i < n; i++) {
        if (cycle[2 * i] < cycle[2 * i + 1]) {
            chromosome[i] = cycle[2 * i + 1] / 2;
        } else {
            chromosome[i] = cycle[2 * i] / 2;
        }
    }
    
    return chromosome;
}

int main() {
    // Example input: a cycle represented as a sequence of integers
    int cycle[] = {1, 2, 4, 3, 6, 5, 8, 7};
    int n = 4; // Number of elements in chromosome (half the cycle length)
    
    int* result = cycleToChromosome(cycle, n);
    
    // Print the result
    printf("(");
    for (int i = 0; i < n; i++) {
        if (i > 0) printf(" ");
        printf("%d", result[i]);
    }
    printf(")\n");
    
    free(result);
    return 0;
}
```

## Explanation

The cycleToChromosome function works as follows:

1. **Input**: A cycle represented as a sequence of integers of length 2n
2. **Process**: 
   - Take consecutive pairs from the cycle: (cycle[0], cycle[1]), (cycle[2], cycle[3]), etc.
   - For each pair (a,b), find the minimum value and divide by 2
   - This gives us the chromosome representation
3. **Output**: An array representing the chromosome

## Example Walkthrough

For input cycle = [1, 2, 4, 3, 6, 5, 8, 7]:

- Pair (1, 2): min(1,2) = 1, 1/2 = 0.5 → 1 (since we're dealing with integers, this should be 1)
- Pair (4, 3): min(4,3) = 3, 3/2 = 1.5 → 2 (but actually, we should take the minimum and divide by 2)
- Wait, let me reconsider the correct approach...

Actually, looking at the standard genome graph representation:
- If we have a cycle with elements [1, 2, 4, 3, 6, 5, 8, 7]
- The correct conversion is: for each pair (a,b), we take the larger one divided by 2

```c
#include <stdio.h>
#include <stdlib.h>

// Function to convert cycle to chromosome
int* cycleToChromosome(int* cycle, int n) {
    int* chromosome = (int*)malloc(n * sizeof(int));
    
    // Process pairs of consecutive elements in the cycle
    for (int i = 0; i < n; i++) {
        if (cycle[2 * i] > cycle[2 * i + 1]) {
            chromosome[i] = cycle[2 * i] / 2;
        } else {
            chromosome[i] = cycle[2 * i + 1] / 2;
        }
    }
    
    return chromosome;
}

int main() {
    // Example input: a cycle represented as a sequence of integers
    int cycle[] = {1, 2, 4, 3, 6, 5, 8, 7};
    int n = 4; // Number of elements in chromosome (half the cycle length)
    
    int* result = cycleToChromosome(cycle, n);
    
    // Print the result
    printf("(");
    for (int i = 0; i < n; i++) {
        if (i > 0) printf(" ");
        printf("%d", result[i]);
    }
    printf(")\n");
    
    free(result);
    return 0;
}
```

## Alternative Implementation (More Clear)

```c
#include <stdio.h>
#include <stdlib.h>

int* cycleToChromosome(int* cycle, int n) {
    int* chromosome = (int*)malloc(n * sizeof(int));
    
    // Process pairs of consecutive elements in the cycle
    for (int i = 0; i < n; i++) {
        int first = cycle[2 * i];
        int second = cycle[2 * i + 1];
        
        // The chromosome element is the larger of the two divided by 2
        if (first > second) {
            chromosome[i] = first / 2;
        } else {
            chromosome[i] = second / 2;
        }
    }
    
    return chromosome;
}

int main() {
    // Example: cycle [1, 2, 4, 3, 6, 5, 8, 7]
    // Expected chromosome: (1 2 3 4)
    int cycle[] = {1, 2, 4, 3, 6, 5, 8, 7};
    int n = 4;
    
    int* result = cycleToChromosome(cycle, n);
    
    printf("(");
    for (int i = 0; i < n; i++) {
        printf("%d", result[i]);
        if (i < n - 1) printf(" ");
    }
    printf(")\n");
    
    free(result);
    return 0;
}
```

