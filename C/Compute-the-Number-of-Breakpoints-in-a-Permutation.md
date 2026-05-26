# Rosalind Problem: Compute the Number of Breakpoints in a Permutation

## Problem Understanding

A breakpoint in a permutation is a position where two adjacent elements are not consecutive in value. For example, in the permutation [1, 3, 2, 4], there are breakpoints at positions 1 and 3 because:
- Position 1: 3 and 2 are not consecutive (3-2=1, but they should be adjacent in the sequence)
- Position 3: 4 and 1 are not consecutive

Actually, let me re-read this more carefully. A breakpoint occurs when two adjacent elements in a permutation are not consecutive in value.

## Solution Approach

1. Read the permutation from input
2. Count positions where adjacent elements are not consecutive
3. Also check the boundary conditions (first and last elements)

Wait, let me reconsider the definition. In genome rearrangement theory, a breakpoint is a position where two adjacent elements in a permutation are not consecutive in the sequence 1,2,3,...,n.

For example, in permutation [3, 4, 5, 1, 2]:
- Between 3 and 4: consecutive ✓
- Between 4 and 5: consecutive ✓  
- Between 5 and 1: not consecutive ✗ (breakpoint)
- Between 1 and 2: consecutive ✓

So we count positions where adjacent elements are not consecutive.

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    char line[1000];
    int permutation[1000];
    int n = 0;
    
    // Read the permutation from stdin
    if (fgets(line, sizeof(line), stdin) != NULL) {
        char *token = strtok(line, " \t\n");
        while (token != NULL) {
            permutation[n++] = atoi(token);
            token = strtok(NULL, " \t\n");
        }
    }
    
    // Count breakpoints
    int breakpoints = 0;
    
    // Check first element (should be 1 for a proper permutation)
    if (permutation[0] != 1) {
        breakpoints++;
    }
    
    // Check middle elements
    for (int i = 0; i < n - 1; i++) {
        if (permutation[i+1] - permutation[i] != 1) {
            breakpoints++;
        }
    }
    
    // Check last element (should be n for a proper permutation)
    if (permutation[n-1] != n) {
        breakpoints++;
    }
    
    printf("%d\n", breakpoints);
    
    return 0;
}
```

Wait, let me re-read the problem. Looking at the standard definition of breakpoints in genome rearrangement:

A breakpoint in a permutation π is a position i such that π(i+1) - π(i) ≠ 1.

But also we need to consider that we're looking at the "breakpoints" in the context of the identity permutation 1,2,3,...,n.

Actually, let me think of this more systematically:

If we have a permutation π = [π₁, π₂, ..., πₙ], a breakpoint occurs at position i (1 ≤ i < n) if π(i+1) - π(i) ≠ 1.

But also, we consider the boundary positions:
- Position 0: 1 - π₁ (if π₁ ≠ 1)
- Position n: πₙ - n (if πₙ ≠ n)

Let me revise:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    int permutation[1000];
    int n = 0;
    
    // Read permutation from stdin
    int num;
    while (scanf("%d", &num) == 1) {
        permutation[n++] = num;
    }
    
    // Count breakpoints
    int breakpoints = 0;
    
    // Check if first element is 1
    if (permutation[0] != 1) {
        breakpoints++;
    }
    
    // Check adjacent pairs
    for (int i = 0; i < n - 1; i++) {
        if (permutation[i+1] - permutation[i] != 1) {
            breakpoints++;
        }
    }
    
    // Check if last element is n
    if (permutation[n-1] != n) {
        breakpoints++;
    }
    
    printf("%d\n", breakpoints);
    
    return 0;
}
```

Actually, let me check a simpler approach that matches standard genome rearrangement definitions:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    int permutation[1000];
    int n = 0;
    
    // Read permutation from stdin
    int num;
    while (scanf("%d", &num) == 1) {
        permutation[n++] = num;
    }
    
    // Count breakpoints
    int breakpoints = 0;
    
    // Check each adjacent pair
    for (int i = 0; i < n - 1; i++) {
        if (permutation[i+1] - permutation[i] != 1) {
            breakpoints++;
        }
    }
    
    // Check first element (should be 1)
    if (permutation[0] != 1) {
        breakpoints++;
    }
    
    // Check last element (should be n)
    if (permutation[n-1] != n) {
        breakpoints++;
    }
    
    printf("%d\n", breakpoints);
    
    return 0;
}
```

