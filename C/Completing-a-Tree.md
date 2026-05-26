# Rosalind Problem: Completing a Tree

## Problem Description
In this problem, we need to find the number of edges that must be added to a graph to make it a tree. A tree is a connected graph with no cycles and exactly n-1 edges for n nodes.

## Solution Approach
1. Read the number of nodes and edges
2. Count the number of connected components
3. The number of edges needed = (number of components - 1)

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>

// Union-Find data structure for finding connected components
int *parent;
int *rank;

// Find with path compression
int find(int x) {
    if (parent[x] != x) {
        parent[x] = find(parent[x]);
    }
    return parent[x];
}

// Union by rank
void unite(int x, int y) {
    int root_x = find(x);
    int root_y = find(y);
    
    if (root_x != root_y) {
        if (rank[root_x] < rank[root_y]) {
            parent[root_x] = root_y;
        } else if (rank[root_x] > rank[root_y]) {
            parent[root_y] = root_x;
        } else {
            parent[root_y] = root_x;
            rank[root_x]++;
        }
    }
}

int main() {
    int n, m;
    int i, u, v;
    int components = 0;
    
    // Read number of nodes
    scanf("%d", &n);
    
    // Initialize Union-Find structure
    parent = (int*)malloc((n + 1) * sizeof(int));
    rank = (int*)malloc((n + 1) * sizeof(int));
    
    // Initialize each node as its own parent
    for (i = 1; i <= n; i++) {
        parent[i] = i;
        rank[i] = 0;
    }
    
    // Read number of edges
    scanf("%d", &m);
    
    // Process each edge
    for (i = 0; i < m; i++) {
        scanf("%d %d", &u, &v);
        unite(u, v);
    }
    
    // Count connected components
    for (i = 1; i <= n; i++) {
        if (parent[i] == i) {
            components++;
        }
    }
    
    // Number of edges needed = components - 1
    printf("%d\n", components - 1);
    
    free(parent);
    free(rank);
    
    return 0;
}
```

## Explanation

1. **Data Structure**: We use Union-Find (Disjoint Set Union) to efficiently track connected components
2. **Find Operation**: With path compression, finds the root of a component
3. **Union Operation**: Merges two components using union by rank
4. **Counting Components**: After processing all edges, we count how many root nodes exist (each represents a connected component)
5. **Result**: To make a graph into a tree, we need (number of components - 1) additional edges

## Sample Input/Output

**Input:**
```
10
8
1 2
2 3
3 4
4 5
5 6
6 7
7 8
8 9
```

**Output:**
```
1
```

**Explanation**: There are 10 nodes and 8 edges. The graph has 2 connected components, so we need 1 more edge to connect them into a single tree.

## Time Complexity
- O(n + m × α(n)) where α(n) is the inverse Ackermann function (nearly constant)
- Space Complexity: O(n)

