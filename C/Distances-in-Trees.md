# Rosalind Problem: Distances in Trees (DIST)

## Problem Description
Given a collection of trees, compute the distance matrix for each tree. The distance between two nodes is the number of edges in the unique path connecting them.

## Solution Approach
1. Parse input to build tree structures
2. For each tree, compute all pairwise distances
3. Output distance matrices

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_NODES 1000
#define MAX_EDGES 2000
#define MAX_LINE 1000

// Tree structure
typedef struct {
    int nodes;
    int edges;
    int adj_matrix[MAX_NODES][MAX_NODES];
    int visited[MAX_NODES];
} Tree;

// Function to initialize tree
void init_tree(Tree* tree) {
    tree->nodes = 0;
    tree->edges = 0;
    for (int i = 0; i < MAX_NODES; i++) {
        for (int j = 0; j < MAX_NODES; j++) {
            tree->adj_matrix[i][j] = 0;
        }
        tree->visited[i] = 0;
    }
}

// Function to add edge to tree
void add_edge(Tree* tree, int u, int v) {
    tree->adj_matrix[u][v] = 1;
    tree->adj_matrix[v][u] = 1;
    tree->edges++;
}

// Function to perform BFS and find distance
int bfs_distance(Tree* tree, int start, int end) {
    // Reset visited array
    for (int i = 0; i < MAX_NODES; i++) {
        tree->visited[i] = 0;
    }
    
    // BFS queue
    int queue[MAX_NODES];
    int front = 0, rear = 0;
    queue[rear++] = start;
    tree->visited[start] = 1;
    
    int distance = 0;
    
    while (front < rear) {
        int size = rear - front;
        for (int i = 0; i < size; i++) {
            int current = queue[front++];
            
            if (current == end) {
                return distance;
            }
            
            // Visit all neighbors
            for (int j = 0; j < tree->nodes; j++) {
                if (tree->adj_matrix[current][j] && !tree->visited[j]) {
                    tree->visited[j] = 1;
                    queue[rear++] = j;
                }
            }
        }
        distance++;
    }
    
    return -1; // No path found
}

// Function to read input and build tree
int read_tree(Tree* tree, FILE* file) {
    init_tree(tree);
    
    char line[MAX_LINE];
    int node_count = 0;
    
    // Read number of nodes
    if (fgets(line, MAX_LINE, file) == NULL) {
        return 0; // No more trees
    }
    
    // Parse number of nodes
    tree->nodes = atoi(line);
    if (tree->nodes == 0) {
        return 0;
    }
    
    // Read edges
    while (fgets(line, MAX_LINE, file)) {
        if (line[0] == '\n' || line[0] == '\r') {
            break; // Empty line separates trees
        }
        
        int u, v;
        if (sscanf(line, "%d %d", &u, &v) == 2) {
            add_edge(tree, u - 1, v - 1); // Convert to 0-indexed
        }
    }
    
    return 1;
}

// Function to compute and print distance matrix
void compute_distance_matrix(Tree* tree) {
    int matrix[MAX_NODES][MAX_NODES];
    
    // Compute all pairwise distances
    for (int i = 0; i < tree->nodes; i++) {
        for (int j = 0; j < tree->nodes; j++) {
            if (i == j) {
                matrix[i][j] = 0;
            } else {
                matrix[i][j] = bfs_distance(tree, i, j);
            }
        }
    }
    
    // Print matrix
    for (int i = 0; i < tree->nodes; i++) {
        for (int j = 0; j < tree->nodes; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
}

int main() {
    Tree tree;
    FILE* file = stdin;
    
    // Process all trees in input
    while (read_tree(&tree, file)) {
        compute_distance_matrix(&tree);
        printf("\n"); // Separate trees with blank line
    }
    
    return 0;
}
```

## Input Format
The input consists of multiple trees, each separated by an empty line. Each tree starts with the number of nodes, followed by the edges (node1 node2) until an empty line.

## Example Input
```
4
1 2
2 3
3 4

5
1 2
2 3
3 4
4 5
```

## Example Output
```
0 1 2 3 
1 0 1 2 
2 1 0 1 
3 2 1 0 

0 1 2 3 4 
1 0 1 2 3 
2 1 0 1 2 
3 2 1 0 1 
4 3 2 1 0 
```

## Key Features
1. **BFS-based distance calculation**: Uses breadth-first search to find shortest paths
2. **Tree parsing**: Reads input in the specified format
3. **Memory efficient**: Uses adjacency matrix representation
4. **Multiple tree support**: Processes multiple trees from input

## Time Complexity
- Building tree: O(E) where E is number of edges
- Computing distances: O(N × (N + E)) where N is number of nodes
- Overall: O(T × N × (N + E)) where T is number of trees

## Space Complexity
O(N²) for adjacency matrix storage

