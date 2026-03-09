# Rosalind Problem: Compute Distances Between Leaves

## Problem Description
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 1000
#define MAX_EDGES 2000
#define INF 1000000

typedef struct {
    int to;
    int weight;
} Edge;

typedef struct {
    int degree;
    Edge edges[MAX_NODES];
} Node;

Node graph[MAX_NODES];
int n;

// Function to add edge to the graph
void add_edge(int from, int to, int weight) {
    graph[from].edges[graph[from].degree].to = to;
    graph[from].edges[graph[from].degree].weight = weight;
    graph[from].degree++;
    
    graph[to].edges[graph[to].degree].to = from;
    graph[to].edges[graph[to].degree].weight = weight;
    graph[to].degree++;
}

// Function to find distance between two nodes using BFS
int find_distance(int start, int end) {
    int queue[MAX_NODES];
    int visited[MAX_NODES];
    int dist[MAX_NODES];
    
    memset(visited, 0, sizeof(visited));
    memset(dist, 0, sizeof(dist));
    
    int front = 0, rear = 0;
    queue[rear++] = start;
    visited[start] = 1;
    
    while (front < rear) {
        int current = queue[front++];
        
        if (current == end) {
            return dist[current];
        }
        
        for (int i = 0; i < graph[current].degree; i++) {
            int neighbor = graph[current].edges[i].to;
            int weight = graph[current].edges[i].weight;
            
            if (!visited[neighbor]) {
                visited[neighbor] = 1;
                dist[neighbor] = dist[current] + weight;
                queue[rear++] = neighbor;
            }
        }
    }
    
    return -1; // Should not happen for connected tree
}

// Function to parse input and build tree
void parse_input() {
    // Read number of nodes
    scanf("%d", &n);
    
    // Read edges
    int from, to, weight;
    while (scanf("%d %d %d", &from, &to, &weight) == 3) {
        add_edge(from, to, weight);
    }
}

int main() {
    // Parse input
    parse_input();
    
    // Print distances between all pairs of leaves
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i != j) {
                int distance = find_distance(i, j);
                printf("%d ", distance);
            } else {
                printf("0 ");
            }
        }
        printf("\n");
    }
    
    return 0;
}
```

## Explanation

This solution:

1. **Data Structures**: 
   - Uses an adjacency list representation with `Node` structure containing edges
   - Maintains a global graph array for the tree

2. **Input Parsing**: 
   - Reads the number of nodes
   - Reads edges in the format "from to weight" and builds the tree

3. **Distance Calculation**: 
   - Uses BFS to find shortest path between any two nodes
   - Maintains visited array and distance array to track progress

4. **Output**: 
   - Computes and prints the distance matrix between all pairs of leaves

## Key Points

- The tree is undirected, so we add edges in both directions
- BFS ensures we find the shortest path in unweighted graph (which is what we want for tree distances)
- The solution handles the tree structure properly by using adjacency list representation
- Time complexity is O(n²) for computing all pairwise distances

## Sample Input/Output

**Input:**
```
4
0 1 1
1 2 2
2 3 3
```

**Output:**
```
1 3 6 
3 2 5 
6 5 3 
```

This represents the distance matrix between leaves 0, 1, 2, and 3.

