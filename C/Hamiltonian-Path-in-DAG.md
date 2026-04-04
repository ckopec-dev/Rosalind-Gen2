# Rosalind Problem: Hamiltonian Path in DAG

## Problem Understanding

A Hamiltonian path in a directed graph is a path that visits each vertex exactly once. For a Directed Acyclic Graph (DAG), we need to find if there exists a path that visits all vertices exactly once.

## Approach

1. **Topological Sort**: First, we'll perform a topological sort of the DAG
2. **Hamiltonian Path Check**: We'll check if there's a path that visits all vertices in topological order
3. **Dynamic Programming**: Use DP to track if we can reach each vertex in the topological order

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_VERTICES 1000
#define MAX_EDGES 10000

// Graph representation
int adj[MAX_VERTICES][MAX_VERTICES];
int in_degree[MAX_VERTICES];
int topo_order[MAX_VERTICES];
int visited[MAX_VERTICES];
int n, m;

// Function to perform topological sort
void topological_sort() {
    int queue[MAX_VERTICES];
    int front = 0, rear = 0;
    
    // Initialize queue with vertices having in-degree 0
    for (int i = 0; i < n; i++) {
        if (in_degree[i] == 0) {
            queue[rear++] = i;
        }
    }
    
    int index = 0;
    while (front < rear) {
        int u = queue[front++];
        topo_order[index++] = u;
        
        // Reduce in-degree of adjacent vertices
        for (int v = 0; v < n; v++) {
            if (adj[u][v]) {
                in_degree[v]--;
                if (in_degree[v] == 0) {
                    queue[rear++] = v;
                }
            }
        }
    }
}

// Function to check if there's a Hamiltonian path
int has_hamiltonian_path() {
    // Check if topological order has all vertices
    if (topo_order[n-1] != n-1) {
        return 0;
    }
    
    // DP approach to check if we can form a Hamiltonian path
    int dp[MAX_VERTICES];
    memset(dp, 0, sizeof(dp));
    
    // Initialize first vertex
    dp[topo_order[0]] = 1;
    
    // For each vertex in topological order
    for (int i = 0; i < n; i++) {
        int u = topo_order[i];
        if (dp[u] == 0) continue;
        
        // Check all neighbors
        for (int v = 0; v < n; v++) {
            if (adj[u][v]) {
                dp[v] = 1;
            }
        }
    }
    
    // Check if all vertices are reachable
    for (int i = 0; i < n; i++) {
        if (dp[i] == 0) {
            return 0;
        }
    }
    
    return 1;
}

// Better approach - check if there's exactly one path through all vertices
int has_hamiltonian_path_better() {
    // Create adjacency list representation
    int adj_list[MAX_VERTICES][MAX_VERTICES];
    int adj_count[MAX_VERTICES];
    memset(adj_count, 0, sizeof(adj_count));
    
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (adj[i][j]) {
                adj_list[i][adj_count[i]++] = j;
            }
        }
    }
    
    // Check if there's exactly one path through all vertices in topological order
    int visited[MAX_VERTICES];
    memset(visited, 0, sizeof(visited));
    
    // Try to construct a path
    int path[MAX_VERTICES];
    int path_len = 0;
    
    // Start with first vertex in topological order
    int current = topo_order[0];
    path[path_len++] = current;
    visited[current] = 1;
    
    // Continue the path
    for (int i = 1; i < n; i++) {
        int next = topo_order[i];
        int found = 0;
        
        // Check if there's an edge from current to next
        for (int j = 0; j < adj_count[current]; j++) {
            if (adj_list[current][j] == next) {
                found = 1;
                break;
            }
        }
        
        if (!found) {
            return 0;
        }
        
        current = next;
        path[path_len++] = current;
        visited[current] = 1;
    }
    
    // Verify all vertices are visited exactly once
    for (int i = 0; i < n; i++) {
        if (visited[i] == 0) {
            return 0;
        }
    }
    
    return 1;
}

int main() {
    // Read input
    scanf("%d %d", &n, &m);
    
    // Initialize adjacency matrix and in-degree array
    memset(adj, 0, sizeof(adj));
    memset(in_degree, 0, sizeof(in_degree));
    
    // Read edges
    for (int i = 0; i < m; i++) {
        int u, v;
        scanf("%d %d", &u, &v);
        u--; v--; // Convert to 0-indexed
        adj[u][v] = 1;
        in_degree[v]++;
    }
    
    // Perform topological sort
    topological_sort();
    
    // Check for Hamiltonian path
    int result = has_hamiltonian_path_better();
    
    if (result) {
        printf("YES\n");
        // Print the path
        for (int i = 0; i < n; i++) {
            printf("%d ", topo_order[i] + 1); // Convert back to 1-indexed
        }
        printf("\n");
    } else {
        printf("NO\n");
    }
    
    return 0;
}
```

## Explanation

The solution works as follows:

1. **Input Processing**: Read the number of vertices `n` and edges `m`, then read all edges and build the adjacency matrix.

2. **Topological Sort**: Use Kahn's algorithm to perform topological sorting of the DAG. This gives us a linear ordering of vertices such that for every directed edge u→v, vertex u comes before vertex v in the ordering.

3. **Hamiltonian Path Check**: 
   - A Hamiltonian path in a DAG visits each vertex exactly once
   - In a topological ordering, if there's a Hamiltonian path, then each vertex should be reachable from the previous one in the sequence
   - We check if there's a direct edge from each vertex to the next one in the topological order

4. **Output**: If a Hamiltonian path exists, output "YES" followed by the path. Otherwise, output "NO".

## Time Complexity
- Topological sort: O(V + E)
- Hamiltonian path check: O(V + E)
- Overall: O(V + E)

## Space Complexity
- O(V²) for adjacency matrix
- O(V) for auxiliary arrays

This approach efficiently solves the Hamiltonian path problem in DAGs by leveraging the topological ordering property.

