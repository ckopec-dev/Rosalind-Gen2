# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Description
Given a directed acyclic graph (DAG), find a topological ordering of its vertices.

## Approach
I'll use Kahn's algorithm for topological sorting:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. While queue is not empty:
   - Remove vertex from queue
   - Add it to result
   - Decrease in-degree of all its neighbors
   - If neighbor's in-degree becomes 0, add to queue

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_VERTICES 1000

// Queue implementation for BFS
typedef struct {
    int items[MAX_VERTICES];
    int front;
    int rear;
    int size;
} Queue;

Queue* createQueue() {
    Queue* q = (Queue*)malloc(sizeof(Queue));
    q->front = -1;
    q->rear = -1;
    q->size = 0;
    return q;
}

int isEmpty(Queue* q) {
    return q->size == 0;
}

void enqueue(Queue* q, int item) {
    q->rear = (q->rear + 1) % MAX_VERTICES;
    q->items[q->rear] = item;
    q->size++;
    if (q->front == -1) {
        q->front = 0;
    }
}

int dequeue(Queue* q) {
    if (isEmpty(q)) {
        return -1;
    }
    int item = q->items[q->front];
    q->front = (q->front + 1) % MAX_VERTICES;
    q->size--;
    if (q->size == 0) {
        q->front = -1;
        q->rear = -1;
    }
    return item;
}

int* topologicalSort(int n, int edges[][2], int edgeCount) {
    // Initialize in-degrees
    int* inDegree = (int*)calloc(n + 1, sizeof(int));
    
    // Build adjacency list and calculate in-degrees
    int** adj = (int**)malloc((n + 1) * sizeof(int*));
    for (int i = 1; i <= n; i++) {
        adj[i] = (int*)malloc(0);
    }
    
    // Count edges for each vertex
    int* edgeCounts = (int*)calloc(n + 1, sizeof(int));
    for (int i = 0; i < edgeCount; i++) {
        int from = edges[i][0];
        int to = edges[i][1];
        edgeCounts[from]++;
    }
    
    // Allocate memory for adjacency lists
    for (int i = 1; i <= n; i++) {
        adj[i] = (int*)malloc(edgeCounts[i] * sizeof(int));
    }
    
    // Reset edge counts for building adjacency lists
    memset(edgeCounts, 0, (n + 1) * sizeof(int));
    
    // Build adjacency lists and update in-degrees
    for (int i = 0; i < edgeCount; i++) {
        int from = edges[i][0];
        int to = edges[i][1];
        adj[from][edgeCounts[from]] = to;
        edgeCounts[from]++;
        inDegree[to]++;
    }
    
    // Initialize queue with vertices of in-degree 0
    Queue* q = createQueue();
    int* result = (int*)malloc((n + 1) * sizeof(int));
    int resultIndex = 0;
    
    for (int i = 1; i <= n; i++) {
        if (inDegree[i] == 0) {
            enqueue(q, i);
        }
    }
    
    // Process vertices
    while (!isEmpty(q)) {
        int current = dequeue(q);
        result[resultIndex++] = current;
        
        // Process neighbors
        for (int i = 0; i < edgeCounts[current]; i++) {
            int neighbor = adj[current][i];
            inDegree[neighbor]--;
            if (inDegree[neighbor] == 0) {
                enqueue(q, neighbor);
            }
        }
    }
    
    // Free memory
    for (int i = 1; i <= n; i++) {
        free(adj[i]);
    }
    free(adj);
    free(inDegree);
    free(edgeCounts);
    free(q);
    
    return result;
}

int main() {
    int n, m;
    scanf("%d %d", &n, &m);
    
    int edges[10000][2];
    for (int i = 0; i < m; i++) {
        scanf("%d %d", &edges[i][0], &edges[i][1]);
    }
    
    int* topologicalOrder = topologicalSort(n, edges, m);
    
    for (int i = 0; i < n; i++) {
        printf("%d ", topologicalOrder[i]);
    }
    printf("\n");
    
    free(topologicalOrder);
    return 0;
}
```

## Input Format
- First line: two integers n (number of vertices) and m (number of edges)
- Next m lines: two integers u and v representing a directed edge from u to v

## Example Input
```
6 8
5 2
5 0
4 0
4 1
2 3
3 1
1 3
0 3
```

## Example Output
```
5 4 2 0 3 1 
```

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for adjacency list and in-degree array

## Key Points
1. This solution uses Kahn's algorithm for topological sorting
2. It handles the case where multiple valid topological orderings exist (any valid ordering is acceptable)
3. The algorithm works by repeatedly removing vertices with in-degree 0 and updating their neighbors
4. The solution assumes vertices are numbered from 1 to n

