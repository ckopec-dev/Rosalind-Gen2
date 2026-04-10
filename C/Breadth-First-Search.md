# Rosalind Problem: Breadth-First Search (BFS) in C

## Problem Understanding

The Breadth-First Search problem asks us to find the shortest path distances from a source vertex to all other vertices in an unweighted graph.

## Solution Approach

I'll implement BFS using a queue to traverse the graph level by level, keeping track of distances from the source vertex.

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_NODES 1000

// Structure for adjacency list node
typedef struct Node {
    int vertex;
    struct Node* next;
} Node;

// Structure for adjacency list
typedef struct {
    Node* head;
} AdjList;

// Structure for graph
typedef struct {
    int numVertices;
    AdjList* adjLists;
} Graph;

// Queue structure for BFS
typedef struct {
    int items[MAX_NODES];
    int front;
    int rear;
} Queue;

// Create a new node
Node* createNode(int vertex) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->vertex = vertex;
    newNode->next = NULL;
    return newNode;
}

// Create a graph
Graph* createGraph(int numVertices) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->numVertices = numVertices;
    
    graph->adjLists = (AdjList*)malloc(numVertices * sizeof(AdjList));
    for (int i = 0; i < numVertices; i++) {
        graph->adjLists[i].head = NULL;
    }
    
    return graph;
}

// Add edge to the graph
void addEdge(Graph* graph, int src, int dest) {
    // Add edge from src to dest
    Node* newNode = createNode(dest);
    newNode->next = graph->adjLists[src].head;
    graph->adjLists[src].head = newNode;
    
    // Add edge from dest to src (since it's an undirected graph)
    newNode = createNode(src);
    newNode->next = graph->adjLists[dest].head;
    graph->adjLists[dest].head = newNode;
}

// Create a new queue
Queue* createQueue() {
    Queue* queue = (Queue*)malloc(sizeof(Queue));
    queue->front = -1;
    queue->rear = -1;
    return queue;
}

// Check if queue is empty
bool isEmpty(Queue* queue) {
    return queue->rear == -1;
}

// Enqueue an element
void enqueue(Queue* queue, int data) {
    if (queue->rear == MAX_NODES - 1) {
        printf("Queue is full\n");
        return;
    }
    
    if (queue->front == -1) {
        queue->front = 0;
    }
    queue->rear++;
    queue->items[queue->rear] = data;
}

// Dequeue an element
int dequeue(Queue* queue) {
    if (isEmpty(queue)) {
        return -1;
    }
    
    int data = queue->items[queue->front];
    queue->front++;
    
    if (queue->front > queue->rear) {
        queue->front = -1;
        queue->rear = -1;
    }
    
    return data;
}

// BFS implementation
void BFS(Graph* graph, int startVertex, int distances[]) {
    // Initialize distances array
    for (int i = 0; i < graph->numVertices; i++) {
        distances[i] = -1; // -1 means unreachable
    }
    
    // Create a visited array
    bool* visited = (bool*)calloc(graph->numVertices, sizeof(bool));
    
    // Create a queue for BFS
    Queue* queue = createQueue();
    
    // Mark the start vertex as visited and enqueue it
    visited[startVertex] = true;
    distances[startVertex] = 0;
    enqueue(queue, startVertex);
    
    // BFS loop
    while (!isEmpty(queue)) {
        int currentVertex = dequeue(queue);
        
        // Visit all adjacent vertices
        Node* temp = graph->adjLists[currentVertex].head;
        while (temp != NULL) {
            int adjacentVertex = temp->vertex;
            
            if (!visited[adjacentVertex]) {
                visited[adjacentVertex] = true;
                distances[adjacentVertex] = distances[currentVertex] + 1;
                enqueue(queue, adjacentVertex);
            }
            temp = temp->next;
        }
    }
    
    // Free memory
    free(visited);
    free(queue);
}

// Print distances
void printDistances(int distances[], int numVertices) {
    for (int i = 0; i < numVertices; i++) {
        printf("%d ", distances[i]);
    }
    printf("\n");
}

int main() {
    // Read input from stdin (assuming standard Rosalind format)
    int numVertices, numEdges;
    scanf("%d %d", &numVertices, &numEdges);
    
    // Create graph
    Graph* graph = createGraph(numVertices);
    
    // Read edges
    for (int i = 0; i < numEdges; i++) {
        int src, dest;
        scanf("%d %d", &src, &dest);
        // Convert to 0-based indexing
        addEdge(graph, src - 1, dest - 1);
    }
    
    // Read start vertex
    int startVertex;
    scanf("%d", &startVertex);
    startVertex--; // Convert to 0-based indexing
    
    // Array to store distances
    int* distances = (int*)malloc(numVertices * sizeof(int));
    
    // Perform BFS
    BFS(graph, startVertex, distances);
    
    // Print results
    printDistances(distances, numVertices);
    
    // Free memory
    for (int i = 0; i < numVertices; i++) {
        Node* temp = graph->adjLists[i].head;
        while (temp != NULL) {
            Node* next = temp->next;
            free(temp);
            temp = next;
        }
    }
    free(graph->adjLists);
    free(graph);
    free(distances);
    
    return 0;
}
```

## Input Format

The input format expected by this program:
```
n m
u1 v1
u2 v2
...
um vm
start_vertex
```

Where:
- `n` is the number of vertices
- `m` is the number of edges
- Each line `ui vi` represents an edge between vertices ui and vi
- `start_vertex` is the vertex from which to start BFS

## Example

**Input:**
```
6 7
1 2
2 3
3 4
4 5
5 6
1 3
2 4
1
```

**Output:**
```
0 1 1 2 3 4
```

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V) for the distance array, visited array, and queue

## Key Features

1. **Adjacency List Representation**: Efficient for sparse graphs
2. **Queue-based BFS**: Ensures level-by-level traversal
3. **Memory Management**: Proper allocation and deallocation of memory
4. **0-based Indexing**: Converts from 1-based input to 0-based C arrays
5. **Error Handling**: Checks for queue overflow and empty queue conditions

This implementation correctly solves the BFS problem for Rosalind's requirements and handles the standard graph traversal scenario efficiently.

