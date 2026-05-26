# Rosalind Problem: Implement Tree Coloring (C Solution)

## Problem Understanding

Tree coloring involves coloring the edges of a tree such that no two adjacent edges share the same color. This is essentially edge coloring of a tree, where we need to find the minimum number of colors required.

## Approach

For a tree, we can use a greedy algorithm:
1. Perform a depth-first search (DFS) traversal
2. For each edge, assign the smallest available color that doesn't conflict with already colored adjacent edges
3. Since trees are acyclic, we can always find a valid coloring

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 1000
#define MAX_EDGES 2000

// Structure to represent an edge
typedef struct Edge {
    int u, v;
    int color;
} Edge;

// Structure to represent adjacency list
typedef struct Node {
    int vertex;
    struct Node* next;
} Node;

// Structure to represent graph
typedef struct Graph {
    int num_vertices;
    int num_edges;
    Node* adj_list[MAX_NODES];
    Edge edges[MAX_EDGES];
} Graph;

// Function to create a new node
Node* create_node(int vertex) {
    Node* new_node = (Node*)malloc(sizeof(Node));
    new_node->vertex = vertex;
    new_node->next = NULL;
    return new_node;
}

// Function to add edge to adjacency list
void add_edge(Graph* graph, int u, int v) {
    // Add edge u-v
    Node* new_node = create_node(v);
    new_node->next = graph->adj_list[u];
    graph->adj_list[u] = new_node;
    
    // Add edge v-u (since it's an undirected graph)
    new_node = create_node(u);
    new_node->next = graph->adj_list[v];
    graph->adj_list[v] = new_node;
}

// Function to initialize graph
Graph* create_graph(int vertices, int edges) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->num_vertices = vertices;
    graph->num_edges = edges;
    
    for (int i = 0; i < vertices; i++) {
        graph->adj_list[i] = NULL;
    }
    
    return graph;
}

// Function to get maximum degree of a vertex
int max_degree(Graph* graph) {
    int max_deg = 0;
    for (int i = 0; i < graph->num_vertices; i++) {
        int deg = 0;
        Node* temp = graph->adj_list[i];
        while (temp != NULL) {
            deg++;
            temp = temp->next;
        }
        if (deg > max_deg) max_deg = deg;
    }
    return max_deg;
}

// Function to color edges using greedy algorithm
void tree_coloring(Graph* graph) {
    // Initialize all edges with color -1 (unassigned)
    for (int i = 0; i < graph->num_edges; i++) {
        graph->edges[i].color = -1;
    }
    
    // For a tree, we can color edges greedily
    // We'll use a simpler approach: assign colors based on the order of edges
    int max_color = 0;
    
    // Create an array to track which colors are used for each vertex
    int* used_colors = (int*)calloc(graph->num_vertices, sizeof(int));
    
    // Color each edge
    for (int i = 0; i < graph->num_edges; i++) {
        int u = graph->edges[i].u;
        int v = graph->edges[i].v;
        
        // Find first available color
        int color = 1;
        while (used_colors[u] & (1 << color) || used_colors[v] & (1 << color)) {
            color++;
        }
        
        graph->edges[i].color = color;
        used_colors[u] |= (1 << color);
        used_colors[v] |= (1 << color);
        
        if (color > max_color) max_color = color;
    }
    
    printf("%d\n", max_color);
    
    // Print the colors of all edges
    for (int i = 0; i < graph->num_edges; i++) {
        printf("%d %d %d\n", graph->edges[i].u, graph->edges[i].v, graph->edges[i].color);
    }
    
    free(used_colors);
}

// Alternative approach: simpler greedy coloring for trees
void simple_tree_coloring(Graph* graph) {
    // Initialize colors
    for (int i = 0; i < graph->num_edges; i++) {
        graph->edges[i].color = 0;
    }
    
    // For a tree, we can color edges greedily
    // Each edge gets a color that's not used by its adjacent edges
    
    // We'll use a simpler approach: just assign colors in order
    int* color_used = (int*)calloc(graph->num_vertices, sizeof(int));
    
    // Color edges one by one
    for (int i = 0; i < graph->num_edges; i++) {
        int u = graph->edges[i].u;
        int v = graph->edges[i].v;
        
        // Find first unused color
        int color = 1;
        while (color_used[u] & (1 << color) || color_used[v] & (1 << color)) {
            color++;
        }
        
        graph->edges[i].color = color;
        color_used[u] |= (1 << color);
        color_used[v] |= (1 << color);
    }
    
    // Find maximum color used
    int max_color = 0;
    for (int i = 0; i < graph->num_edges; i++) {
        if (graph->edges[i].color > max_color) {
            max_color = graph->edges[i].color;
        }
    }
    
    printf("%d\n", max_color);
    
    free(color_used);
}

// Main function
int main() {
    int n, m;
    scanf("%d %d", &n, &m);
    
    Graph* graph = create_graph(n, m);
    
    // Read edges
    for (int i = 0; i < m; i++) {
        int u, v;
        scanf("%d %d", &u, &v);
        graph->edges[i].u = u;
        graph->edges[i].v = v;
        add_edge(graph, u, v);
    }
    
    // Simple approach for tree coloring
    simple_tree_coloring(graph);
    
    // Free memory
    for (int i = 0; i < n; i++) {
        Node* temp = graph->adj_list[i];
        while (temp != NULL) {
            Node* next = temp->next;
            free(temp);
            temp = next;
        }
    }
    free(graph);
    
    return 0;
}
```

## Explanation

This solution implements tree coloring using a greedy approach:

1. **Graph Representation**: Uses adjacency list representation for efficient traversal
2. **Edge Coloring**: For each edge, assigns the smallest available color that doesn't conflict with adjacent edges
3. **Color Tracking**: Uses bit manipulation to track which colors are already used for each vertex
4. **Output**: Prints the maximum number of colors used and the color of each edge

## Time Complexity
- O(m × Δ) where m is the number of edges and Δ is the maximum degree
- In the worst case, this is O(n²) for a star graph

## Space Complexity
- O(n + m) for storing the graph and auxiliary arrays

The solution handles the tree coloring problem by ensuring no two adjacent edges share the same color, which is the fundamental requirement for edge coloring.

