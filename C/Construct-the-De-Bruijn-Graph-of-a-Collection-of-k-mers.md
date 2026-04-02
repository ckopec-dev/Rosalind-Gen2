# Rosalind Problem: Construct the De Bruijn Graph of a Collection of k-mers

## Problem Description
Given a collection of k-mers, construct the De Bruijn graph. The De Bruijn graph is a directed graph where each node represents a (k-1)-mer and each edge represents a k-mer.

## Solution in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a node in the graph
typedef struct Node {
    char* label;
    struct Node* next;
} Node;

// Structure to represent the graph
typedef struct Graph {
    Node** nodes;
    int num_nodes;
    int capacity;
} Graph;

// Hash function for strings
unsigned int hash(const char* str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash;
}

// Create a new graph
Graph* create_graph(int capacity) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->nodes = (Node**)calloc(capacity, sizeof(Node*));
    graph->num_nodes = 0;
    graph->capacity = capacity;
    return graph;
}

// Add a node to the graph
void add_node(Graph* graph, const char* label) {
    unsigned int index = hash(label) % graph->capacity;
    
    // Check if node already exists
    Node* current = graph->nodes[index];
    while (current != NULL) {
        if (strcmp(current->label, label) == 0) {
            return; // Node already exists
        }
        current = current->next;
    }
    
    // Create new node
    Node* new_node = (Node*)malloc(sizeof(Node));
    new_node->label = (char*)malloc(strlen(label) + 1);
    strcpy(new_node->label, label);
    new_node->next = graph->nodes[index];
    graph->nodes[index] = new_node;
    graph->num_nodes++;
}

// Get the prefix of a string (first k-1 characters)
char* get_prefix(const char* kmer, int k) {
    char* prefix = (char*)malloc(k);
    strncpy(prefix, kmer, k-1);
    prefix[k-1] = '\0';
    return prefix;
}

// Get the suffix of a string (last k-1 characters)
char* get_suffix(const char* kmer, int k) {
    char* suffix = (char*)malloc(k);
    strcpy(suffix, kmer + 1);
    suffix[k-1] = '\0';
    return suffix;
}

// Construct De Bruijn graph from k-mers
void construct_de_bruijn_graph(Graph* graph, char** kmers, int num_kmers, int k) {
    // Add all prefixes and suffixes as nodes
    for (int i = 0; i < num_kmers; i++) {
        char* prefix = get_prefix(kmers[i], k);
        char* suffix = get_suffix(kmers[i], k);
        
        add_node(graph, prefix);
        add_node(graph, suffix);
        
        free(prefix);
        free(suffix);
    }
    
    // Print edges (this is a simplified approach)
    printf("De Bruijn Graph Edges:\n");
    for (int i = 0; i < num_kmers; i++) {
        char* prefix = get_prefix(kmers[i], k);
        char* suffix = get_suffix(kmers[i], k);
        printf("%s -> %s\n", prefix, suffix);
        free(prefix);
        free(suffix);
    }
}

// Print the graph (for debugging)
void print_graph(Graph* graph) {
    printf("Graph nodes:\n");
    for (int i = 0; i < graph->capacity; i++) {
        Node* current = graph->nodes[i];
        while (current != NULL) {
            printf("  %s\n", current->label);
            current = current->next;
        }
    }
}

// Free the graph memory
void free_graph(Graph* graph) {
    for (int i = 0; i < graph->capacity; i++) {
        Node* current = graph->nodes[i];
        while (current != NULL) {
            Node* temp = current;
            current = current->next;
            free(temp->label);
            free(temp);
        }
    }
    free(graph->nodes);
    free(graph);
}

int main() {
    // Example input
    char* kmers[] = {"GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG"};
    int num_kmers = 7;
    int k = 4;
    
    // Create graph
    Graph* graph = create_graph(100);
    
    // Construct De Bruijn graph
    construct_de_bruijn_graph(graph, kmers, num_kmers, k);
    
    // Print graph for debugging
    // print_graph(graph);
    
    // Free memory
    free_graph(graph);
    
    return 0;
}
```

## Explanation

This C implementation solves the De Bruijn graph construction problem:

1. **Data Structures**: 
   - `Node` structure represents a graph node containing a label and pointer to next node
   - `Graph` structure represents the entire graph with hash table of nodes

2. **Key Functions**:
   - `create_graph()`: Initializes a new graph with given capacity
   - `add_node()`: Adds a node to the graph using hash table for fast lookup
   - `get_prefix()`: Extracts the first k-1 characters of a k-mer
   - `get_suffix()`: Extracts the last k-1 characters of a k-mer
   - `construct_de_bruijn_graph()`: Builds the graph by creating edges between prefix and suffix of each k-mer

3. **Algorithm**:
   - For each k-mer, extract its prefix and suffix
   - Add both prefix and suffix as nodes in the graph
   - Create an edge from prefix to suffix (representing the k-mer)

4. **Time Complexity**: O(n×k) where n is the number of k-mers and k is the k-mer length

5. **Space Complexity**: O(n×k) for storing the graph

The solution handles the example input with k-mers: `GAGG`, `CAGG`, `GGGG`, `GGGA`, `CAGG`, `AGGG`, `GGAG` and constructs the appropriate De Bruijn graph edges.

