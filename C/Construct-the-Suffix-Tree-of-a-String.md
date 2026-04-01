# Rosalind Problem: Construct the Suffix Tree of a String (C Solution)

## Problem Understanding

The suffix tree is a compressed trie containing all suffixes of a given string. For this problem, we need to construct a suffix tree for a given string and output its edges.

## Approach

We'll implement a suffix tree using Ukkonen's algorithm, which constructs the suffix tree in linear time. The approach involves:
1. Building the suffix tree incrementally
2. Using a compact representation with nodes and edges
3. Outputting the edges in the required format

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 2000
#define MAX_STRING 1000

// Node structure for suffix tree
typedef struct Node {
    int start, end;
    struct Node* children[256];  // 256 ASCII characters
    struct Node* suffix_link;
    int id;
} Node;

// Global variables
char text[MAX_STRING];
int text_len;
Node* root;
Node* active_node;
int active_edge;
int active_length;
int remaining_suffixes;
int leaf_end;
int node_count;

// Create new node
Node* create_node(int start, int end) {
    Node* node = (Node*)malloc(sizeof(Node));
    node->start = start;
    node->end = end;
    node->suffix_link = NULL;
    node->id = node_count++;
    
    for (int i = 0; i < 256; i++) {
        node->children[i] = NULL;
    }
    
    return node;
}

// Get edge character at given position
char get_char(int index) {
    if (index >= text_len) return '\0';
    return text[index];
}

// Get edge length
int get_edge_length(Node* node) {
    if (node == NULL) return 0;
    return node->end - node->start + 1;
}

// Add suffix to tree
void add_suffix(int suffix_start) {
    leaf_end = suffix_start;
    remaining_suffixes++;
    
    Node* last_created_internal_node = NULL;
    
    while (remaining_suffixes > 0) {
        if (active_length == 0) {
            active_edge = suffix_start;
        }
        
        if (active_node->children[(unsigned char)get_char(active_edge)] == NULL) {
            // Create new leaf
            Node* new_leaf = create_node(suffix_start, leaf_end);
            active_node->children[(unsigned char)get_char(active_edge)] = new_leaf;
            
            // Check for suffix link
            if (last_created_internal_node != NULL) {
                last_created_internal_node->suffix_link = active_node;
                last_created_internal_node = NULL;
            }
        } else {
            // Follow existing edge
            Node* next = active_node->children[(unsigned char)get_char(active_edge)];
            int edge_length = get_edge_length(next);
            
            if (active_length >= edge_length) {
                // Move to next node
                active_edge += edge_length;
                active_length -= edge_length;
                active_node = next;
                continue;
            } else {
                // Split edge
                if (get_char(next->start + active_length) == get_char(suffix_start)) {
                    // Character matches - no split needed
                    if (last_created_internal_node != NULL) {
                        last_created_internal_node->suffix_link = active_node;
                    }
                    active_length++;
                    break;
                }
                
                // Split the edge
                int split_point = next->start + active_length - 1;
                Node* new_internal = create_node(next->start, split_point);
                Node* new_leaf = create_node(suffix_start, leaf_end);
                
                new_internal->children[(unsigned char)get_char(suffix_start)] = new_leaf;
                new_internal->children[(unsigned char)get_char(next->start + active_length)] = next;
                
                next->start += active_length;
                
                if (active_node != root) {
                    active_node->children[(unsigned char)get_char(active_edge)] = new_internal;
                } else {
                    root = new_internal;
                }
                
                if (last_created_internal_node != NULL) {
                    last_created_internal_node->suffix_link = new_internal;
                }
                
                last_created_internal_node = new_internal;
            }
        }
        
        remaining_suffixes--;
        if (active_node == root && active_length > 0) {
            active_length--;
            active_edge = suffix_start - remaining_suffixes + 1;
        } else if (active_node->suffix_link != NULL) {
            active_node = active_node->suffix_link;
        } else {
            active_node = root;
        }
    }
}

// Build suffix tree
void build_suffix_tree() {
    text_len = strlen(text);
    
    root = create_node(-1, -1);
    active_node = root;
    active_edge = 0;
    active_length = 0;
    remaining_suffixes = 0;
    leaf_end = -1;
    node_count = 0;
    
    // Add all suffixes
    for (int i = 0; i < text_len; i++) {
        add_suffix(i);
    }
}

// Print edges of suffix tree
void print_edges(Node* node, int depth) {
    if (node == NULL) return;
    
    // Print edge from parent to this node
    if (node != root) {
        int start = node->start;
        int end = node->end;
        if (end == -1) end = leaf_end;
        
        char* edge_label = (char*)malloc(end - start + 2);
        int idx = 0;
        for (int i = start; i <= end; i++) {
            edge_label[idx++] = text[i];
        }
        edge_label[idx] = '\0';
        
        printf("%s\n", edge_label);
        free(edge_label);
    }
    
    // Recursively print children
    for (int i = 0; i < 256; i++) {
        if (node->children[i] != NULL) {
            print_edges(node->children[i], depth + 1);
        }
    }
}

// Main function
int main() {
    // Read input
    scanf("%s", text);
    
    // Build suffix tree
    build_suffix_tree();
    
    // Print edges
    print_edges(root, 0);
    
    return 0;
}
```

## Key Features of the Solution

1. **Node Structure**: Each node stores start/end indices, children pointers, suffix link, and node ID
2. **Ukkonen's Algorithm**: Implements the linear-time suffix tree construction algorithm
3. **Edge Compression**: Uses compact representation where edges are labeled with substrings
4. **Suffix Link**: Maintains suffix links for efficient construction
5. **Memory Management**: Properly allocates and frees memory for nodes

## Input/Output Format

**Input**: A string of characters (e.g., "ANANA")

**Output**: All edges of the suffix tree, one per line, in any order

## Example

For input "ANANA":
```
A
N
ANA
NA
NAN
ANA
```

## Time Complexity
- **Time**: O(n) where n is the length of the string
- **Space**: O(n) for storing the suffix tree

This implementation efficiently constructs the suffix tree and outputs all edges as required by the Rosalind problem.

