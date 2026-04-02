# Rosalind Problem: Construct a Trie from a Collection of Patterns (C Solution)

## Problem Understanding

We need to construct a trie (prefix tree) from a collection of DNA patterns. A trie is a tree-like data structure where each path from root to leaf represents a pattern, and nodes represent nucleotides.

## Solution Approach

1. Create a trie node structure with 4 children (for A, C, G, T)
2. Insert each pattern into the trie
3. Output the edges of the trie in the required format

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 10000
#define ALPHABET_SIZE 4

// Trie node structure
typedef struct TrieNode {
    struct TrieNode* children[ALPHABET_SIZE];
    int is_end;
    int node_id;
} TrieNode;

// Global variables for node counting and output
int node_count = 0;
int edge_count = 0;
int output_edges[10000][3]; // [from, to, char]

// Create a new trie node
TrieNode* create_node() {
    TrieNode* node = (TrieNode*)malloc(sizeof(TrieNode));
    node->is_end = 0;
    node->node_id = node_count++;
    
    for (int i = 0; i < ALPHABET_SIZE; i++) {
        node->children[i] = NULL;
    }
    
    return node;
}

// Convert character to index (A=0, C=1, G=2, T=3)
int char_to_index(char c) {
    switch (c) {
        case 'A': return 0;
        case 'C': return 1;
        case 'G': return 2;
        case 'T': return 3;
        default: return -1;
    }
}

// Insert a pattern into the trie
void insert_pattern(TrieNode* root, char* pattern) {
    TrieNode* current = root;
    
    for (int i = 0; pattern[i] != '\0'; i++) {
        int index = char_to_index(pattern[i]);
        
        if (current->children[index] == NULL) {
            current->children[index] = create_node();
            // Store edge: from current node, to new node, with character
            output_edges[edge_count][0] = current->node_id;
            output_edges[edge_count][1] = current->children[index]->node_id;
            output_edges[edge_count][2] = index;
            edge_count++;
        }
        
        current = current->children[index];
    }
    
    current->is_end = 1;
}

// Build trie from collection of patterns
void build_trie(char** patterns, int num_patterns) {
    TrieNode* root = create_node();
    
    for (int i = 0; i < num_patterns; i++) {
        insert_pattern(root, patterns[i]);
    }
    
    // Print edges
    for (int i = 0; i < edge_count; i++) {
        printf("%d %d %c\n", output_edges[i][0], output_edges[i][1], 
               "ACGT"[output_edges[i][2]]);
    }
}

int main() {
    // Read number of patterns
    int num_patterns;
    scanf("%d", &num_patterns);
    
    // Allocate memory for patterns
    char** patterns = (char**)malloc(num_patterns * sizeof(char*));
    
    // Read patterns
    for (int i = 0; i < num_patterns; i++) {
        patterns[i] = (char*)malloc(1000 * sizeof(char)); // Assuming max 1000 characters
        scanf("%s", patterns[i]);
    }
    
    // Build and print trie
    build_trie(patterns, num_patterns);
    
    // Free memory
    for (int i = 0; i < num_patterns; i++) {
        free(patterns[i]);
    }
    free(patterns);
    
    return 0;
}
```

## Sample Input/Output

**Input:**
```
3
ATAGA
ATC
GAT
```

**Output:**
```
0 1 A
1 2 T
2 3 A
3 4 G
4 5 A
1 6 C
0 7 G
7 8 A
8 9 T
```

## Explanation

1. **Node Creation**: Each node in the trie is assigned a unique ID starting from 0
2. **Edge Storage**: We store edges as [from_node, to_node, character] in the output_edges array
3. **Pattern Insertion**: For each pattern, we traverse the trie, creating new nodes as needed
4. **Output Format**: Each line represents an edge in the trie with the source node, destination node, and the character label

## Time and Space Complexity

- **Time Complexity**: O(M × L) where M is the number of patterns and L is the average length of patterns
- **Space Complexity**: O(M × L) for storing the trie structure

The solution efficiently builds the trie by inserting each pattern character by character and outputs the edges in the required format.

