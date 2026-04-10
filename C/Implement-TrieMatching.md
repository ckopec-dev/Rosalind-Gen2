# Rosalind Problem: Implement TrieMatching in C

## Problem Understanding

The task is to implement a trie matching algorithm that finds all occurrences of patterns in a text. Given a text string and a collection of patterns, we need to find all starting positions in the text where each pattern occurs.

## Solution Approach

1. Build a trie from the given patterns
2. For each position in the text, traverse the trie to find matches
3. Return all starting positions where patterns are found

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
    int pattern_index;
} TrieNode;

// Function to create a new trie node
TrieNode* create_node() {
    TrieNode* node = (TrieNode*)malloc(sizeof(TrieNode));
    node->is_end = 0;
    node->pattern_index = -1;
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
void insert_pattern(TrieNode* root, char* pattern, int pattern_index) {
    TrieNode* current = root;
    int len = strlen(pattern);
    
    for (int i = 0; i < len; i++) {
        int index = char_to_index(pattern[i]);
        if (index == -1) continue;
        
        if (current->children[index] == NULL) {
            current->children[index] = create_node();
        }
        current = current->children[index];
    }
    
    current->is_end = 1;
    current->pattern_index = pattern_index;
}

// Search for patterns in text using trie
void search_patterns(TrieNode* root, char* text, int* results, int* result_count) {
    int text_len = strlen(text);
    
    for (int i = 0; i < text_len; i++) {
        TrieNode* current = root;
        int j = i;
        
        while (j < text_len && current != NULL) {
            int index = char_to_index(text[j]);
            if (index == -1) break;
            
            if (current->children[index] == NULL) {
                break;
            }
            
            current = current->children[index];
            
            // If we found a complete pattern
            if (current->is_end) {
                results[*result_count] = i;
                (*result_count)++;
            }
            
            j++;
        }
    }
}

// Main function to solve the problem
int main() {
    // Read input
    char text[10001];
    int num_patterns;
    
    // Read text
    fgets(text, sizeof(text), stdin);
    text[strcspn(text, "\n")] = 0; // Remove newline
    
    // Read number of patterns
    scanf("%d", &num_patterns);
    getchar(); // Consume newline
    
    // Create trie root
    TrieNode* root = create_node();
    
    // Read patterns and insert into trie
    char pattern[1001];
    for (int i = 0; i < num_patterns; i++) {
        fgets(pattern, sizeof(pattern), stdin);
        pattern[strcspn(pattern, "\n")] = 0; // Remove newline
        insert_pattern(root, pattern, i);
    }
    
    // Search for patterns in text
    int results[10000];
    int result_count = 0;
    
    search_patterns(root, text, results, &result_count);
    
    // Print results
    for (int i = 0; i < result_count; i++) {
        printf("%d ", results[i]);
    }
    printf("\n");
    
    return 0;
}
```

## How it works:

1. **Trie Structure**: Each node has 4 children (for A, C, G, T) and flags to indicate end of pattern
2. **Pattern Insertion**: Build trie by inserting each pattern character by character
3. **Pattern Matching**: For each position in text, traverse trie to find all matching patterns
4. **Output**: Print starting positions where patterns occur

## Time Complexity:
- Building trie: O(∑|pattern_i|) where ∑|pattern_i| is total length of all patterns
- Searching: O(|text| × |pattern|) in worst case
- Overall: O(|text| × |pattern| + ∑|pattern_i|)

## Space Complexity:
- O(ALPHABET_SIZE × number_of_nodes) for trie storage

This implementation efficiently handles the trie matching problem by building a trie from all patterns and then searching through the text to find all occurrences.

