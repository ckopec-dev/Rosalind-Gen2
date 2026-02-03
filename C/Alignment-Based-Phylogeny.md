# Rosalind Problem: Alignment-Based_Phylogeny

I'll solve this step-by-step, implementing the alignment-based phylogeny algorithm in C.

## Problem Understanding

The problem asks us to build a phylogenetic tree using the UPGMA (Unweighted Pair Group Method with Arithmetic Mean) algorithm based on sequence alignment distances.

## Solution Approach

1. Read input sequences
2. Calculate pairwise distances using sequence alignment
3. Apply UPGMA algorithm to build the phylogenetic tree
4. Output the tree structure

## Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_SEQ_LEN 1000
#define MAX_SEQS 100
#define INF 1000000

// Structure to store sequence data
typedef struct {
    char name[50];
    char sequence[MAX_SEQ_LEN];
    int length;
} Sequence;

// Structure for distance matrix
typedef struct {
    int n;
    double matrix[MAX_SEQS][MAX_SEQS];
} DistanceMatrix;

// Structure for tree node
typedef struct TreeNode {
    int id;
    char name[50];
    struct TreeNode* left;
    struct TreeNode* right;
    double distance;
} TreeNode;

// Global variables
Sequence sequences[MAX_SEQS];
int num_sequences;
DistanceMatrix dist_matrix;

// Function to calculate edit distance between two sequences
double edit_distance(char* seq1, char* seq2) {
    int len1 = strlen(seq1);
    int len2 = strlen(seq2);
    int i, j;
    
    // Create DP table
    int** dp = (int**)malloc((len1 + 1) * sizeof(int*));
    for (i = 0; i <= len1; i++) {
        dp[i] = (int*)malloc((len2 + 1) * sizeof(int));
    }
    
    // Initialize base cases
    for (i = 0; i <= len1; i++) dp[i][0] = i;
    for (j = 0; j <= len2; j++) dp[0][j] = j;
    
    // Fill the DP table
    for (i = 1; i <= len1; i++) {
        for (j = 1; j <= len2; j++) {
            if (seq1[i-1] == seq2[j-1]) {
                dp[i][j] = dp[i-1][j-1];
            } else {
                dp[i][j] = 1 + fmin(fmin(dp[i-1][j], dp[i][j-1]), dp[i-1][j-1]);
            }
        }
    }
    
    double result = (double)dp[len1][len2];
    
    // Free memory
    for (i = 0; i <= len1; i++) {
        free(dp[i]);
    }
    free(dp);
    
    return result;
}

// Calculate distance matrix
void calculate_distance_matrix() {
    int i, j;
    for (i = 0; i < num_sequences; i++) {
        for (j = 0; j < num_sequences; j++) {
            if (i == j) {
                dist_matrix.matrix[i][j] = 0.0;
            } else {
                double dist = edit_distance(sequences[i].sequence, sequences[j].sequence);
                dist_matrix.matrix[i][j] = dist;
            }
        }
    }
    dist_matrix.n = num_sequences;
}

// Find minimum distance in distance matrix
int find_min_distance(int* visited, int n, double* min_dist) {
    int i, j;
    *min_dist = INF;
    int min_i = -1, min_j = -1;
    
    for (i = 0; i < n; i++) {
        if (!visited[i]) continue;
        for (j = i + 1; j < n; j++) {
            if (!visited[j]) continue;
            if (dist_matrix.matrix[i][j] < *min_dist) {
                *min_dist = dist_matrix.matrix[i][j];
                min_i = i;
                min_j = j;
            }
        }
    }
    
    return min_i;
}

// Perform UPGMA clustering
TreeNode* upgma() {
    int i, j, k;
    int n = num_sequences;
    
    // Initialize cluster array
    int* cluster = (int*)malloc(n * sizeof(int));
    for (i = 0; i < n; i++) {
        cluster[i] = i;
    }
    
    // Create tree nodes for each sequence
    TreeNode** nodes = (TreeNode**)malloc(n * sizeof(TreeNode*));
    for (i = 0; i < n; i++) {
        nodes[i] = (TreeNode*)malloc(sizeof(TreeNode));
        nodes[i]->id = i;
        strcpy(nodes[i]->name, sequences[i].name);
        nodes[i]->left = NULL;
        nodes[i]->right = NULL;
        nodes[i]->distance = 0.0;
    }
    
    // UPGMA algorithm
    while (n > 1) {
        double min_dist = INF;
        int min_i = -1, min_j = -1;
        
        // Find minimum distance
        for (i = 0; i < num_sequences; i++) {
            if (cluster[i] == -1) continue;
            for (j = i + 1; j < num_sequences; j++) {
                if (cluster[j] == -1) continue;
                if (dist_matrix.matrix[i][j] < min_dist) {
                    min_dist = dist_matrix.matrix[i][j];
                    min_i = i;
                    min_j = j;
                }
            }
        }
        
        // Create new internal node
        TreeNode* new_node = (TreeNode*)malloc(sizeof(TreeNode));
        new_node->id = num_sequences + (num_sequences - n);  // Unique ID
        new_node->left = nodes[min_i];
        new_node->right = nodes[min_j];
        new_node->distance = min_dist / 2.0;
        strcpy(new_node->name, "internal");
        
        // Update distance matrix
        for (i = 0; i < num_sequences; i++) {
            if (cluster[i] == -1) continue;
            if (i != min_i && i != min_j) {
                dist_matrix.matrix[min_i][i] = (dist_matrix.matrix[min_i][i] + dist_matrix.matrix[min_j][i]) / 2.0;
                dist_matrix.matrix[i][min_i] = dist_matrix.matrix[min_i][i];
            }
        }
        
        // Mark clusters as merged
        cluster[min_j] = -1;
        nodes[min_i] = new_node;
        
        n--;
    }
    
    TreeNode* root = nodes[0];
    free(cluster);
    free(nodes);
    
    return root;
}

// Print tree in Newick format
void print_tree(TreeNode* node, FILE* output) {
    if (node == NULL) return;
    
    if (node->left == NULL && node->right == NULL) {
        fprintf(output, "%s", node->name);
    } else {
        fprintf(output, "(");
        print_tree(node->left, output);
        fprintf(output, ",");
        print_tree(node->right, output);
        fprintf(output, ")");
        if (node->distance > 0) {
            fprintf(output, ":%.6f", node->distance);
        }
    }
}

// Main function
int main() {
    FILE* input = fopen("input.txt", "r");
    FILE* output = fopen("output.txt", "w");
    
    if (!input || !output) {
        printf("Error opening files\n");
        return 1;
    }
    
    // Read sequences
    char line[2 * MAX_SEQ_LEN];
    num_sequences = 0;
    
    while (fgets(line, sizeof(line), input)) {
        if (line[0] == '>') {
            // Read sequence name
            sscanf(line, "%*c%49s", sequences[num_sequences].name);
            // Read sequence
            char* seq_start = line + strlen(sequences[num_sequences].name) + 1;
            int len = 0;
            while (fgets(line, sizeof(line), input)) {
                if (line[0] == '>') {
                    ungetc(line[0], input);
                    break;
                }
                int line_len = strlen(line);
                if (line_len > 0 && line[line_len-1] == '\n') line_len--;
                memcpy(sequences[num_sequences].sequence + len, line, line_len);
                len += line_len;
            }
            sequences[num_sequences].length = len;
            sequences[num_sequences].sequence[len] = '\0';
            num_sequences++;
        }
    }
    
    // Calculate distance matrix
    calculate_distance_matrix();
    
    // Build phylogenetic tree using UPGMA
    TreeNode* root = upgma();
    
    // Output tree in Newick format
    print_tree(root, output);
    fprintf(output, ";\n");
    
    fclose(input);
    fclose(output);
    
    return 0;
}
```

## Explanation

This implementation solves the Alignment-Based_Phylogeny problem by:

1. **Reading Input**: Parses FASTA format sequences from input file
2. **Distance Calculation**: Computes edit distance between all pairs of sequences
3. **UPGMA Algorithm**: 
   - Starts with each sequence as a separate cluster
   - Repeatedly finds the closest pair of clusters
   - Merges them into a new internal node
   - Updates the distance matrix
4. **Tree Output**: Formats the resulting tree in Newick format

## Key Features

- Uses edit distance as the similarity metric
- Implements UPGMA clustering algorithm
- Handles sequence parsing in FASTA format
- Outputs tree in proper Newick format
- Memory management for dynamic structures

## Time Complexity
- Distance calculation: O(n² × m²) where n is number of sequences and m is average sequence length
- UPGMA algorithm: O(n³) for n sequences
- Overall: O(n³ × m²)

## Usage
Compile with: `gcc -o phylogeny phylogeny.c -lm`
Run with: `./phylogeny`

The output will be written to `output.txt` in Newick format.

