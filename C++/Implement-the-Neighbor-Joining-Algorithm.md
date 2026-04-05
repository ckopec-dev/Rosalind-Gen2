# Rosalind Problem: Implement the Neighbor Joining Algorithm

## Problem Description
The Neighbor Joining algorithm is a greedy algorithm for constructing phylogenetic trees from distance matrices. Given a distance matrix, the algorithm repeatedly joins the closest pair of nodes until a tree is formed.

## Solution

```cpp
#include <iostream>
#include <vector>
#include <iomanip>
#include <limits>
#include <cmath>

using namespace std;

class NeighborJoining {
private:
    vector<vector<double>> dist_matrix;
    vector<string> names;
    int n;
    
public:
    NeighborJoining(vector<vector<double>>& distance_matrix, vector<string>& node_names) 
        : dist_matrix(distance_matrix), names(node_names), n(node_names.size()) {}
    
    // Calculate the Q matrix for neighbor joining
    vector<vector<double>> calculateQMatrix() {
        vector<vector<double>> Q(n, vector<double>(n, 0.0));
        
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i == j) {
                    Q[i][j] = 0.0;
                } else {
                    double sum_i = 0.0, sum_j = 0.0;
                    for (int k = 0; k < n; k++) {
                        if (k != i) sum_i += dist_matrix[i][k];
                        if (k != j) sum_j += dist_matrix[j][k];
                    }
                    Q[i][j] = (n - 2) * dist_matrix[i][j] - sum_i - sum_j;
                }
            }
        }
        return Q;
    }
    
    // Find the minimum element in Q matrix
    pair<int, int> findMinimumQ(vector<vector<double>>& Q) {
        double min_val = numeric_limits<double>::max();
        pair<int, int> min_pair(0, 1);
        
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i != j && Q[i][j] < min_val) {
                    min_val = Q[i][j];
                    min_pair = make_pair(i, j);
                }
            }
        }
        return min_pair;
    }
    
    // Calculate the distance from a node to its joined partner
    double calculateDistance(int i, int j) {
        return (dist_matrix[i][j] + (getSum(i) - getSum(j)) / (n - 2));
    }
    
    // Get sum of distances for a node
    double getSum(int node) {
        double sum = 0.0;
        for (int i = 0; i < n; i++) {
            sum += dist_matrix[node][i];
        }
        return sum;
    }
    
    // Update the distance matrix after joining
    void updateMatrix(int i, int j, double distance_i, double distance_j) {
        // Create new node name
        string new_name = "(" + names[i] + "," + names[j] + ")";
        
        // Create new distance matrix
        vector<vector<double>> new_matrix(n - 1, vector<double>(n - 1, 0.0));
        vector<string> new_names(n - 1);
        
        int new_index = 0;
        for (int k = 0; k < n; k++) {
            if (k != i && k != j) {
                new_names[new_index] = names[k];
                new_index++;
            }
        }
        new_names[new_index] = new_name;
        
        // Fill the new matrix
        int new_i = 0, new_j = 0;
        for (int k = 0; k < n; k++) {
            if (k == i || k == j) continue;
            new_j = 0;
            for (int l = 0; l < n; l++) {
                if (l == i || l == j) continue;
                new_matrix[new_i][new_j] = dist_matrix[k][l];
                new_j++;
            }
            new_i++;
        }
        
        // Add new distances
        new_i = 0;
        for (int k = 0; k < n; k++) {
            if (k == i || k == j) continue;
            new_matrix[new_i][n - 2] = (dist_matrix[k][i] + dist_matrix[k][j] - dist_matrix[i][j]) / 2.0;
            new_matrix[n - 2][new_i] = new_matrix[new_i][n - 2];
            new_i++;
        }
        
        // Update matrices
        dist_matrix = new_matrix;
        names = new_names;
        n--;
    }
    
    // Print the distance matrix
    void printMatrix() {
        cout << fixed << setprecision(3);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                cout << dist_matrix[i][j] << " ";
            }
            cout << endl;
        }
    }
    
    // Build the neighbor joining tree
    vector<vector<string>> buildTree() {
        vector<vector<string>> tree_edges;
        
        while (n > 2) {
            // Calculate Q matrix
            vector<vector<double>> Q = calculateQMatrix();
            
            // Find minimum element in Q
            pair<int, int> min_pair = findMinimumQ(Q);
            int i = min_pair.first;
            int j = min_pair.second;
            
            // Calculate distances
            double distance_i = calculateDistance(i, j) / 2.0;
            double distance_j = dist_matrix[i][j] - distance_i;
            
            // Create new edge
            string edge1 = names[i] + ":" + to_string(distance_i);
            string edge2 = names[j] + ":" + to_string(distance_j);
            tree_edges.push_back({edge1, edge2});
            
            // Update matrix
            updateMatrix(i, j, distance_i, distance_j);
        }
        
        // Add final edge
        string final_edge1 = names[0] + ":" + to_string(dist_matrix[0][1]/2.0);
        string final_edge2 = names[1] + ":" + to_string(dist_matrix[0][1]/2.0);
        tree_edges.push_back({final_edge1, final_edge2});
        
        return tree_edges;
    }
};

int main() {
    // Read input
    int n;
    cin >> n;
    
    vector<string> names(n);
    vector<vector<double>> dist_matrix(n, vector<double>(n));
    
    // Read node names
    for (int i = 0; i < n; i++) {
        cin >> names[i];
    }
    
    // Read distance matrix
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> dist_matrix[i][j];
        }
    }
    
    // Create neighbor joining object
    NeighborJoining nj(dist_matrix, names);
    
    // Build tree
    vector<vector<string>> edges = nj.buildTree();
    
    // Output edges
    for (const auto& edge : edges) {
        cout << edge[0] << " " << edge[1] << endl;
    }
    
    return 0;
}
```

## Explanation

This implementation follows the neighbor joining algorithm step by step:

1. **Q Matrix Calculation**: For each pair of nodes (i,j), calculate Q[i][j] using the formula:
   ```
   Q[i][j] = (n-2) * d[i][j] - sum(d[i][k]) - sum(d[j][k])
   ```

2. **Find Minimum**: Identify the pair of nodes that minimizes Q[i][j].

3. **Distance Calculation**: Compute the distances from the two nodes to their new joined node:
   ```
   distance_i = (d[i][j] + (sum_i - sum_j) / (n-2)) / 2
   ```

4. **Matrix Update**: Remove the two joined nodes and add a new node with updated distances.

5. **Repeat**: Continue until only two nodes remain, then add the final edge.

The algorithm handles the tree construction by maintaining the distance matrix and node names, updating them after each join operation until the complete tree is built.

## Time Complexity
- O(n³) for the complete algorithm where n is the number of nodes
- Each iteration takes O(n²) time to compute Q matrix and find minimum
- There are O(n) iterations in total

## Space Complexity
- O(n²) for storing the distance matrix
- O(n) for storing node names and intermediate results

