# Rosalind Problem: Compute Distances Between Leaves

## Problem Description
In this problem, you need to compute the distances between all pairs of leaves in a weighted tree.

## Solution in Lightning Web Component (JavaScript)

```javascript
// Compute Distances Between Leaves
// Given: An unrooted binary tree with weighted edges
// Return: A distance matrix where matrix[i][j] is the distance between leaves i and j

class TreeDistanceCalculator {
    constructor() {
        this.adjacencyList = new Map();
        this.leaves = [];
        this.nodeCount = 0;
    }

    // Parse input tree and build adjacency list
    buildTree(edges) {
        this.adjacencyList.clear();
        this.leaves = [];
        this.nodeCount = 0;

        // Process each edge
        for (const edge of edges) {
            const [from, to, weight] = edge.split(' ');
            const fromNode = parseInt(from);
            const toNode = parseInt(to);
            const weightValue = parseInt(weight);

            // Add nodes to adjacency list
            if (!this.adjacencyList.has(fromNode)) {
                this.adjacencyList.set(fromNode, []);
                this.nodeCount++;
            }
            if (!this.adjacencyList.has(toNode)) {
                this.adjacencyList.set(toNode, []);
                this.nodeCount++;
            }

            // Add edges
            this.adjacencyList.get(fromNode).push({node: toNode, weight: weightValue});
            this.adjacencyList.get(toNode).push({node: fromNode, weight: weightValue});
        }

        // Identify leaves (nodes with only one connection)
        this.identifyLeaves();
    }

    // Identify leaf nodes (nodes with only one neighbor)
    identifyLeaves() {
        this.leaves = [];
        for (const [node, neighbors] of this.adjacencyList.entries()) {
            if (neighbors.length === 1) {
                this.leaves.push(node);
            }
        }
    }

    // Find distance between two nodes using BFS
    findDistance(start, end) {
        if (start === end) return 0;
        
        const queue = [{node: start, distance: 0}];
        const visited = new Set();
        visited.add(start);

        while (queue.length > 0) {
            const {node, distance} = queue.shift();
            
            for (const neighbor of this.adjacencyList.get(node)) {
                if (neighbor.node === end) {
                    return distance + neighbor.weight;
                }
                
                if (!visited.has(neighbor.node)) {
                    visited.add(neighbor.node);
                    queue.push({
                        node: neighbor.node,
                        distance: distance + neighbor.weight
                    });
                }
            }
        }
        
        return -1; // No path found
    }

    // Compute distance matrix for all pairs of leaves
    computeDistanceMatrix() {
        const n = this.leaves.length;
        const matrix = Array(n).fill().map(() => Array(n).fill(0));
        
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                if (i === j) {
                    matrix[i][j] = 0;
                } else {
                    matrix[i][j] = this.findDistance(this.leaves[i], this.leaves[j]);
                }
            }
        }
        
        return matrix;
    }

    // Format output as string
    formatOutput(matrix) {
        let result = '';
        for (let i = 0; i < matrix.length; i++) {
            result += matrix[i].join(' ') + '\n';
        }
        return result.trim();
    }
}

// Main function to solve the problem
function computeDistancesBetweenLeaves(input) {
    // Parse input
    const lines = input.trim().split('\n');
    
    // First line contains number of nodes (we don't need it for our implementation)
    // But we'll parse the edges
    const edges = [];
    for (let i = 1; i < lines.length; i++) {
        if (lines[i].trim() !== '') {
            edges.push(lines[i].trim());
        }
    }
    
    // Create calculator and solve
    const calculator = new TreeDistanceCalculator();
    calculator.buildTree(edges);
    const distanceMatrix = calculator.computeDistanceMatrix();
    
    return calculator.formatOutput(distanceMatrix);
}

// Example usage:
// const input = `4
// 0->4:11
// 1->4:2
// 2->5:6
// 3->5:7
// 4->5:12
// 4->0:11
// 5->4:12
// 5->2:6
// 5->3:7
// 0->4:11
// 1->4:2
// 2->5:6
// 3->5:7`;

// console.log(computeDistancesBetweenLeaves(input));
```

## Explanation

This solution works by:

1. **Building the Tree**: Parse the input edges and construct an adjacency list representation of the tree
2. **Identifying Leaves**: Find all leaf nodes (nodes with only one connection)
3. **Computing Distances**: For each pair of leaves, use BFS to find the shortest path distance
4. **Output Format**: Return the distance matrix in the required format

## Key Features

- **Tree Representation**: Uses adjacency list for efficient graph traversal
- **BFS Algorithm**: Implements breadth-first search to find shortest paths between leaves
- **Leaf Detection**: Automatically identifies leaf nodes from the adjacency structure
- **Matrix Output**: Formats results as a distance matrix

## Time Complexity
- Building tree: O(E) where E is number of edges
- Computing all distances: O(L² × V) where L is number of leaves and V is number of vertices
- Overall: O(E + L² × V)

## Space Complexity
O(V + E) for storing the adjacency list and auxiliary data structures

The solution handles the specific requirements of computing distances between all pairs of leaves in a weighted tree, which is the core of the Rosalind problem.

