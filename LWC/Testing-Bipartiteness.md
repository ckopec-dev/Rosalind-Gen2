# Rosalind Problem: Testing Bipartiteness

## Problem Description
A graph is bipartite if its vertices can be partitioned into two sets such that every edge connects a vertex from one set to a vertex from the other set. We need to determine whether a given undirected graph is bipartite.

## Solution Approach
We'll use BFS (Breadth-First Search) coloring to check if the graph is bipartite:
1. Color vertices with two colors (0 and 1)
2. Start with any unvisited vertex and color it with color 0
3. For each vertex, color its neighbors with the opposite color
4. If we find a neighbor that's already colored with the same color, the graph is not bipartite

## Lightning Web Component Implementation

```javascript
// bipartite.js
import { LightningElement } from 'lwc';

export default class Bipartite extends LightningElement {
    graph = [];
    isBipartite = true;
    
    // Main function to test if graph is bipartite
    testBipartiteness(adjacencyList) {
        this.graph = adjacencyList;
        const n = adjacencyList.length;
        
        // Initialize color array -1 means unvisited
        const colors = new Array(n).fill(-1);
        
        // Check each component of the graph
        for (let i = 0; i < n; i++) {
            if (colors[i] === -1) {
                if (!this.bfsCheck(i, colors)) {
                    return false;
                }
            }
        }
        
        return true;
    }
    
    // BFS helper function to check bipartiteness
    bfsCheck(startVertex, colors) {
        const queue = [startVertex];
        colors[startVertex] = 0; // Start with color 0
        
        while (queue.length > 0) {
            const currentVertex = queue.shift();
            const currentColor = colors[currentVertex];
            const oppositeColor = 1 - currentColor; // Toggle between 0 and 1
            
            // Check all neighbors
            for (const neighbor of this.graph[currentVertex]) {
                if (colors[neighbor] === -1) {
                    // Color the neighbor with opposite color
                    colors[neighbor] = oppositeColor;
                    queue.push(neighbor);
                } else if (colors[neighbor] === currentColor) {
                    // If neighbor has same color, not bipartite
                    return false;
                }
            }
        }
        
        return true;
    }
    
    // Example usage with sample data
    handleTest() {
        // Sample graph: adjacency list representation
        // Graph with 5 vertices: 0-1, 1-2, 2-3, 3-4, 4-0
        const sampleGraph = [
            [1],      // Vertex 0 connects to 1
            [0, 2],   // Vertex 1 connects to 0, 2
            [1, 3],   // Vertex 2 connects to 1, 3
            [2, 4],   // Vertex 3 connects to 2, 4
            [3]       // Vertex 4 connects to 3
        ];
        
        const result = this.testBipartiteness(sampleGraph);
        console.log('Is bipartite:', result);
        
        // Another example - a bipartite graph
        const bipartiteGraph = [
            [1, 3],   // Vertex 0 connects to 1, 3
            [0, 2],   // Vertex 1 connects to 0, 2
            [1, 3],   // Vertex 2 connects to 1, 3
            [0, 2]    // Vertex 3 connects to 0, 2
        ];
        
        const result2 = this.testBipartiteness(bipartiteGraph);
        console.log('Is bipartite (second example):', result2);
    }
    
    // Method to parse input from Rosalind format
    parseRosalindInput(input) {
        // Parse the input format from Rosalind
        // First line: number of vertices and edges
        // Following lines: edges
        
        const lines = input.trim().split('\n');
        const [n, m] = lines[0].split(' ').map(Number);
        
        const adjacencyList = Array(n).fill().map(() => []);
        
        for (let i = 1; i <= m; i++) {
            const [u, v] = lines[i].split(' ').map(Number);
            adjacencyList[u-1].push(v-1); // Convert to 0-based indexing
            adjacencyList[v-1].push(u-1);
        }
        
        return adjacencyList;
    }
    
    // Process Rosalind-style input
    processInput(input) {
        const adjacencyList = this.parseRosalindInput(input);
        return this.testBipartiteness(adjacencyList);
    }
}
```

## HTML Template

```html
<!-- bipartite.html -->
<template>
    <div class="container">
        <h2>Bipartite Graph Checker</h2>
        <lightning-button 
            label="Test Sample Graphs" 
            onclick={handleTest}
            variant="brand">
        </lightning-button>
        
        <div class="result">
            <p>Result: <span class={resultClass}>{resultText}</span></p>
        </div>
    </div>
</template>
```

## CSS Styling

```css
/* bipartite.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.result {
    margin-top: 20px;
    padding: 15px;
    border-radius: 5px;
    background-color: #f5f5f5;
}

.result.success {
    background-color: #d4edda;
    border: 1px solid #c3e6cb;
}

.result.error {
    background-color: #f8d7da;
    border: 1px solid #f5c6cb;
}

.result span {
    font-weight: bold;
}
```

## Key Features

1. **BFS-based Algorithm**: Uses breadth-first search with coloring technique
2. **Component-based Design**: Modular approach using LWC structure
3. **Input Parsing**: Handles Rosalind-style input format
4. **Multiple Test Cases**: Includes sample test cases
5. **Error Handling**: Detects non-bipartite graphs

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the color array and queue

## How It Works

1. Each vertex is colored with either 0 or 1
2. Starting from any unvisited vertex, we color it with 0
3. All its neighbors are colored with 1
4. Continue this process for all connected vertices
5. If we ever try to color a vertex that's already colored with the same color, the graph is not bipartite

This implementation efficiently determines whether a given undirected graph is bipartite using the classic BFS coloring approach.

