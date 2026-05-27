# Rosalind Problem: Shortest Paths in DAG

## Problem Description
Find the shortest path from node 1 to all other nodes in a weighted directed acyclic graph (DAG).

## Solution in Lightning Web Component

```javascript
import { LightningElement } from 'lwc';

export default class ShortestPathsInDag extends LightningElement {
    // Input data
    inputGraph = `6 10
1 2 5
1 3 3
1 4 6
2 4 2
2 5 4
3 4 7
3 5 5
4 5 1
4 6 8
5 6 3`;

    // Parse input and solve
    solve() {
        const lines = this.inputGraph.trim().split('\n');
        const [n, m] = lines[0].split(' ').map(Number);
        
        // Build adjacency list
        const adj = Array(n + 1).fill(null).map(() => []);
        const edges = [];
        
        for (let i = 1; i <= m; i++) {
            const [u, v, w] = lines[i].split(' ').map(Number);
            adj[u].push({to: v, weight: w});
            edges.push({from: u, to: v, weight: w});
        }
        
        // Topological sort
        const topoOrder = this.topologicalSort(n, adj);
        
        // Initialize distances
        const dist = Array(n + 1).fill(Infinity);
        dist[1] = 0;
        
        // Relax edges in topological order
        for (const u of topoOrder) {
            if (dist[u] !== Infinity) {
                for (const edge of adj[u]) {
                    const v = edge.to;
                    const weight = edge.weight;
                    if (dist[u] + weight < dist[v]) {
                        dist[v] = dist[u] + weight;
                    }
                }
            }
        }
        
        // Format output
        const result = dist.slice(1).map(d => d === Infinity ? 'x' : d.toString()).join(' ');
        return result;
    }
    
    // Topological sort using DFS
    topologicalSort(n, adj) {
        const visited = Array(n + 1).fill(false);
        const stack = [];
        
        for (let i = 1; i <= n; i++) {
            if (!visited[i]) {
                this.dfs(i, adj, visited, stack);
            }
        }
        
        return stack.reverse();
    }
    
    dfs(node, adj, visited, stack) {
        visited[node] = true;
        
        for (const edge of adj[node]) {
            if (!visited[edge.to]) {
                this.dfs(edge.to, adj, visited, stack);
            }
        }
        
        stack.push(node);
    }
    
    // Main method to run the solution
    get solution() {
        return this.solve();
    }
}
```

## HTML Template

```html
<template>
    <div class="solution-container">
        <h2>Shortest Paths in DAG</h2>
        <p>Input Graph:</p>
        <pre>{inputGraph}</pre>
        <p>Shortest distances from node 1:</p>
        <p><strong>{solution}</strong></p>
    </div>
</template>
```

## CSS Styles

```css
.solution-container {
    padding: 20px;
    font-family: Arial, sans-serif;
}

pre {
    background-color: #f4f4f4;
    padding: 10px;
    border-radius: 5px;
    overflow-x: auto;
}
```

## Explanation

This solution implements the shortest path algorithm for a directed acyclic graph (DAG):

1. **Parse Input**: Read the number of nodes and edges, then build an adjacency list representation of the graph.

2. **Topological Sort**: Since the graph is a DAG, we perform topological sorting to process nodes in the correct order.

3. **Distance Initialization**: Initialize all distances to infinity except for the source node (node 1) which is set to 0.

4. **Relaxation**: Process each node in topological order and relax all outgoing edges, updating distances if shorter paths are found.

5. **Output Format**: Convert distances to string format, using 'x' for unreachable nodes.

## Time Complexity
- **Time**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space**: O(V + E) for storing the graph and auxiliary data structures

## Example Output
For the given input graph, the output would be: `0 5 3 7 9 12`

This represents the shortest distances from node 1 to all other nodes in the DAG.

