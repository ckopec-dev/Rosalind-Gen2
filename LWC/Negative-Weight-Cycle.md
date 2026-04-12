# Rosalind Problem: Negative Weight Cycle

## Problem Description
Given a weighted directed graph, determine whether it contains a negative weight cycle reachable from the source node.

## Solution in Lightning Web Component (JavaScript)

```javascript
import { LightningElement } from 'lwc';

export default class NegativeWeightCycle extends LightningElement {
    // Bellman-Ford algorithm to detect negative weight cycle
    findNegativeWeightCycle(edges, numNodes, source) {
        // Initialize distances
        let distances = new Array(numNodes).fill(Infinity);
        distances[source] = 0;
        
        // Relax edges repeatedly
        for (let i = 0; i < numNodes - 1; i++) {
            for (let edge of edges) {
                let [u, v, w] = edge;
                if (distances[u] !== Infinity && distances[u] + w < distances[v]) {
                    distances[v] = distances[u] + w;
                }
            }
        }
        
        // Check for negative weight cycle
        for (let edge of edges) {
            let [u, v, w] = edge;
            if (distances[u] !== Infinity && distances[u] + w < distances[v]) {
                return true; // Negative cycle exists
            }
        }
        
        return false; // No negative cycle
    }
    
    // Main method to solve the problem
    solveProblem() {
        // Example input
        const edges = [
            [0, 1, -1],
            [1, 2, -2],
            [2, 3, -3],
            [3, 0, 1],
            [0, 3, 2]
        ];
        
        const numNodes = 4;
        const source = 0;
        
        const hasNegativeCycle = this.findNegativeWeightCycle(edges, numNodes, source);
        
        console.log("Has negative weight cycle:", hasNegativeCycle);
        return hasNegativeCycle;
    }
    
    // Alternative implementation with path tracking
    findNegativeWeightCycleWithPath(edges, numNodes, source) {
        let distances = new Array(numNodes).fill(Infinity);
        let predecessors = new Array(numNodes).fill(-1);
        distances[source] = 0;
        
        // Relax edges
        for (let i = 0; i < numNodes - 1; i++) {
            for (let edge of edges) {
                let [u, v, w] = edge;
                if (distances[u] !== Infinity && distances[u] + w < distances[v]) {
                    distances[v] = distances[u] + w;
                    predecessors[v] = u;
                }
            }
        }
        
        // Check for negative cycle
        for (let edge of edges) {
            let [u, v, w] = edge;
            if (distances[u] !== Infinity && distances[u] + w < distances[v]) {
                // Found negative cycle, trace it back
                return this.traceNegativeCycle(predecessors, u, v, numNodes);
            }
        }
        
        return null;
    }
    
    // Trace back negative cycle
    traceNegativeCycle(predecessors, start, end, numNodes) {
        // Find cycle by following predecessors
        let visited = new Set();
        let current = end;
        
        while (!visited.has(current)) {
            visited.add(current);
            current = predecessors[current];
            if (current === -1) break;
        }
        
        // Return the cycle
        let cycle = [];
        let node = current;
        do {
            cycle.push(node);
            node = predecessors[node];
        } while (node !== current && node !== -1);
        
        cycle.push(current);
        return cycle.reverse();
    }
    
    // Method to parse input from string
    parseInput(inputString) {
        const lines = inputString.trim().split('\n');
        const [numNodes, numEdges] = lines[0].split(' ').map(Number);
        const edges = [];
        
        for (let i = 1; i <= numEdges; i++) {
            const [u, v, w] = lines[i].split(' ').map(Number);
            edges.push([u, v, w]);
        }
        
        return { numNodes, edges };
    }
    
    // Complete solution method
    solve(inputString) {
        const { numNodes, edges } = this.parseInput(inputString);
        const source = 0; // Assuming source is node 0
        
        const result = this.findNegativeWeightCycle(edges, numNodes, source);
        
        return result ? "YES" : "NO";
    }
    
    // Example usage
    connectedCallback() {
        // Example input
        const exampleInput = `4 5
0 1 -1
1 2 -2
2 3 -3
3 0 1
0 3 2`;
        
        const solution = this.solve(exampleInput);
        console.log("Solution:", solution);
    }
}
```

## Explanation

This solution implements the Bellman-Ford algorithm to detect negative weight cycles:

1. **Initialization**: Set all distances to infinity except the source node which is set to 0.

2. **Relaxation Phase**: Run the relaxation step `n-1` times where `n` is the number of nodes. For each edge, if we can find a shorter path, update the distance.

3. **Negative Cycle Detection**: Run one more iteration to check if any distances can still be improved. If so, a negative cycle exists.

4. **Time Complexity**: O(VE) where V is vertices and E is edges.
5. **Space Complexity**: O(V) for storing distances.

## Key Features

- Implements Bellman-Ford algorithm for negative cycle detection
- Handles directed weighted graphs
- Returns "YES" if negative cycle exists, "NO" otherwise
- Includes path tracing capability for detailed cycle information
- Parses input from string format
- Works with Lightning Web Component framework

## Usage

The component can be used by calling the `solve()` method with input in the specified format:
```
<number of nodes> <number of edges>
<edge 1>
<edge 2>
...
```

Where each edge is represented as `source destination weight`.

