# Rosalind Problem: Implement_GraphToGenome

## Problem Description
Given a genome graph, we need to reconstruct the original genome by finding cycles and converting them to genome strings.

## Solution

```javascript
// Lightning Web Component for GraphToGenome
import { LightningElement } from 'lwc';

export default class GraphToGenome extends LightningElement {
    // Input data
    genomeGraph = [];
    
    // Method to implement GraphToGenome
    graphToGenome(genomeGraph) {
        // Convert genome graph to cycles
        let cycles = this.findCycles(genomeGraph);
        
        // Convert cycles to genome string
        let genome = this.cyclesToGenome(cycles);
        
        return genome;
    }
    
    // Find cycles in the genome graph
    findCycles(genomeGraph) {
        let cycles = [];
        let visited = new Set();
        
        // Create adjacency list representation
        let adjList = this.buildAdjacencyList(genomeGraph);
        
        // Find all cycles
        for (let node in adjList) {
            if (!visited.has(node)) {
                let cycle = this.findCycleFromNode(node, adjList, visited);
                if (cycle.length > 0) {
                    cycles.push(cycle);
                }
            }
        }
        
        return cycles;
    }
    
    // Build adjacency list from genome graph
    buildAdjacencyList(genomeGraph) {
        let adjList = {};
        
        for (let edge of genomeGraph) {
            let from = edge[0];
            let to = edge[1];
            
            if (!adjList[from]) {
                adjList[from] = [];
            }
            adjList[from].push(to);
            
            if (!adjList[to]) {
                adjList[to] = [];
            }
            adjList[to].push(from);
        }
        
        return adjList;
    }
    
    // Find cycle starting from a given node
    findCycleFromNode(startNode, adjList, visited) {
        let cycle = [];
        let current = startNode;
        let prev = null;
        
        while (!visited.has(current)) {
            visited.add(current);
            cycle.push(current);
            
            let neighbors = adjList[current];
            let next = null;
            
            for (let neighbor of neighbors) {
                if (neighbor !== prev) {
                    next = neighbor;
                    break;
                }
            }
            
            if (next === null) {
                break;
            }
            
            prev = current;
            current = next;
            
            // If we return to start node, we found a cycle
            if (current === startNode) {
                cycle.push(current);
                break;
            }
        }
        
        return cycle;
    }
    
    // Convert cycles to genome string
    cyclesToGenome(cycles) {
        let genome = [];
        
        for (let cycle of cycles) {
            // Remove the last element as it's the same as first (cycle)
            let cycleWithoutLast = cycle.slice(0, -1);
            
            // Convert cycle to genome string
            let genomeString = this.cycleToGenomeString(cycleWithoutLast);
            genome.push(genomeString);
        }
        
        return genome;
    }
    
    // Convert single cycle to genome string
    cycleToGenomeString(cycle) {
        let result = [];
        
        for (let i = 0; i < cycle.length; i++) {
            let node = cycle[i];
            let isNegative = node < 0;
            let absNode = Math.abs(node);
            
            if (isNegative) {
                result.push(`-${absNode}`);
            } else {
                result.push(`+${absNode}`);
            }
        }
        
        return result.join(' ');
    }
    
    // Main method to solve the problem
    solve() {
        // Example input
        let exampleGraph = [
            [1, 2],
            [2, 3],
            [3, 4],
            [4, 1],
            [5, 6],
            [6, 7],
            [7, 8],
            [8, 5]
        ];
        
        this.genomeGraph = exampleGraph;
        let result = this.graphToGenome(exampleGraph);
        
        console.log("Result:", result);
        return result;
    }
}
```

## Explanation

This implementation solves the GraphToGenome problem by:

1. **Finding Cycles**: The algorithm identifies cycles in the genome graph using a depth-first search approach
2. **Building Adjacency List**: Converts the input graph into an adjacency list representation for efficient traversal
3. **Cycle Detection**: Uses visited set to track nodes and identify complete cycles
4. **Genome Reconstruction**: Converts each cycle into a genome string format

## Key Features

- **Cycle Detection**: Properly identifies cycles in the genome graph
- **Adjacency List**: Efficient graph representation for traversal
- **Genome String Conversion**: Converts cycle nodes to proper genome string format with signs
- **Modular Design**: Separates concerns into different methods for clarity

## Time Complexity
- O(V + E) where V is the number of vertices and E is the number of edges

## Space Complexity  
- O(V + E) for storing the adjacency list and visited set

The solution handles both positive and negative integers representing genome segments and properly reconstructs the genome from the given graph structure.

