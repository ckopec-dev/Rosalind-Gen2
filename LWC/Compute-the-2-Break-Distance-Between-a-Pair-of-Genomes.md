# Compute the 2-Break Distance Between a Pair of Genomes

## Problem Understanding

The 2-break distance between two genomes is the minimum number of 2-break operations needed to transform one genome into another. A 2-break operation cuts two edges and rejoins them in a different way.

## Solution Approach

The key insight is that the 2-break distance equals the number of cycles in the breakpoint graph minus the number of genomes.

## Implementation

```javascript
// Lightning Web Component for computing 2-break distance
import { LightningElement } from 'lwc';

export default class Compute2BreakDistance extends LightningElement {
    // Compute 2-break distance between two genomes
    compute2BreakDistance(genome1, genome2) {
        // Convert genomes to adjacency lists
        const adj1 = this.getAdjacencyList(genome1);
        const adj2 = this.getAdjacencyList(genome2);
        
        // Create breakpoint graph
        const breakpointGraph = this.createBreakpointGraph(adj1, adj2);
        
        // Count cycles in the graph
        const cycles = this.countCycles(breakpointGraph);
        
        // 2-break distance = number of cycles - number of genomes
        return cycles - genome1.length;
    }
    
    // Get adjacency list representation of genome
    getAdjacencyList(genome) {
        const adj = new Map();
        
        // Process each chromosome
        genome.forEach(chromosome => {
            const n = chromosome.length;
            
            // Handle circular chromosomes
            for (let i = 0; i < n; i++) {
                const gene1 = chromosome[i];
                const gene2 = chromosome[(i + 1) % n];
                
                if (!adj.has(gene1)) {
                    adj.set(gene1, []);
                }
                if (!adj.has(gene2)) {
                    adj.set(gene2, []);
                }
                
                adj.get(gene1).push(gene2);
                adj.get(gene2).push(gene1);
            }
        });
        
        return adj;
    }
    
    // Create breakpoint graph from two adjacency lists
    createBreakpointGraph(adj1, adj2) {
        const graph = new Map();
        
        // Add edges from first genome
        adj1.forEach((neighbors, gene) => {
            if (!graph.has(gene)) {
                graph.set(gene, []);
            }
            neighbors.forEach(neighbor => {
                graph.get(gene).push({gene: neighbor, type: 'genome1'});
            });
        });
        
        // Add edges from second genome
        adj2.forEach((neighbors, gene) => {
            if (!graph.has(gene)) {
                graph.set(gene, []);
            }
            neighbors.forEach(neighbor => {
                graph.get(gene).push({gene: neighbor, type: 'genome2'});
            });
        });
        
        return graph;
    }
    
    // Count cycles in the breakpoint graph
    countCycles(graph) {
        const visited = new Set();
        let cycleCount = 0;
        
        // DFS to find cycles
        graph.forEach((neighbors, gene) => {
            if (!visited.has(gene)) {
                // Start DFS from this gene
                const stack = [gene];
                visited.add(gene);
                
                let cycleLength = 0;
                
                while (stack.length > 0) {
                    const current = stack.pop();
                    cycleLength++;
                    
                    neighbors.forEach(neighbor => {
                        if (!visited.has(neighbor.gene)) {
                            visited.add(neighbor.gene);
                            stack.push(neighbor.gene);
                        }
                    });
                }
                
                // If we found a cycle (more than 2 nodes)
                if (cycleLength > 2) {
                    cycleCount++;
                }
            }
        });
        
        return cycleCount;
    }
    
    // Alternative simpler approach using cycle counting
    compute2BreakDistanceSimple(genome1, genome2) {
        // Convert to signed permutations
        const perm1 = this.toSignedPermutation(genome1);
        const perm2 = this.toSignedPermutation(genome2);
        
        // Count cycles in the permutation graph
        const cycles = this.countPermutationCycles(perm1, perm2);
        
        // 2-break distance = n - cycles where n is number of elements
        const n = perm1.length;
        return n - cycles;
    }
    
    // Convert genome to signed permutation
    toSignedPermutation(genome) {
        const perm = [];
        genome.forEach(chromosome => {
            chromosome.forEach(gene => {
                perm.push(gene);
            });
        });
        return perm;
    }
    
    // Count cycles in permutation graph
    countPermutationCycles(perm1, perm2) {
        const visited = new Set();
        let cycleCount = 0;
        
        // Create mapping from perm1 to perm2
        const mapping = new Map();
        for (let i = 0; i < perm1.length; i++) {
            mapping.set(perm1[i], perm2[i]);
        }
        
        // Find cycles
        for (let i = 0; i < perm1.length; i++) {
            if (!visited.has(perm1[i])) {
                let current = perm1[i];
                let cycleLength = 0;
                
                while (!visited.has(current)) {
                    visited.add(current);
                    current = mapping.get(current);
                    cycleLength++;
                }
                
                if (cycleLength > 0) {
                    cycleCount++;
                }
            }
        }
        
        return cycleCount;
    }
    
    // Handle input and output
    handleCompute() {
        // Example input
        const genome1 = [[+1, +2, +3, +4, +5, +6]];
        const genome2 = [[+1, -3, -6, -5, +2, +4]];
        
        const distance = this.compute2BreakDistanceSimple(genome1, genome2);
        
        console.log(`2-break distance: ${distance}`);
        // Expected output: 3
    }
}
```

## Example Usage

```javascript
// Example input
const genome1 = [[+1, +2, +3, +4, +5, +6]];
const genome2 = [[+1, -3, -6, -5, +2, +4]];

// Expected output: 3
```

## Key Concepts

1. **2-break operation**: Cuts two edges and rejoins them differently
2. **Breakpoint graph**: Graph formed by combining adjacency information from both genomes
3. **Cycle counting**: The number of cycles in the breakpoint graph determines the distance
4. **Formula**: Distance = n - c, where n is the number of elements and c is the number of cycles

## Time Complexity
- O(n) where n is the total number of elements in both genomes

## Space Complexity
- O(n) for storing the adjacency lists and graph representation

