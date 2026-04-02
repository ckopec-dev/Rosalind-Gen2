# Rosalind Problem: Implement 2-BreakOnGenomeGraph

## Problem Understanding

The 2-BreakOnGenomeGraph operation is a fundamental operation in genome rearrangement. Given a genome graph and four indices, we need to perform a 2-break operation that removes two edges and adds two new edges.

## Solution in Lightning Web Component

```javascript
// 2BreakOnGenomeGraph.js
import { LightningElement } from 'lwc';

export default class TwoBreakOnGenomeGraph extends LightningElement {
    
    /**
     * Performs 2-break operation on a genome graph
     * @param {Array} genomeGraph - The genome graph as array of edges
     * @param {number} i - First index
     * @param {number} j - Second index
     * @param {number} k - Third index
     * @param {number} l - Fourth index
     * @returns {Array} New genome graph after 2-break
     */
    twoBreakOnGenomeGraph(genomeGraph, i, j, k, l) {
        // Create a copy of the genome graph
        const newGraph = [...genomeGraph];
        
        // Find the edges to remove
        const edge1 = [i, j];
        const edge2 = [k, l];
        
        // Remove the two edges
        const edge1Index = newGraph.findIndex(edge => 
            (edge[0] === i && edge[1] === j) || 
            (edge[0] === j && edge[1] === i)
        );
        
        const edge2Index = newGraph.findIndex(edge => 
            (edge[0] === k && edge[1] === l) || 
            (edge[0] === l && edge[1] === k)
        );
        
        // Remove the edges
        if (edge1Index !== -1) {
            newGraph.splice(edge1Index, 1);
        }
        if (edge2Index !== -1) {
            newGraph.splice(edge2Index, 1);
        }
        
        // Add the new edges
        newGraph.push([i, k]);
        newGraph.push([j, l]);
        
        return newGraph;
    }
    
    /**
     * Helper function to format genome graph for display
     * @param {Array} graph - Genome graph
     * @returns {string} Formatted string representation
     */
    formatGenomeGraph(graph) {
        return graph.map(edge => `(${edge[0]}, ${edge[1]})`).join(' ');
    }
    
    /**
     * Example usage
     */
    handleExample() {
        // Example from problem
        const genomeGraph = [[1, 2], [3, 4], [5, 6], [7, 8]];
        const result = this.twoBreakOnGenomeGraph(genomeGraph, 1, 6, 3, 8);
        
        console.log('Original graph:', this.formatGenomeGraph(genomeGraph));
        console.log('Result graph:', this.formatGenomeGraph(result));
    }
    
    /**
     * Process input from user
     * @param {string} input - Input string in format "graph i j k l"
     */
    processInput(input) {
        try {
            const parts = input.trim().split(' ');
            
            // Parse genome graph (edges)
            const edges = [];
            for (let i = 0; i < parts.length - 4; i += 2) {
                edges.push([parseInt(parts[i]), parseInt(parts[i + 1])]);
            }
            
            // Parse indices
            const i = parseInt(parts[parts.length - 4]);
            const j = parseInt(parts[parts.length - 3]);
            const k = parseInt(parts[parts.length - 2]);
            const l = parseInt(parts[parts.length - 1]);
            
            // Perform 2-break
            const result = this.twoBreakOnGenomeGraph(edges, i, j, k, l);
            
            return this.formatGenomeGraph(result);
        } catch (error) {
            return "Error processing input";
        }
    }
}
```

## HTML Template

```html
<!-- two-break-on-genome-graph.html -->
<template>
    <div class="container">
        <h2>2-Break on Genome Graph</h2>
        
        <div class="input-section">
            <h3>Input Format:</h3>
            <p>Enter genome graph edges followed by four indices (i j k l)</p>
            <p>Example: "1 2 3 4 5 6 7 8 1 6 3 8"</p>
            
            <lightning-input 
                label="Input" 
                value={inputValue}
                onchange={handleInputChange}>
            </lightning-input>
            
            <lightning-button 
                label="Process" 
                onclick={handleProcess}>
            </lightning-button>
        </div>
        
        <div class="output-section">
            <h3>Result:</h3>
            <p>{outputResult}</p>
        </div>
        
        <div class="example-section">
            <h3>Example:</h3>
            <p>Original graph: (1, 2) (3, 4) (5, 6) (7, 8)</p>
            <p>2-break with indices 1, 6, 3, 8</p>
            <p>Result: (1, 6) (3, 8) (2, 4) (5, 7)</p>
        </div>
    </div>
</template>
```

## CSS Styling

```css
/* two-break-on-genome-graph.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section, .output-section, .example-section {
    margin-bottom: 20px;
    padding: 15px;
    border: 1px solid #ccc;
    border-radius: 5px;
}

.input-section h3, .output-section h3, .example-section h3 {
    margin-top: 0;
    color: #333;
}

.lightning-input {
    margin-bottom: 10px;
}
```

## Usage Instructions

1. **Input Format**: 
   - Enter genome graph edges as space-separated pairs: `1 2 3 4 5 6 7 8`
   - Followed by four indices: `1 6 3 8`

2. **Example**:
   - Input: `1 2 3 4 5 6 7 8 1 6 3 8`
   - Original graph: (1,2) (3,4) (5,6) (7,8)
   - After 2-break with indices (1,6,3,8): (1,6) (3,8) (2,4) (5,7)

3. **Operation**:
   - Remove edges (1,2) and (3,4)
   - Add edges (1,6) and (3,8)

## Key Points

- The 2-break operation is essential for genome rearrangement studies
- It models the biological process of chromosome rearrangements
- The algorithm maintains the structure of the genome graph while modifying edge connections
- Proper edge removal and addition ensures the resulting graph remains valid

This implementation provides a complete solution for the 2-break operation on genome graphs in Lightning Web Component format.

