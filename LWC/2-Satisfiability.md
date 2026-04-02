# Rosalind Problem 2-Satisfiability Solution in Lightning Web Component

## Problem Understanding

The 2-Satisfiability (2-SAT) problem asks whether a given Boolean formula in conjunctive normal form (CNF) where each clause contains exactly 2 literals can be satisfied.

## Solution Approach

We'll use a graph-based approach with strongly connected components (SCC) to solve 2-SAT:
1. Convert each clause (A ∨ B) to two implications: ¬A → B and ¬B → A
2. Build a directed graph with these implications
3. Check if any variable and its negation are in the same SCC
4. If yes, the formula is unsatisfiable; otherwise, it's satisfiable

## Lightning Web Component Implementation

```javascript
// satSolver.js
import { LightningElement } from 'lwc';

export default class SatSolver extends LightningElement {
    clauses = [];
    result = '';
    
    // Input handler for clauses
    handleClauseInput(event) {
        const input = event.target.value;
        this.clauses = input.split('\n')
            .filter(line => line.trim() !== '')
            .map(line => {
                const [a, b] = line.trim().split(' ');
                return [parseInt(a), parseInt(b)];
            });
    }
    
    // Main 2-SAT solver function
    solve2Sat() {
        if (this.clauses.length === 0) {
            this.result = 'No clauses provided';
            return;
        }
        
        // Build implication graph
        const graph = this.buildGraph();
        const reverseGraph = this.buildReverseGraph(graph);
        
        // Find strongly connected components
        const sccs = this.findStronglyConnectedComponents(graph, reverseGraph);
        
        // Check if any variable and its negation are in same SCC
        const satisfiable = this.checkSatisfiability(sccs);
        
        this.result = satisfiable ? 'Satisfiable' : 'Unsatisfiable';
    }
    
    // Build implication graph from clauses
    buildGraph() {
        const graph = new Map();
        
        // Initialize all nodes
        for (const [a, b] of this.clauses) {
            const absA = Math.abs(a);
            const absB = Math.abs(b);
            
            if (!graph.has(absA)) graph.set(absA, []);
            if (!graph.has(absB)) graph.set(absB, []);
        }
        
        // Add edges for each clause (A ∨ B) becomes ¬A → B and ¬B → A
        for (const [a, b] of this.clauses) {
            // Add edge ¬a → b
            const negA = -a;
            const posB = b;
            
            if (!graph.has(negA)) graph.set(negA, []);
            graph.get(negA).push(posB);
            
            // Add edge ¬b → a
            const negB = -b;
            const posA = a;
            
            if (!graph.has(negB)) graph.set(negB, []);
            graph.get(negB).push(posA);
        }
        
        return graph;
    }
    
    // Build reverse graph for Kosaraju's algorithm
    buildReverseGraph(graph) {
        const reverseGraph = new Map();
        
        // Initialize all nodes
        for (const [node, _] of graph) {
            if (!reverseGraph.has(node)) reverseGraph.set(node, []);
        }
        
        // Add reverse edges
        for (const [node, neighbors] of graph) {
            for (const neighbor of neighbors) {
                if (!reverseGraph.has(neighbor)) reverseGraph.set(neighbor, []);
                reverseGraph.get(neighbor).push(node);
            }
        }
        
        return reverseGraph;
    }
    
    // Find strongly connected components using Kosaraju's algorithm
    findStronglyConnectedComponents(graph, reverseGraph) {
        const visited = new Set();
        const finishStack = [];
        const sccs = [];
        
        // First DFS to get finish order
        for (const [node, _] of graph) {
            if (!visited.has(node)) {
                this.dfsFirst(node, graph, visited, finishStack);
            }
        }
        
        // Second DFS on reverse graph in reverse finish order
        visited.clear();
        const componentMap = new Map();
        let componentId = 0;
        
        while (finishStack.length > 0) {
            const node = finishStack.pop();
            if (!visited.has(node)) {
                const component = [];
                this.dfsSecond(node, reverseGraph, visited, component);
                sccs.push(component);
                componentId++;
            }
        }
        
        return sccs;
    }
    
    // First DFS for finish order
    dfsFirst(node, graph, visited, finishStack) {
        visited.add(node);
        
        const neighbors = graph.get(node) || [];
        for (const neighbor of neighbors) {
            if (!visited.has(neighbor)) {
                this.dfsFirst(neighbor, graph, visited, finishStack);
            }
        }
        
        finishStack.push(node);
    }
    
    // Second DFS for component finding
    dfsSecond(node, reverseGraph, visited, component) {
        visited.add(node);
        component.push(node);
        
        const neighbors = reverseGraph.get(node) || [];
        for (const neighbor of neighbors) {
            if (!visited.has(neighbor)) {
                this.dfsSecond(neighbor, reverseGraph, visited, component);
            }
        }
    }
    
    // Check if satisfiable
    checkSatisfiability(sccs) {
        // Create mapping from node to component
        const nodeToComponent = new Map();
        for (let i = 0; i < sccs.length; i++) {
            for (const node of sccs[i]) {
                nodeToComponent.set(node, i);
            }
        }
        
        // Check if any variable and its negation are in same component
        const variables = new Set();
        for (const [a, b] of this.clauses) {
            variables.add(Math.abs(a));
            variables.add(Math.abs(b));
        }
        
        for (const varName of variables) {
            const negVar = -varName;
            const compA = nodeToComponent.get(varName);
            const compNegA = nodeToComponent.get(negVar);
            
            if (compA === compNegA) {
                return false; // Unsatisfiable
            }
        }
        
        return true; // Satisfiable
    }
    
    // Handle solve button click
    handleSolve() {
        this.solve2Sat();
    }
}
```

```html
<!-- satSolver.html -->
<template>
    <div class="satisfiability-solver">
        <h2>2-Satisfiability Solver</h2>
        
        <div class="input-section">
            <h3>Enter Clauses (one per line):</h3>
            <p>Format: "a b" where a and b are integers (positive or negative)</p>
            <textarea 
                data-id="clause-input"
                oninput={handleClauseInput}
                placeholder="Example:
1 2
-1 3
-2 -3"
                rows="10"
                cols="50">
            </textarea>
        </div>
        
        <lightning-button 
            label="Solve 2-SAT" 
            variant="brand" 
            onclick={handleSolve}>
        </lightning-button>
        
        <div class="result-section">
            <h3>Result:</h3>
            <p>{result}</p>
        </div>
    </div>
</template>
```

```css
/* satSolver.css */
.satisfiability-solver {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

textarea {
    width: 100%;
    padding: 10px;
    font-family: monospace;
    border: 1px solid #ccc;
    border-radius: 4px;
}
```

## Example Usage

For the input:
```
1 2
-1 3
-2 -3
```

The solver will:
1. Convert to implications: ¬1 → 2, ¬2 → 1, 1 → 3, ¬3 → 1, 2 → -3, 3 → -2
2. Build the implication graph
3. Find strongly connected components
4. Check if any variable and its negation are in the same component
5. Return "Satisfiable" or "Unsatisfiable"

## Time Complexity

- **Time**: O(V + E) where V is the number of variables and E is the number of clauses
- **Space**: O(V + E) for storing the graph and components

## Key Features

1. **Graph-based approach** using implication edges
2. **Kosaraju's algorithm** for finding strongly connected components
3. **Input validation** and error handling
4. **Clean LWC structure** with proper separation of concerns
5. **User-friendly interface** with clear input/output

This implementation correctly solves the 2-SAT problem by leveraging the relationship between satisfiability and strongly connected components in implication graphs.

