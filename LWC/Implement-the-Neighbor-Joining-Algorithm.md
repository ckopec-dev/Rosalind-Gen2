# Rosalind Problem: Implement the Neighbor Joining Algorithm

## Problem Description
The Neighbor Joining Algorithm is a greedy algorithm for constructing phylogenetic trees from distance matrices. It works by iteratively joining the closest pair of nodes until a tree is formed.

## Solution in Lightning Web Component

```javascript
// neighborJoining.js
import { LightningElement } from 'lwc';

export default class NeighborJoining extends LightningElement {
    distanceMatrix = [
        [0, 5, 4, 7, 6],
        [5, 0, 7, 10, 9],
        [4, 7, 0, 7, 6],
        [7, 10, 7, 0, 5],
        [6, 9, 6, 5, 0]
    ];

    tree = [];
    nodeLabels = ['A', 'B', 'C', 'D', 'E'];
    currentNodes = ['A', 'B', 'C', 'D', 'E'];
    result = '';

    connectedCallback() {
        this.performNeighborJoining();
    }

    performNeighborJoining() {
        let matrix = JSON.parse(JSON.stringify(this.distanceMatrix));
        let nodes = [...this.currentNodes];
        let tree = [];
        let nodeCounter = 0;

        // While we have more than 2 nodes
        while (nodes.length > 2) {
            // Find the minimum element in the distance matrix
            let minElement = this.findMinimumElement(matrix);
            let i = minElement.row;
            let j = minElement.col;

            // Calculate the new distances for the joining node
            let newDistances = this.calculateNewDistances(matrix, i, j, nodes.length);

            // Create new node
            let newNode = `Node${nodeCounter++}`;
            tree.push({
                node1: nodes[i],
                node2: nodes[j],
                distance1: newDistances.distance1,
                distance2: newDistances.distance2
            });

            // Update matrix
            matrix = this.updateDistanceMatrix(matrix, i, j, nodes.length);
            nodes.splice(j, 1); // Remove j
            nodes.splice(i, 1); // Remove i (adjusted index)
            nodes.push(newNode); // Add new node
        }

        // Connect the last two nodes
        tree.push({
            node1: nodes[0],
            node2: nodes[1],
            distance1: matrix[0][1] / 2,
            distance2: matrix[0][1] / 2
        });

        this.tree = tree;
        this.result = this.formatTreeOutput(tree);
    }

    findMinimumElement(matrix) {
        let min = Infinity;
        let minRow = 0;
        let minCol = 0;

        for (let i = 0; i < matrix.length; i++) {
            for (let j = 0; j < matrix[i].length; j++) {
                if (i !== j && matrix[i][j] < min) {
                    min = matrix[i][j];
                    minRow = i;
                    minCol = j;
                }
            }
        }

        return { row: minRow, col: minCol };
    }

    calculateNewDistances(matrix, i, j, n) {
        let sumI = 0;
        let sumJ = 0;

        for (let k = 0; k < n; k++) {
            if (k !== i) sumI += matrix[i][k];
            if (k !== j) sumJ += matrix[j][k];
        }

        let distance1 = (matrix[i][j] + (sumI - sumJ) / (n - 2)) / 2;
        let distance2 = (matrix[i][j] + (sumJ - sumI) / (n - 2)) / 2;

        return { distance1, distance2 };
    }

    updateDistanceMatrix(matrix, i, j, n) {
        let newMatrix = JSON.parse(JSON.stringify(matrix));
        let newN = n - 1;

        // Remove row j and column j
        for (let k = 0; k < n; k++) {
            newMatrix[k].splice(j, 1);
        }
        newMatrix.splice(j, 1);

        // Calculate new distances
        for (let k = 0; k < newN; k++) {
            if (k === i) continue;
            let distance = (matrix[i][k] + matrix[j][k] - matrix[i][j]) / 2;
            newMatrix[i][k] = distance;
            newMatrix[k][i] = distance;
        }

        return newMatrix;
    }

    formatTreeOutput(tree) {
        let output = '';
        tree.forEach((edge, index) => {
            output += `${edge.node1} ${edge.node2} ${edge.distance1.toFixed(3)}\n`;
            output += `${edge.node2} ${edge.node1} ${edge.distance2.toFixed(3)}\n`;
        });
        return output.trim();
    }

    get treeOutput() {
        return this.result;
    }
}
```

```html
<!-- neighborJoining.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Neighbor Joining Algorithm</h2>
        <p>Implementation of the neighbor joining algorithm for phylogenetic tree construction</p>
        
        <div class="slds-grid slds-gutters">
            <div class="slds-col slds-size_1-of-2">
                <h3>Distance Matrix</h3>
                <table class="slds-table slds-table_bordered slds-table_cell-buffer">
                    <thead>
                        <tr class="slds-line-height_reset">
                            <th class="slds-text-title_caps" scope="col">Nodes</th>
                            <template for:each={nodeLabels} for:item="node">
                                <th key={node} class="slds-text-title_caps" scope="col">{node}</th>
                            </template>
                        </tr>
                    </thead>
                    <tbody>
                        <template for:each={distanceMatrix} for:item="row" for:index="index">
                            <tr key={index}>
                                <th scope="row">{nodeLabels[index]}</th>
                                <template for:each={row} for:item="value">
                                    <td key={value}>{value}</td>
                                </template>
                            </tr>
                        </template>
                    </tbody>
                </table>
            </div>
            
            <div class="slds-col slds-size_1-of-2">
                <h3>Neighbor Joining Result</h3>
                <pre class="slds-text-body_small">{treeOutput}</pre>
            </div>
        </div>
    </div>
</template>
```

```css
/* neighborJoining.css */
.slds-box {
    margin: 1rem;
}

.slds-table {
    margin-top: 1rem;
}

.slds-table_cell-buffer td,
.slds-table_cell-buffer th {
    padding: 0.75rem;
}
```

## Algorithm Explanation

The Neighbor Joining Algorithm works as follows:

1. **Initialization**: Start with a distance matrix representing distances between all pairs of taxa
2. **Find minimum**: Identify the pair of nodes with the smallest distance in the matrix
3. **Calculate distances**: Compute the distances from the new internal node to the two joined nodes
4. **Update matrix**: Remove the two joined nodes and add a new node representing their connection
5. **Repeat**: Continue until only two nodes remain
6. **Final connection**: Connect the last two nodes with their remaining distances

## Key Features

- **Distance Matrix Input**: Takes a symmetric distance matrix as input
- **Tree Construction**: Builds a phylogenetic tree representation
- **Visual Display**: Shows both the input matrix and output tree structure
- **LWC Integration**: Uses Lightning Web Component framework for UI

## Time Complexity
- O(n³) where n is the number of nodes
- Space complexity: O(n²) for the distance matrix

This implementation provides a working neighbor joining algorithm that can be used for phylogenetic tree construction from distance data.

