# Rosalind Problem: Introduction to Set Operations

## Problem Statement
Given two positive integers n and m, and two sets A and B of positive integers that are subsets of {1,2,...,n}, return the sets A∪B, A∩B, A−B, and B−A.

## Solution in Lightning Web Component

```javascript
// setOperations.js
import { LightningElement } from 'lwc';

export default class SetOperations extends LightningElement {
    n = 10; // universe size
    m = 5;  // set size
    setA = new Set([1, 2, 3, 4, 5]);
    setB = new Set([3, 4, 5, 6, 7]);
    
    get union() {
        return new Set([...this.setA, ...this.setB]);
    }
    
    get intersection() {
        return new Set([...this.setA].filter(x => this.setB.has(x)));
    }
    
    get differenceAB() {
        return new Set([...this.setA].filter(x => !this.setB.has(x)));
    }
    
    get differenceBA() {
        return new Set([...this.setB].filter(x => !this.setA.has(x)));
    }
    
    get formattedUnion() {
        return Array.from(this.union).sort((a, b) => a - b).join(' ');
    }
    
    get formattedIntersection() {
        return Array.from(this.intersection).sort((a, b) => a - b).join(' ');
    }
    
    get formattedDifferenceAB() {
        return Array.from(this.differenceAB).sort((a, b) => a - b).join(' ');
    }
    
    get formattedDifferenceBA() {
        return Array.from(this.differenceBA).sort((a, b) => a - b).join(' ');
    }
    
    // Method to parse input from text area
    parseInput(inputText) {
        const lines = inputText.trim().split('\n');
        const n = parseInt(lines[0]);
        const m = parseInt(lines[1]);
        const setA = new Set(lines[2].trim().split(' ').map(x => parseInt(x)));
        const setB = new Set(lines[3].trim().split(' ').map(x => parseInt(x)));
        
        return { n, m, setA, setB };
    }
    
    // Method to format output
    formatOutput(n, m, setA, setB) {
        const union = new Set([...setA, ...setB]);
        const intersection = new Set([...setA].filter(x => setB.has(x)));
        const differenceAB = new Set([...setA].filter(x => !setB.has(x)));
        const differenceBA = new Set([...setB].filter(x => !setA.has(x)));
        
        return [
            Array.from(union).sort((a, b) => a - b).join(' '),
            Array.from(intersection).sort((a, b) => a - b).join(' '),
            Array.from(differenceAB).sort((a, b) => a - b).join(' '),
            Array.from(differenceBA).sort((a, b) => a - b).join(' ')
        ].join('\n');
    }
}
```

```html
<!-- setOperations.html -->
<template>
    <div class="container">
        <h2>Set Operations</h2>
        
        <div class="input-section">
            <h3>Input</h3>
            <lightning-textarea 
                label="Enter input (format: n, m, setA, setB)"
                value={inputText}
                onchange={handleInputChange}
                rows="10">
            </lightning-textarea>
        </div>
        
        <div class="output-section">
            <h3>Output</h3>
            <lightning-textarea 
                label="Union (A∪B)"
                value={formattedUnion}
                readonly>
            </lightning-textarea>
            
            <lightning-textarea 
                label="Intersection (A∩B)"
                value={formattedIntersection}
                readonly>
            </lightning-textarea>
            
            <lightning-textarea 
                label="Difference (A−B)"
                value={formattedDifferenceAB}
                readonly>
            </lightning-textarea>
            
            <lightning-textarea 
                label="Difference (B−A)"
                value={formattedDifferenceBA}
                readonly>
            </lightning-textarea>
        </div>
        
        <div class="result-section">
            <h3>Results</h3>
            <p>Union: {formattedUnion}</p>
            <p>Intersection: {formattedIntersection}</p>
            <p>A−B: {formattedDifferenceAB}</p>
            <p>B−A: {formattedDifferenceBA}</p>
        </div>
    </div>
</template>
```

```css
/* setOperations.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section, .output-section, .result-section {
    margin-bottom: 20px;
    padding: 15px;
    border: 1px solid #e1e1e1;
    border-radius: 8px;
}

.input-section h3, .output-section h3, .result-section h3 {
    margin-top: 0;
    color: #0070d2;
}

.lightning-textarea {
    margin-bottom: 10px;
}
```

## Example Usage

For input:
```
10
5
1 2 3 4 5
3 4 5 6 7
```

Expected output:
```
1 2 3 4 5 6 7
3 4 5
1 2
6 7
```

## Explanation

1. **Union (A∪B)**: All elements that are in either set A or set B
2. **Intersection (A∩B)**: Elements that are in both sets A and B
3. **Difference (A−B)**: Elements that are in set A but not in set B
4. **Difference (B−A)**: Elements that are in set B but not in set A

The solution uses JavaScript's Set data structure for efficient set operations and properly sorts the results for consistent output formatting.

