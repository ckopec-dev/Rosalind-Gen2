# Rosalind Problem: Generate the d-Neighborhood of a String

## Problem Description
The d-neighborhood of a string is the set of all strings that have Hamming distance at most d from the original string.

## Solution in Lightning Web Component

```javascript
// dNeighborhood.js
import { LightningElement } from 'lwc';

export default class DNeighborhood extends LightningElement {
    inputString = '';
    distance = 0;
    result = '';
    
    handleStringChange(event) {
        this.inputString = event.target.value;
    }
    
    handleDistanceChange(event) {
        this.distance = parseInt(event.target.value);
    }
    
    generateDNeighborhood() {
        if (!this.inputString || this.distance < 0) {
            this.result = 'Please enter valid input';
            return;
        }
        
        const neighborhood = this.getDNeighborhood(this.inputString, this.distance);
        this.result = neighborhood.join('\n');
    }
    
    getDNeighborhood(pattern, d) {
        // Base case: if d is 0, return the pattern itself
        if (d === 0) {
            return [pattern];
        }
        
        // Base case: if pattern length is 1, return all nucleotides
        if (pattern.length === 1) {
            return ['A', 'C', 'G', 'T'];
        }
        
        const neighborhood = new Set();
        const nucleotides = ['A', 'C', 'G', 'T'];
        const suffix = pattern.substring(1);
        const suffixNeighbors = this.getDNeighborhood(suffix, d);
        
        for (const text of suffixNeighbors) {
            if (this.hammingDistance(pattern.substring(1), text) < d) {
                // Add all nucleotides at the beginning
                for (const nucleotide of nucleotides) {
                    neighborhood.add(nucleotide + text);
                }
            } else {
                // Add the original first nucleotide
                neighborhood.add(pattern[0] + text);
            }
        }
        
        return Array.from(neighborhood);
    }
    
    hammingDistance(str1, str2) {
        let distance = 0;
        const minLength = Math.min(str1.length, str2.length);
        
        for (let i = 0; i < minLength; i++) {
            if (str1[i] !== str2[i]) {
                distance++;
            }
        }
        
        return distance;
    }
    
    // Alternative implementation using recursive approach
    getDNeighborhoodRecursive(pattern, d) {
        if (d === 0) {
            return [pattern];
        }
        
        if (pattern.length === 1) {
            return ['A', 'C', 'G', 'T'];
        }
        
        const neighborhood = new Set();
        const suffixNeighbors = this.getDNeighborhoodRecursive(pattern.substring(1), d);
        
        for (const text of suffixNeighbors) {
            if (this.hammingDistance(pattern.substring(1), text) < d) {
                // If we can change the first character, add all possibilities
                for (const nucleotide of ['A', 'C', 'G', 'T']) {
                    neighborhood.add(nucleotide + text);
                }
            } else {
                // If we cannot change the first character, keep it
                neighborhood.add(pattern[0] + text);
            }
        }
        
        return Array.from(neighborhood);
    }
}
```

```html
<!-- dNeighborhood.html -->
<template>
    <div class="container">
        <h2>d-Neighborhood of a String</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Input String" 
                value={inputString}
                onchange={handleStringChange}
                type="text"
                variant="standard">
            </lightning-input>
            
            <lightning-input 
                label="Distance (d)" 
                value={distance}
                onchange={handleDistanceChange}
                type="number"
                min="0"
                variant="standard">
            </lightning-input>
            
            <lightning-button 
                label="Generate d-Neighborhood" 
                onclick={generateDNeighborhood}
                variant="brand">
            </lightning-button>
        </div>
        
        <div class="result-section">
            <h3>Result:</h3>
            <lightning-textarea 
                value={result}
                readonly="true"
                label="d-Neighborhood"
                rows="10">
            </lightning-textarea>
        </div>
    </div>
</template>
```

```css
/* dNeighborhood.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
    padding: 15px;
    border: 1px solid #e1e1e1;
    border-radius: 5px;
}

.result-section {
    margin-top: 20px;
}

.result-section h3 {
    margin-bottom: 10px;
}
```

## Example Usage

For input string "ACG" and distance 1:
- Output would include: "ACG", "CCG", "TCG", "GCG", "AAG", "ATG", "AGG", "ACT", "ACC", "ACG", "ACA"

## Key Features

1. **Input Validation**: Checks for valid input string and distance
2. **Recursive Algorithm**: Implements the d-neighborhood generation algorithm
3. **Hamming Distance Calculation**: Computes the distance between strings
4. **User Interface**: Clean LWC interface for input and output
5. **Set Data Structure**: Ensures unique results in the neighborhood

## Algorithm Explanation

The algorithm works recursively:
1. If d=0, return the original string
2. If pattern length is 1, return all nucleotides
3. For longer patterns:
   - Generate d-neighborhood of the suffix
   - For each neighbor, decide whether to keep the first character or replace it
   - If Hamming distance < d, we can change the first character
   - Otherwise, keep the original first character

This implementation efficiently generates all strings within Hamming distance d from the input string.

