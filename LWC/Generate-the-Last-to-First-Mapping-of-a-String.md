# Rosalind Problem: Last-to-First Mapping

## Problem Understanding

The Last-to-First mapping is a fundamental concept in Burrows-Wheeler Transform (BWT) applications. Given a string, we need to create a mapping from the last column of the BWT matrix to the first column.

## Solution Approach

In the BWT process:
1. We sort all rotations of the string lexicographically
2. The last column is the BWT transformation
3. The last-to-first mapping tells us where each character in the last column maps to in the first column

## Implementation in Lightning Web Component

```javascript
import { LightningElement } from 'lwc';

export default class LastToFirstMapping extends LightningElement {
    inputString = '';
    result = '';
    
    handleInputChange(event) {
        this.inputString = event.target.value;
    }
    
    handleGenerate() {
        if (!this.inputString) {
            this.result = 'Please enter a string';
            return;
        }
        
        const mapping = this.generateLastToFirstMapping(this.inputString);
        this.result = mapping.join(' ');
    }
    
    generateLastToFirstMapping(s) {
        // Add $ to make it circular
        const augmentedString = s + '$';
        
        // Create all rotations
        const rotations = [];
        for (let i = 0; i < augmentedString.length; i++) {
            rotations.push(augmentedString.slice(i) + augmentedString.slice(0, i));
        }
        
        // Sort rotations lexicographically
        rotations.sort();
        
        // Extract first and last columns
        const firstColumn = rotations.map(rotation => rotation[0]);
        const lastColumn = rotations.map(rotation => rotation[rotation.length - 1]);
        
        // Create mapping from last to first
        const mapping = new Array(lastColumn.length);
        const lastColumnIndices = {};
        
        // Record indices for each character in last column
        for (let i = 0; i < lastColumn.length; i++) {
            if (!lastColumnIndices[lastColumn[i]]) {
                lastColumnIndices[lastColumn[i]] = [];
            }
            lastColumnIndices[lastColumn[i]].push(i);
        }
        
        // For each character in first column, map to corresponding index in last column
        for (let i = 0; i < firstColumn.length; i++) {
            const char = firstColumn[i];
            const indices = lastColumnIndices[char];
            const index = indices.shift(); // Take the first occurrence
            mapping[i] = index;
        }
        
        return mapping;
    }
    
    handleClear() {
        this.inputString = '';
        this.result = '';
    }
}
```

## HTML Template

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Last-to-First Mapping</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="inputString">Input String</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="inputString"
                    class="slds-input"
                    value={inputString}
                    onchange={handleInputChange}
                    placeholder="Enter string (e.g., 'ACGT$')">
            </div>
        </div>
        
        <div class="slds-m-top_medium">
            <lightning-button 
                label="Generate Mapping" 
                variant="brand" 
                onclick={handleGenerate}>
            </lightning-button>
            <lightning-button 
                label="Clear" 
                variant="neutral" 
                onclick={handleClear}
                class="slds-m-left_small">
            </lightning-button>
        </div>
        
        <div class="slds-m-top_medium">
            <lightning-card title="Result">
                <div class="slds-card__body">
                    <p class="slds-text-body_regular">{result}</p>
                </div>
            </lightning-card>
        </div>
    </div>
</template>
```

## Example Usage

For input string "ACGT$":
1. All rotations: 
   - "ACGT$"
   - "CGT$A"
   - "GT$AC"
   - "T$ACG"
   - "$ACGT"
2. Sorted rotations:
   - "$ACGT"
   - "ACGT$"
   - "CGT$A"
   - "GT$AC"
   - "T$ACG"
3. First column: [$, A, C, G, T]
4. Last column: [T, $, A, C, G]
5. Mapping: [3, 0, 1, 2, 4]

## Key Concepts

1. **BWT Matrix Construction**: Creates all rotations and sorts them
2. **Last-to-First Mapping**: For each position in the last column, determines where it maps to in the first column
3. **Character Counting**: Handles duplicate characters by tracking their positions
4. **Index Mapping**: Creates the final numerical mapping array

This implementation efficiently computes the last-to-first mapping that's essential for BWT-based string algorithms and applications.

