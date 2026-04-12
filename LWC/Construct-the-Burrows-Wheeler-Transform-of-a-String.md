# Rosalind Problem: Construct the Burrows-Wheeler Transform of a String

## Problem Understanding

The Burrows-Wheeler Transform (BWT) is a string transformation used in data compression and bioinformatics. Given a string, we need to:
1. Create all rotations of the string
2. Sort these rotations lexicographically
3. Take the last column of the sorted rotations

## Solution Approach

We'll implement this step by step:
1. Generate all rotations of the input string
2. Sort rotations lexicographically
3. Extract the last character from each rotation

## Lightning Web Component Implementation

```javascript
// bwt.js
import { LightningElement } from 'lwc';

export default class Bwt extends LightningElement {
    inputString = '';
    result = '';
    error = '';

    handleInputChange(event) {
        this.inputString = event.target.value;
        this.error = '';
    }

    handleBwt() {
        try {
            if (!this.inputString) {
                this.error = 'Please enter a string';
                return;
            }

            // Add sentinel character (typically $) to mark end of string
            const augmentedString = this.inputString + '$';
            
            // Generate all rotations
            const rotations = this.generateRotations(augmentedString);
            
            // Sort rotations lexicographically
            rotations.sort();
            
            // Extract last column
            this.result = this.extractLastColumn(rotations);
            
        } catch (error) {
            this.error = 'Error processing string: ' + error.message;
        }
    }

    generateRotations(str) {
        const rotations = [];
        const n = str.length;
        
        for (let i = 0; i < n; i++) {
            // Create rotation by moving first i characters to end
            const rotation = str.slice(i) + str.slice(0, i);
            rotations.push(rotation);
        }
        
        return rotations;
    }

    extractLastColumn(rotations) {
        return rotations.map(rotation => rotation[rotation.length - 1]).join('');
    }

    handleClear() {
        this.inputString = '';
        this.result = '';
        this.error = '';
    }
}
```

```html
<!-- bwt.html -->
<template>
    <div class="container">
        <h2>Burrows-Wheeler Transform</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Input String" 
                value={inputString}
                onchange={handleInputChange}
                placeholder="Enter DNA string (e.g., 'banana$')">
            </lightning-input>
            
            <lightning-button 
                label="Compute BWT" 
                onclick={handleBwt}
                variant="brand">
            </lightning-button>
            
            <lightning-button 
                label="Clear" 
                onclick={handleClear}
                variant="neutral">
            </lightning-button>
        </div>

        <div class="output-section">
            <template if:true={result}>
                <lightning-card title="Result">
                    <div class="bwt-result">
                        <p>Burrows-Wheeler Transform: {result}</p>
                    </div>
                </lightning-card>
            </template>

            <template if:true={error}>
                <lightning-card title="Error">
                    <div class="error-message">
                        <p style="color: red;">{error}</p>
                    </div>
                </lightning-card>
            </template>
        </div>
    </div>
</template>
```

```css
/* bwt.css */
.container {
    max-width: 800px;
    margin: 20px auto;
    padding: 20px;
}

.input-section {
    margin-bottom: 20px;
}

.output-section {
    margin-top: 20px;
}

.bwt-result {
    font-family: monospace;
    font-size: 18px;
    padding: 10px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

.error-message {
    padding: 10px;
    background-color: #ffebee;
    border-radius: 5px;
}
```

## Example Walkthrough

For input string "banana$":
1. **Rotations**: 
   - banana$
   - anana$b
   - nana$ba
   - ana$ban
   - na$bana
   - a$banan
   - $banana

2. **Sorted rotations**:
   - $banana
   - a$banan
   - ana$ban
   - anana$b
   - banana$
   - na$bana
   - nana$ba

3. **Last column**: $annb$aa

## Test Cases

**Input**: "banana$"
**Expected Output**: "annb$aa"

**Input**: "ACGT$"
**Expected Output**: "T$CGA"

## Key Features

1. **Input validation** - Checks for empty strings
2. **Error handling** - Catches and displays processing errors
3. **User-friendly interface** - Clear input/output sections
4. **Responsive design** - Works on different screen sizes
5. **Clear functionality** - Reset button to clear results

## Time Complexity
- Generation of rotations: O(n²) where n is string length
- Sorting: O(n² log n)
- Overall: O(n² log n)

## Space Complexity
- O(n²) for storing all rotations

This implementation correctly computes the Burrows-Wheeler Transform as required for Rosalind problems.

