# Rosalind Problem: Implement_NumberToPattern

## Problem Description
The problem asks us to convert a number into its corresponding DNA pattern of a given length, where the patterns are ordered lexicographically.

## Solution in Lightning Web Component

```javascript
// NumberToPattern.js
import { LightningElement } from 'lwc';

export default class NumberToPattern extends LightningElement {
    // Main function to convert number to pattern
    numberToPattern(number, k) {
        if (k === 1) {
            return this.numberToSymbol(number);
        }
        
        const prefixIndex = Math.floor(number / 4);
        const remainder = number % 4;
        const prefixPattern = this.numberToPattern(prefixIndex, k - 1);
        const symbol = this.numberToSymbol(remainder);
        
        return prefixPattern + symbol;
    }
    
    // Convert number to DNA symbol (0->A, 1->C, 2->G, 3->T)
    numberToSymbol(number) {
        const symbols = ['A', 'C', 'G', 'T'];
        return symbols[number];
    }
    
    // Example usage
    handleConvert() {
        const number = 45;
        const k = 4;
        const result = this.numberToPattern(number, k);
        console.log(`Number ${number} with k=${k} becomes: ${result}`);
        // Expected output: "ACGT"
    }
    
    // Alternative iterative approach
    numberToPatternIterative(number, k) {
        let pattern = '';
        const symbols = ['A', 'C', 'G', 'T'];
        
        for (let i = 0; i < k; i++) {
            const remainder = number % 4;
            pattern = symbols[remainder] + pattern;
            number = Math.floor(number / 4);
        }
        
        return pattern;
    }
}
```

```html
<!-- NumberToPattern.html -->
<template>
    <div class="container">
        <h2>Number To Pattern Converter</h2>
        <div class="input-section">
            <lightning-input 
                type="number" 
                label="Number" 
                value={inputNumber}
                onchange={handleNumberChange}>
            </lightning-input>
            <lightning-input 
                type="number" 
                label="k (pattern length)" 
                value={inputK}
                onchange={handleKChange}>
            </lightning-input>
            <lightning-button 
                label="Convert" 
                onclick={handleConvert}>
            </lightning-button>
        </div>
        <div class="result-section">
            <p>Result: {result}</p>
        </div>
    </div>
</template>
```

```javascript
// NumberToPattern.css
.container {
    padding: 20px;
    max-width: 600px;
    margin: 0 auto;
}

.input-section {
    display: flex;
    flex-direction: column;
    gap: 10px;
    margin-bottom: 20px;
}

.result-section {
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 5px;
    font-family: monospace;
}
```

## Algorithm Explanation

1. **Recursive Approach**: 
   - Base case: If k=1, return the DNA symbol corresponding to the number
   - Recursive case: Divide the number by 4 to get the prefix index, use the remainder to get the last symbol
   - Combine the recursive result with the symbol

2. **Iterative Approach**:
   - Start from the rightmost position
   - For each position, get the remainder when dividing by 4
   - Map remainder to DNA symbol
   - Build the pattern from right to left

## Time and Space Complexity
- **Time Complexity**: O(k) where k is the pattern length
- **Space Complexity**: O(k) for the recursive call stack (or O(1) for iterative)

## Test Case
For input number = 45, k = 4:
- 45 ÷ 4 = 11 remainder 1 → 'C'
- 11 ÷ 4 = 2 remainder 3 → 'T' 
- 2 ÷ 4 = 0 remainder 2 → 'G'
- 0 ÷ 4 = 0 remainder 0 → 'A'
- Result: "ACGT"

