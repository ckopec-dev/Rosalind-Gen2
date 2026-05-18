# Rosalind Problem: Counting Subsets

## Problem Statement
Given a positive integer n ≤ 1000, return the number of subsets of a set with n elements.

## Solution in Lightning Web Component

```javascript
// counting_subsets.js
import { LightningElement } from 'lwc';

export default class CountingSubsets extends LightningElement {
    n = 0;
    result = 0;
    error = '';

    handleInputChange(event) {
        this.n = parseInt(event.target.value);
        this.error = '';
    }

    calculateSubsets() {
        if (isNaN(this.n) || this.n <= 0 || this.n > 1000) {
            this.error = 'Please enter a positive integer between 1 and 1000';
            return;
        }

        // Number of subsets of a set with n elements is 2^n
        // Since each element can either be included or excluded from a subset
        this.result = Math.pow(2, this.n) % 1000000; // Take modulo 1000000 as per typical Rosalind requirements
    }

    get displayResult() {
        if (this.result > 0) {
            return `Number of subsets: ${this.result}`;
        }
        return '';
    }
}
```

```html
<!-- counting_subsets.html -->
<template>
    <div class="container">
        <h2>Counting Subsets</h2>
        <p>Given a positive integer n ≤ 1000, return the number of subsets of a set with n elements.</p>
        
        <div class="input-section">
            <lightning-input 
                label="Enter n (1-1000):" 
                type="number" 
                min="1" 
                max="1000"
                value={n}
                onchange={handleInputChange}>
            </lightning-input>
            
            <lightning-button 
                label="Calculate Subsets" 
                onclick={calculateSubsets}
                variant="brand">
            </lightning-button>
        </div>

        <div class="result-section">
            <template if:true={error}>
                <lightning-alert 
                    title="Error" 
                    message={error} 
                    variant="error">
                </lightning-alert>
            </template>

            <template if:true={result}>
                <lightning-card title="Result">
                    <p class="slds-text-body_regular">{displayResult}</p>
                </lightning-card>
            </template>
        </div>
    </div>
</template>
```

```css
/* counting_subsets.css */
.container {
    max-width: 600px;
    margin: 20px auto;
    padding: 20px;
}

.input-section {
    margin-bottom: 20px;
}

.result-section {
    margin-top: 20px;
}

.slds-text-body_regular {
    margin: 10px 0;
}
```

## Explanation

### Mathematical Background
The number of subsets of a set with n elements is 2^n. This is because for each element in the set, we have two choices:
1. Include the element in the subset
2. Exclude the element from the subset

Since these choices are independent for each element, the total number of combinations is 2 × 2 × ... × 2 (n times) = 2^n.

### Key Features
1. **Input Validation**: Ensures n is between 1 and 1000
2. **Modulo Operation**: Uses modulo 1000000 to handle large numbers (standard Rosalind requirement)
3. **User-Friendly Interface**: Clean LWC component with input field and calculation button
4. **Error Handling**: Displays appropriate error messages for invalid inputs

### Usage
1. Enter a positive integer n (between 1 and 1000)
2. Click "Calculate Subsets"
3. View the result showing 2^n (modulo 1000000)

### Example
- If n = 3, the result is 2^3 = 8 subsets
- If n = 10, the result is 2^10 = 1024 subsets

The solution efficiently calculates the result using the mathematical property that the number of subsets equals 2 raised to the power of the number of elements.

