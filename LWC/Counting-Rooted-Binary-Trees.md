# Counting Rooted Binary Trees Solution

## Problem Understanding

We need to count the number of distinct rooted binary trees that can be formed with n labeled leaves.

For a rooted binary tree with n labeled leaves, the number of such trees is given by the formula:
**(2n-3)!!** (double factorial)

This is because:
- We have n labeled leaves
- The number of ways to arrange n labeled leaves in a binary tree structure is (2n-3)!!
- The double factorial (2n-3)!! = (2n-3) × (2n-5) × ... × 3 × 1

## Solution in Lightning Web Component

```javascript
import { LightningElement } from 'lwc';

export default class CountingRootedBinaryTrees extends LightningElement {
    inputN = '';
    result = '';
    error = '';

    handleInputChange(event) {
        this.inputN = event.target.value;
        this.error = '';
        this.result = '';
    }

    calculate() {
        const n = parseInt(this.inputN);
        
        if (isNaN(n) || n <= 0) {
            this.error = 'Please enter a positive integer';
            return;
        }

        if (n > 100) {
            this.error = 'Input too large (maximum 100)';
            return;
        }

        try {
            const result = this.countRootedBinaryTrees(n);
            this.result = result.toString();
        } catch (error) {
            this.error = 'Calculation error: ' + error.message;
        }
    }

    countRootedBinaryTrees(n) {
        // For n labeled leaves, the number of rooted binary trees is (2n-3)!!
        if (n === 1) {
            return 1;
        }

        // Calculate (2n-3)!! = (2n-3) × (2n-5) × ... × 3 × 1
        let result = 1;
        const max = 2 * n - 3;
        
        for (let i = max; i >= 1; i -= 2) {
            result *= i;
        }
        
        return result;
    }

    handleKeyPress(event) {
        if (event.key === 'Enter') {
            this.calculate();
        }
    }
}
```

```html
<template>
    <div class="container">
        <h2>Counting Rooted Binary Trees</h2>
        <p>Calculate the number of distinct rooted binary trees with n labeled leaves.</p>
        
        <div class="input-section">
            <lightning-input 
                label="Number of labeled leaves (n)" 
                type="number" 
                value={inputN}
                onchange={handleInputChange}
                onkeypress={handleKeyPress}
                min="1"
                variant="standard">
            </lightning-input>
            
            <lightning-button 
                label="Calculate" 
                onclick={calculate}
                variant="brand"
                class="slds-m-top_small">
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
                <lightning-card 
                    title="Result" 
                    icon-name="custom:custom18">
                    <div class="slds-text-body_regular">
                        <p>Number of rooted binary trees with {inputN} labeled leaves:</p>
                        <p class="slds-text-heading_medium slds-text-color_success">
                            {result}
                        </p>
                        <p>Formula: (2n-3)!! = (2n-3) × (2n-5) × ... × 3 × 1</p>
                    </div>
                </lightning-card>
            </template>
        </div>
    </div>
</template>
```

```css
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
```

## Example Usage

For n = 3:
- Formula: (2×3-3)!! = 3!! = 3 × 1 = 3
- Result: 3 rooted binary trees

For n = 4:
- Formula: (2×4-3)!! = 5!! = 5 × 3 × 1 = 15
- Result: 15 rooted binary trees

## Key Points

1. **Formula**: The number of rooted binary trees with n labeled leaves is (2n-3)!!
2. **Double Factorial**: (2n-3)!! = (2n-3) × (2n-5) × ... × 3 × 1
3. **Base Case**: For n=1, there's only 1 tree
4. **Efficiency**: The solution uses iterative multiplication for better performance
5. **Input Validation**: Handles invalid inputs and large numbers appropriately

This implementation provides a complete LWC component that calculates and displays the number of rooted binary trees for a given number of labeled leaves.

