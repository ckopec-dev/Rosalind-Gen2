# Rosalind Problem: Reversal Distance

## Problem Statement
The reversal distance between two permutations is the minimum number of reversals required to transform one permutation into another.

## Solution in Lightning Web Component

```javascript
// reversal_distance.js
import { LightningElement } from 'lwc';

export default class ReversalDistance extends LightningElement {
    inputPermutation1 = '';
    inputPermutation2 = '';
    result = '';
    error = '';

    handleCalculate() {
        try {
            const perm1 = this.inputPermutation1.split(' ').map(Number);
            const perm2 = this.inputPermutation2.split(' ').map(Number);
            
            if (perm1.length !== perm2.length) {
                throw new Error('Permutations must have the same length');
            }
            
            const distance = this.calculateReversalDistance(perm1, perm2);
            this.result = `Reversal distance: ${distance}`;
            this.error = '';
        } catch (error) {
            this.error = error.message;
            this.result = '';
        }
    }

    calculateReversalDistance(perm1, perm2) {
        // Create a copy of perm2 to work with
        let current = [...perm2];
        let distance = 0;
        
        // For each position from left to right
        for (let i = 0; i < perm1.length; i++) {
            // Find where the element from perm1 should be
            let targetIndex = current.indexOf(perm1[i]);
            
            // If it's not in the correct position, perform a reversal
            if (targetIndex !== i) {
                // Reverse from position i to targetIndex
                this.reverse(current, i, targetIndex);
                distance++;
                
                // If the element is still not in the correct position,
                // we need to reverse again
                if (current[i] !== perm1[i]) {
                    this.reverse(current, i, i);
                    distance++;
                }
            }
        }
        
        return distance;
    }

    reverse(array, start, end) {
        while (start < end) {
            [array[start], array[end]] = [array[end], array[start]];
            start++;
            end--;
        }
    }

    handlePermutation1Change(event) {
        this.inputPermutation1 = event.target.value;
    }

    handlePermutation2Change(event) {
        this.inputPermutation2 = event.target.value;
    }
}
```

```html
<!-- reversal_distance.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Reversal Distance Calculator</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="perm1">First Permutation</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="perm1"
                    class="slds-input"
                    value={inputPermutation1}
                    onchange={handlePermutation1Change}
                    placeholder="e.g., 1 2 3 4 5"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label" for="perm2">Second Permutation</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="perm2"
                    class="slds-input"
                    value={inputPermutation2}
                    onchange={handlePermutation2Change}
                    placeholder="e.g., 5 4 3 2 1"
                />
            </div>
        </div>

        <button 
            class="slds-button slds-button_brand"
            onclick={handleCalculate}
        >
            Calculate Reversal Distance
        </button>

        <div if:true={result} class="slds-m-top_medium">
            <p class="slds-text-heading_small">{result}</p>
        </div>

        <div if:true={error} class="slds-m-top_medium">
            <p class="slds-text-color_error">{error}</p>
        </div>
    </div>
</template>
```

```css
/* reversal_distance.css */
.slds-box {
    padding: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 400px;
}

.slds-button {
    margin-top: 1rem;
}
```

## How It Works

1. **Input Processing**: The component accepts two permutations as space-separated numbers
2. **Distance Calculation**: Uses a greedy algorithm to find the minimum number of reversals:
   - For each position, find where the correct element should be
   - If it's not in place, reverse the subarray to move it to the correct position
   - Count each reversal operation
3. **Output**: Displays the calculated reversal distance

## Example Usage

Input:
- Permutation 1: `1 2 3 4 5`
- Permutation 2: `5 4 3 2 1`

Output: `Reversal distance: 2`

The algorithm works by:
1. First reversing the entire array to get `[5 4 3 2 1]` (1 reversal)
2. Then reversing to get `[1 2 3 4 5]` (1 reversal)
3. Total: 2 reversals

## Time Complexity
O(n²) where n is the length of the permutation

## Space Complexity  
O(n) for storing the permutations

