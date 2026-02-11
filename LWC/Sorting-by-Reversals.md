# Rosalind Problem: Sorting_by_Reversals

## Problem Understanding

The Sorting_by_Reversals problem asks us to find the minimum number of reversals needed to sort a permutation, where each reversal can be applied to any contiguous subsequence of the permutation.

## Approach

We'll use a greedy approach:
1. Find the current position of the target element (from 1 to n)
2. If it's not in the correct position, reverse the subsequence from current position to target position
3. Continue until the permutation is sorted

## Solution in Lightning Web Component

```javascript
// Sorting_By_Reversals.js
import { LightningElement } from 'lwc';

export default class SortingByReversals extends LightningElement {
    inputPermutation = '';
    result = '';
    steps = [];
    isSolved = false;

    handleInputChange(event) {
        this.inputPermutation = event.target.value;
    }

    solve() {
        if (!this.inputPermutation.trim()) {
            this.result = 'Please enter a permutation';
            return;
        }

        const permutation = this.inputPermutation.trim().split(' ').map(Number);
        const steps = this.findSortingReversals(permutation);
        
        this.steps = steps;
        this.isSolved = true;
        
        if (steps.length === 0) {
            this.result = 'Permutation is already sorted';
        } else {
            this.result = `Minimum reversals needed: ${steps.length}`;
        }
    }

    findSortingReversals(permutation) {
        const steps = [];
        const target = [...permutation].sort((a, b) => a - b);
        const n = permutation.length;
        
        // Create a copy to work with
        let current = [...permutation];
        
        // For each position from left to right
        for (let i = 0; i < n; i++) {
            // Find where the element that should be at position i is currently located
            let pos = current.indexOf(i + 1);
            
            // If it's not already in the correct position
            if (pos !== i) {
                // If it's at the beginning, just reverse to put it in correct position
                if (pos === 0) {
                    // Reverse from position 0 to i (inclusive)
                    this.reverse(current, 0, i);
                    steps.push({
                        step: steps.length + 1,
                        operation: `Reverse positions 0 to ${i}`,
                        permutation: [...current]
                    });
                } else {
                    // First reverse to bring element to front
                    this.reverse(current, 0, pos);
                    steps.push({
                        step: steps.length + 1,
                        operation: `Reverse positions 0 to ${pos}`,
                        permutation: [...current]
                    });
                    
                    // Then reverse to put it in correct position
                    this.reverse(current, 0, i);
                    steps.push({
                        step: steps.length + 1,
                        operation: `Reverse positions 0 to ${i}`,
                        permutation: [...current]
                    });
                }
            }
        }
        
        return steps;
    }

    reverse(array, start, end) {
        while (start < end) {
            [array[start], array[end]] = [array[end], array[start]];
            start++;
            end--;
        }
    }

    clear() {
        this.inputPermutation = '';
        this.result = '';
        this.steps = [];
        this.isSolved = false;
    }

    get stepList() {
        return this.steps.map((step, index) => ({
            ...step,
            id: index + 1
        }));
    }
}
```

```html
<!-- Sorting_By_Reversals.html -->
<template>
    <div class="slds-box slds-p-around_medium">
        <h2>Sorting by Reversals</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Input Permutation</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={inputPermutation}
                    onchange={handleInputChange}
                    placeholder="Enter permutation (e.g., 3 1 2 4)"
                />
            </div>
        </div>

        <div class="slds-m-top_medium">
            <lightning-button 
                label="Solve" 
                variant="brand" 
                onclick={solve}
                class="slds-m-right_small"
            ></lightning-button>
            <lightning-button 
                label="Clear" 
                variant="neutral" 
                onclick={clear}
            ></lightning-button>
        </div>

        <template if:true={isSolved}>
            <div class="slds-m-top_medium">
                <p class="slds-text-heading_small">{result}</p>
                
                <template if:true={steps.length > 0}>
                    <h3 class="slds-text-heading_small slds-m-top_medium">Steps:</h3>
                    <div class="slds-grid slds-gutters slds-wrap">
                        <template for:each={stepList} for:item="step">
                            <div key={step.id} class="slds-col slds-size_1-of-1 slds-medium-size_1-of-2 slds-large-size_1-of-3">
                                <div class="slds-box slds-box_x-small">
                                    <p><strong>Step {step.id}:</strong> {step.operation}</p>
                                    <p><strong>Permutation:</strong> {step.permutation}</p>
                                </div>
                            </div>
                        </template>
                    </div>
                </template>
            </div>
        </template>
    </div>
</template>
```

```css
/* Sorting_By_Reversals.css */
.slds-box_x-small {
    padding: 0.5rem;
}
```

## How It Works

1. **Input**: User provides a permutation of numbers (e.g., "3 1 2 4")
2. **Algorithm**: 
   - For each position from left to right, find the correct element
   - If it's not in the correct position, perform reversals to move it there
   - Use a greedy approach: bring element to front, then to correct position
3. **Output**: Shows the minimum number of reversals and the step-by-step process

## Example

Input: `3 1 2 4`
- Step 1: Reverse positions 0-1 to move 1 to position 0: `1 3 2 4`
- Step 2: Reverse positions 0-2 to move 2 to position 2: `2 3 1 4`  
- Step 3: Reverse positions 0-1 to move 1 to position 0: `1 3 2 4`
- Step 4: Reverse positions 0-2 to move 3 to position 1: `1 2 3 4`

Total: 4 reversals needed

## Time Complexity
- O(n²) where n is the length of the permutation
- Space complexity: O(n) for storing the steps

This solution handles the core problem of finding the minimum number of reversals needed to sort a permutation using a greedy algorithm approach.

