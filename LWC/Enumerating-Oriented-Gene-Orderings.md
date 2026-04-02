# Rosalind Problem: Enumerating Oriented Gene Orderings

## Problem Description
Given a positive integer n, return the total number of signed permutations of length n, followed by a list of all such permutations.

## Solution in Lightning Web Component

```javascript
// Enumerating_Oriented_Gene_Orderings.js
import { LightningElement } from 'lwc';

export default class EnumeratingOrientedGeneOrderings extends LightningElement {
    n = 3; // Default input value
    result = [];
    permutationCount = 0;

    handleInputChange(event) {
        this.n = parseInt(event.target.value);
        this.generatePermutations();
    }

    generatePermutations() {
        if (this.n <= 0) {
            this.result = [];
            this.permutationCount = 0;
            return;
        }

        // Generate all permutations of numbers 1 to n
        const numbers = Array.from({length: this.n}, (_, i) => i + 1);
        const allPerms = this.permute(numbers);
        
        // For each permutation, generate all sign combinations
        const signedPerms = [];
        allPerms.forEach(perm => {
            const signCombinations = this.generateSignCombinations(this.n);
            signCombinations.forEach(signs => {
                const signedPerm = perm.map((num, index) => {
                    return signs[index] ? num : -num;
                });
                signedPerms.push(signedPerm);
            });
        });

        this.result = signedPerms;
        this.permutationCount = signedPerms.length;
    }

    permute(arr) {
        if (arr.length <= 1) return [arr];
        
        const result = [];
        for (let i = 0; i < arr.length; i++) {
            const current = arr[i];
            const remaining = arr.slice(0, i).concat(arr.slice(i + 1));
            const remainingPerms = this.permute(remaining);
            
            remainingPerms.forEach(perm => {
                result.push([current].concat(perm));
            });
        }
        return result;
    }

    generateSignCombinations(n) {
        const result = [];
        const total = Math.pow(2, n);
        
        for (let i = 0; i < total; i++) {
            const signs = [];
            for (let j = 0; j < n; j++) {
                signs.push((i & (1 << j)) !== 0);
            }
            result.push(signs);
        }
        return result;
    }

    get formattedResult() {
        if (this.result.length === 0) return '';
        
        let output = `${this.permutationCount}\n`;
        this.result.forEach(perm => {
            output += perm.join(' ') + '\n';
        });
        return output.trim();
    }
}
```

```html
<!-- Enumerating_Oriented_Gene_Orderings.html -->
<template>
    <div class="container">
        <h2>Enumerating Oriented Gene Orderings</h2>
        
        <div class="input-section">
            <label for="n-input">Enter n (positive integer):</label>
            <input 
                id="n-input"
                type="number" 
                value={n} 
                min="1"
                onchange={handleInputChange}
                class="input-field"
            />
        </div>

        <div class="result-section">
            <h3>Results:</h3>
            <p>Total number of signed permutations: {permutationCount}</p>
            <pre class="output">{formattedResult}</pre>
        </div>

        <div class="explanation">
            <h3>Explanation:</h3>
            <p>
                For a given n, we need to find all signed permutations of length n.
                Each permutation can have each element in either positive or negative form.
                The total number of signed permutations is n! × 2^n.
            </p>
        </div>
    </div>
</template>
```

```css
/* Enumerating_Oriented_Gene_Orderings.css */
.container {
    max-width: 800px;
    margin: 20px auto;
    padding: 20px;
    font-family: Arial, sans-serif;
}

.input-section {
    margin-bottom: 20px;
}

.input-field {
    padding: 8px;
    margin-left: 10px;
    width: 100px;
}

.result-section {
    margin-top: 20px;
}

.output {
    background-color: #f5f5f5;
    padding: 15px;
    border: 1px solid #ddd;
    border-radius: 4px;
    white-space: pre-wrap;
    font-family: monospace;
}

.explanation {
    margin-top: 30px;
    padding: 15px;
    background-color: #e8f4f8;
    border-radius: 4px;
}
```

## Algorithm Explanation

1. **Input**: A positive integer n
2. **Approach**:
   - Generate all permutations of numbers 1 to n
   - For each permutation, generate all possible sign combinations (positive/negative)
   - Each element can be either positive or negative, so 2^n combinations per permutation
   - Total permutations = n! × 2^n

3. **Steps**:
   - Generate all permutations of [1, 2, ..., n]
   - For each permutation, create all possible sign combinations
   - Combine permutations with sign combinations to get signed permutations

## Example
For n = 2:
- Permutations: [1,2], [2,1]
- Sign combinations for each: [+,+], [+,−], [−,+], [−,−]
- Signed permutations: [1,2], [1,-2], [-1,2], [-1,-2], [2,1], [2,-1], [-2,1], [-2,-1]

## Time Complexity
O(n! × 2^n × n) where n! is for permutations and 2^n is for sign combinations

## Space Complexity
O(n! × 2^n × n) for storing all results

