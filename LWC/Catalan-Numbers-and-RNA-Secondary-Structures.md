# Rosalind Problem: Catalan Numbers and RNA Secondary Structures

## Problem Understanding

This problem asks us to find the number of possible secondary structures for an RNA sequence, which is equivalent to finding the nth Catalan number where n is the number of base pairs.

## Solution Approach

The key insight is that the number of valid RNA secondary structures (where each nucleotide can pair with at most one other nucleotide) follows the Catalan number sequence. For a sequence of length n, we need to count valid pairings.

## Lightning Web Component Implementation

```javascript
// catalan-rna.js
import { LightningElement } from 'lwc';

export default class CatalanRna extends LightningElement {
    inputSequence = '';
    result = 0;
    errorMessage = '';

    handleInputChange(event) {
        this.inputSequence = event.target.value.toUpperCase();
        this.errorMessage = '';
    }

    calculateCatalan() {
        if (!this.inputSequence) {
            this.errorMessage = 'Please enter an RNA sequence';
            return;
        }

        // Validate RNA sequence (only A, U, G, C allowed)
        const validSequence = /^[AUCG]+$/;
        if (!validSequence.test(this.inputSequence)) {
            this.errorMessage = 'Invalid RNA sequence. Only A, U, G, C characters allowed.';
            return;
        }

        // For RNA secondary structures, we calculate the number of valid pairings
        // This is equivalent to finding the nth Catalan number where n is the number of base pairs
        const n = this.inputSequence.length / 2;
        
        if (n !== Math.floor(n)) {
            this.errorMessage = 'RNA sequence length must be even for valid secondary structures';
            return;
        }

        this.result = this.catalanNumber(n);
        this.errorMessage = '';
    }

    // Calculate nth Catalan number using dynamic programming
    catalanNumber(n) {
        if (n <= 1) return 1;
        
        // Use dynamic programming to avoid overflow
        const catalan = new Array(n + 1);
        catalan[0] = 1;
        catalan[1] = 1;
        
        for (let i = 2; i <= n; i++) {
            catalan[i] = 0;
            for (let j = 0; j < i; j++) {
                catalan[i] += catalan[j] * catalan[i - 1 - j];
            }
        }
        
        return catalan[n];
    }

    // Alternative implementation using the direct formula
    catalanNumberFormula(n) {
        if (n <= 1) return 1;
        
        // Using the formula: C(n) = (2n)! / ((n+1)! * n!)
        // But for large n, we use the recurrence relation to avoid overflow
        return this.catalanNumber(n);
    }

    // More efficient implementation for large numbers using memoization
    catalanNumberMemo(n, memo = {}) {
        if (n in memo) return memo[n];
        if (n <= 1) return memo[n] = 1;
        
        let result = 0;
        for (let i = 0; i < n; i++) {
            result += this.catalanNumberMemo(i, memo) * this.catalanNumberMemo(n - 1 - i, memo);
        }
        
        return memo[n] = result;
    }

    get sequenceLength() {
        return this.inputSequence.length;
    }

    get isEvenLength() {
        return this.inputSequence.length % 2 === 0;
    }

    get basePairCount() {
        return this.inputSequence.length / 2;
    }
}
```

```html
<!-- catalan-rna.html -->
<template>
    <div class="container">
        <h2>RNA Secondary Structures and Catalan Numbers</h2>
        
        <div class="input-section">
            <lightning-input 
                label="RNA Sequence" 
                value={inputSequence}
                onchange={handleInputChange}
                placeholder="Enter RNA sequence (e.g., AUCGUAUCG)"
                type="text">
            </lightning-input>
            
            <lightning-button 
                label="Calculate Secondary Structures" 
                onclick={calculateCatalan}
                variant="brand">
            </lightning-button>
        </div>

        <div class="result-section">
            <template if:true={errorMessage}>
                <lightning-alert 
                    title="Error" 
                    message={errorMessage} 
                    variant="error">
                </lightning-alert>
            </template>

            <template if:true={result}>
                <div class="result-card">
                    <p><strong>Input Sequence:</strong> {inputSequence}</p>
                    <p><strong>Sequence Length:</strong> {sequenceLength} nucleotides</p>
                    <p><strong>Base Pairs:</strong> {basePairCount}</p>
                    <p><strong>Number of Valid Secondary Structures:</strong> {result}</p>
                    <p><strong>This corresponds to the {basePairCount}th Catalan number</strong></p>
                </div>
            </template>
        </div>

        <div class="explanation">
            <h3>Explanation</h3>
            <p>The number of possible secondary structures for an RNA sequence follows the Catalan number sequence:</p>
            <ul>
                <li>C(0) = 1</li>
                <li>C(1) = 1</li>
                <li>C(n) = Σ(i=0 to n-1) C(i) × C(n-1-i)</li>
            </ul>
            <p>For an RNA sequence of length n, the number of valid secondary structures is the (n/2)th Catalan number.</p>
        </div>
    </div>
</template>
```

```css
/* catalan-rna.css */
.container {
    max-width: 800px;
    margin: 0 auto;
    padding: 20px;
    font-family: Arial, sans-serif;
}

.input-section {
    margin-bottom: 20px;
    padding: 15px;
    border: 1px solid #e1e1e1;
    border-radius: 5px;
    background-color: #f9f9f9;
}

.result-section {
    margin-bottom: 20px;
}

.result-card {
    padding: 15px;
    border: 1px solid #d1d1d1;
    border-radius: 5px;
    background-color: #f0f8ff;
    margin-top: 10px;
}

.explanation {
    margin-top: 30px;
    padding: 15px;
    border: 1px solid #e1e1e1;
    border-radius: 5px;
    background-color: #f9f9f9;
}

.explanation h3 {
    margin-top: 0;
}

.explanation ul {
    margin: 10px 0;
    padding-left: 20px;
}

.explanation li {
    margin: 5px 0;
}
```

## Key Features

1. **Input Validation**: Checks for valid RNA sequences (A, U, G, C only)
2. **Catalan Number Calculation**: Implements dynamic programming approach to compute Catalan numbers
3. **User-Friendly Interface**: Clear input, calculation button, and result display
4. **Error Handling**: Provides meaningful error messages for invalid inputs
5. **Educational Component**: Includes explanation of the mathematical concept

## How It Works

1. User inputs an RNA sequence
2. The system validates the sequence and checks if it's even-length (required for valid secondary structures)
3. Calculates the number of possible secondary structures using Catalan numbers
4. Displays the result with additional information about the calculation

## Example Usage

For input "AUCG":
- Sequence length: 4 nucleotides
- Base pairs: 2
- Number of valid secondary structures: 2nd Catalan number = 2

This solution efficiently handles the mathematical computation while providing a clean, user-friendly interface for the Rosalind problem.

