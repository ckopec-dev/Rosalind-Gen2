# Rosalind Problem: Fibonacci Numbers

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of the Fibonacci sequence at position n.

## Solution in Lightning Web Component (JavaScript)

```javascript
// Fibonacci_Numbers.js
import { LightningElement } from 'lwc';

export default class FibonacciNumbers extends LightningElement {
    n = 10; // Default input value
    result = 0;

    handleInputChange(event) {
        this.n = parseInt(event.target.value);
        this.calculateFibonacci();
    }

    calculateFibonacci() {
        if (this.n <= 0) {
            this.result = 0;
            return;
        }
        
        if (this.n === 1) {
            this.result = 1;
            return;
        }
        
        if (this.n === 2) {
            this.result = 1;
            return;
        }

        let a = 1; // F(1)
        let b = 1; // F(2)
        let fib = 0;

        for (let i = 3; i <= this.n; i++) {
            fib = a + b;
            a = b;
            b = fib;
        }

        this.result = fib;
    }

    connectedCallback() {
        this.calculateFibonacci();
    }
}
```

```html
<!-- Fibonacci_Numbers.html -->
<template>
    <div class="container">
        <h2>Fibonacci Numbers</h2>
        <p>Enter a positive integer n (≤ 25):</p>
        <lightning-input 
            type="number" 
            label="n" 
            value={n}
            min="1"
            max="25"
            onchange={handleInputChange}>
        </lightning-input>
        <p class="result">F({n}) = {result}</p>
    </div>
</template>
```

```css
/* Fibonacci_Numbers.css */
.container {
    padding: 20px;
    max-width: 400px;
    margin: 0 auto;
}

.result {
    font-size: 1.2em;
    font-weight: bold;
    margin-top: 20px;
    padding: 10px;
    background-color: #f0f8ff;
    border-radius: 5px;
}
```

## Explanation

This solution implements the Fibonacci sequence calculation in Lightning Web Component:

1. **Input Handling**: Uses `lightning-input` to accept user input with validation (1-25 range)
2. **Fibonacci Calculation**: 
   - Uses iterative approach for efficiency
   - Handles base cases (n=1, n=2) directly
   - For n>2, calculates iteratively using two variables to track previous values
3. **Real-time Updates**: Updates result automatically when input changes
4. **Edge Cases**: Properly handles invalid inputs and boundary conditions

## Sample Input/Output
- Input: n = 5
- Output: F(5) = 5
- Input: n = 10  
- Output: F(10) = 55

The algorithm has O(n) time complexity and O(1) space complexity, making it efficient for the given constraints (n ≤ 25).

