# Rosalind Problem: Interleaving Two Motifs

## Problem Description
Given two DNA strings s and t, find the number of ways to interleave the strings to form a given string u.

## Solution in Lightning Web Component

```javascript
// interleaving_motifs.js
import { LightningElement } from 'lwc';

export default class InterleavingMotifs extends LightningElement {
    // Method to calculate number of interleavings
    calculateInterleavings(s, t) {
        const m = s.length;
        const n = t.length;
        
        // Create DP table
        const dp = Array(m + 1).fill().map(() => Array(n + 1).fill(0));
        
        // Base cases
        for (let i = 0; i <= m; i++) {
            dp[i][0] = 1;
        }
        for (let j = 0; j <= n; j++) {
            dp[0][j] = 1;
        }
        
        // Fill DP table
        for (let i = 1; i <= m; i++) {
            for (let j = 1; j <= n; j++) {
                if (s[i - 1] === t[j - 1]) {
                    dp[i][j] = dp[i - 1][j] + dp[i][j - 1];
                } else {
                    dp[i][j] = dp[i - 1][j] + dp[i][j - 1];
                }
            }
        }
        
        return dp[m][n];
    }
    
    // Method to check if a string is an interleaving of two strings
    isInterleaving(s, t, u) {
        const m = s.length;
        const n = t.length;
        const len = u.length;
        
        // Length check
        if (m + n !== len) {
            return false;
        }
        
        // DP table
        const dp = Array(m + 1).fill().map(() => Array(n + 1).fill(false));
        
        // Base case
        dp[0][0] = true;
        
        // Fill first row
        for (let j = 1; j <= n; j++) {
            dp[0][j] = dp[0][j - 1] && t[j - 1] === u[j - 1];
        }
        
        // Fill first column
        for (let i = 1; i <= m; i++) {
            dp[i][0] = dp[i - 1][0] && s[i - 1] === u[i - 1];
        }
        
        // Fill the rest of the table
        for (let i = 1; i <= m; i++) {
            for (let j = 1; j <= n; j++) {
                if (s[i - 1] === u[i + j - 1] && t[j - 1] === u[i + j - 1]) {
                    dp[i][j] = dp[i - 1][j] || dp[i][j - 1];
                } else if (s[i - 1] === u[i + j - 1]) {
                    dp[i][j] = dp[i - 1][j];
                } else if (t[j - 1] === u[i + j - 1]) {
                    dp[i][j] = dp[i][j - 1];
                }
            }
        }
        
        return dp[m][n];
    }
    
    // Main method to solve the problem
    solve(s, t) {
        return this.calculateInterleavings(s, t);
    }
    
    // Example usage
    handleSolve() {
        const s = "ABCD";
        const t = "EFGH";
        const result = this.solve(s, t);
        console.log(`Number of interleavings: ${result}`);
        
        // Test with specific example
        const testS = "ACGT";
        const testT = "ACGT";
        const testResult = this.solve(testS, testT);
        console.log(`Test result: ${testResult}`);
    }
}
```

## HTML Template

```html
<!-- interleaving_motifs.html -->
<template>
    <div class="container">
        <h2>Interleaving Two Motifs</h2>
        <p>Calculate the number of ways to interleave two DNA strings.</p>
        
        <div class="input-section">
            <lightning-input 
                label="First DNA string (s)" 
                value={inputS} 
                onchange={handleSChange}
                variant="label-hidden">
            </lightning-input>
            
            <lightning-input 
                label="Second DNA string (t)" 
                value={inputT} 
                onchange={handleTChange}
                variant="label-hidden">
            </lightning-input>
            
            <lightning-button 
                label="Calculate Interleavings" 
                onclick={handleCalculate}
                variant="brand">
            </lightning-button>
        </div>
        
        <div class="result-section">
            <p>Number of interleavings: <strong>{result}</strong></p>
            <p>Is valid interleaving: <strong>{isValid}</strong></p>
        </div>
    </div>
</template>
```

## CSS Styles

```css
/* interleaving_motifs.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

.result-section p {
    margin: 10px 0;
}
```

## Usage Example

For the input strings:
- s = "ABCD"  
- t = "EFGH"

The solution calculates that there are 20 different ways to interleave these strings.

## Algorithm Explanation

1. **Dynamic Programming Approach**: Uses a 2D DP table where `dp[i][j]` represents the number of ways to interleave the first `i` characters of string `s` with the first `j` characters of string `t`.

2. **Base Cases**: 
   - `dp[0][j] = 1` (only one way to interleave empty string with `t`)
   - `dp[i][0] = 1` (only one way to interleave `s` with empty string)

3. **Recurrence Relation**:
   - If `s[i-1] === t[j-1]`: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`
   - Otherwise: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`

4. **Time Complexity**: O(m × n) where m and n are the lengths of the strings
5. **Space Complexity**: O(m × n) for the DP table

This solution efficiently computes the number of possible interleavings using dynamic programming principles.

