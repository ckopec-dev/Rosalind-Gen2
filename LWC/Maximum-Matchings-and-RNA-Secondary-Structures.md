# Rosalind Problem: Maximum Matchings and RNA Secondary Structures

## Problem Understanding

This problem asks us to find the maximum number of base pairings in an RNA sequence that can form valid secondary structures, where:
- A pairs with U
- G pairs with C
- No base can pair with itself
- Base pairs cannot cross (no pseudoknots)

## Solution Approach

The key insight is to use dynamic programming with the following recurrence:
- For any position i, we can either:
  1. Not pair it with anything (dp[i] = dp[i+1])
  2. Pair it with a valid complement at position j (dp[i] = dp[i+1] + dp[j+1] + 1)

## Implementation

```javascript
// Lightning Web Component for Maximum Matchings and RNA Secondary Structures
import { LightningElement } from 'lwc';

export default class MaximumMatchings extends LightningElement {
    // RNA sequence input
    sequence = '';
    
    // Handle input change
    handleSequenceChange(event) {
        this.sequence = event.target.value.toUpperCase();
    }
    
    // Calculate maximum matchings
    calculateMaxMatchings() {
        if (!this.sequence) return 0;
        
        // Convert sequence to array for easier handling
        const rna = this.sequence.split('');
        const n = rna.length;
        
        // Memoization table
        const memo = new Array(n).fill(-1);
        
        // Helper function to check if two bases can pair
        function canPair(b1, b2) {
            return (b1 === 'A' && b2 === 'U') || 
                   (b1 === 'U' && b2 === 'A') ||
                   (b1 === 'G' && b2 === 'C') ||
                   (b1 === 'C' && b2 === 'G');
        }
        
        // Dynamic programming function
        function dp(i) {
            if (i >= n) return 1;
            if (memo[i] !== -1) return memo[i];
            
            // Option 1: Don't pair current base
            let max = dp(i + 1);
            
            // Option 2: Try pairing current base with each valid complement
            for (let j = i + 1; j < n; j++) {
                if (canPair(rna[i], rna[j])) {
                    // Pair i with j, then solve for remaining
                    max = Math.max(max, dp(i + 1) * dp(j + 1) + 1);
                }
            }
            
            memo[i] = max;
            return max;
        }
        
        // Return result
        return dp(0);
    }
    
    // Alternative approach using proper dynamic programming
    calculateMaxMatchingsOptimized() {
        if (!this.sequence) return 0;
        
        const rna = this.sequence.split('');
        const n = rna.length;
        
        // dp[i][j] represents maximum matchings for substring from i to j
        const dp = Array(n).fill().map(() => Array(n).fill(0));
        
        // Helper function to check if two bases can pair
        function canPair(b1, b2) {
            return (b1 === 'A' && b2 === 'U') || 
                   (b1 === 'U' && b2 === 'A') ||
                   (b1 === 'G' && b2 === 'C') ||
                   (b1 === 'C' && b2 === 'G');
        }
        
        // Fill DP table
        for (let l = 1; l <= n; l++) {
            for (let i = 0; i <= n - l; i++) {
                const j = i + l - 1;
                
                if (l === 1) {
                    dp[i][j] = 0; // Single base cannot form a pair
                } else {
                    // Option 1: Don't pair first base
                    dp[i][j] = dp[i + 1][j];
                    
                    // Option 2: Try pairing first base with each valid complement
                    for (let k = i + 1; k <= j; k++) {
                        if (canPair(rna[i], rna[k])) {
                            // Pair i with k
                            let val = 1; // This pair
                            if (i + 1 <= k - 1) {
                                val += dp[i + 1][k - 1]; // Matchings in middle
                            }
                            if (k + 1 <= j) {
                                val += dp[k + 1][j]; // Matchings in right part
                            }
                            dp[i][j] = Math.max(dp[i][j], val);
                        }
                    }
                }
            }
        }
        
        return dp[0][n - 1];
    }
    
    // Handle calculation button click
    handleCalculate() {
        const result = this.calculateMaxMatchingsOptimized();
        this.dispatchEvent(new CustomEvent('result', {
            detail: { result: result }
        }));
    }
    
    // Template for input
    render() {
        return `
            <div class="container">
                <h2>Maximum Matchings and RNA Secondary Structures</h2>
                <div class="input-section">
                    <label for="sequence">RNA Sequence:</label>
                    <input id="sequence" type="text" value={sequence} onchange={handleSequenceChange} />
                    <button onclick={handleCalculate}>Calculate</button>
                </div>
                <div class="output-section">
                    <p>Maximum matchings: <span id="result"></span></p>
                </div>
            </div>
        `;
    }
}
```

## Example Usage

For the input RNA sequence "AUGCUA":
- Valid pairings: A-U, U-A, G-C, C-G
- Maximum matchings would be calculated using the dynamic programming approach
- The result represents the maximum number of non-crossing base pairs possible

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the RNA sequence
- **Space Complexity**: O(n²) for the dynamic programming table

## Key Insights

1. This is a classic dynamic programming problem similar to the "maximum number of non-crossing pairs" problem
2. The recurrence relation considers both the case where we don't pair a base and where we pair it with a valid complement
3. The constraint of no pseudoknots (non-crossing pairs) is crucial for the dynamic programming approach to work correctly

The solution handles all valid base pairing combinations (A-U, U-A, G-C, C-G) while ensuring no base pairs cross each other in the final structure.

