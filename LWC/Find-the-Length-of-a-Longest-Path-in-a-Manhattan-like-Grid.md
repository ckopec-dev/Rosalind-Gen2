# Find the Length of a Longest Path in a Manhattan-like Grid

This problem asks us to find the longest path in a Manhattan-like grid from top-left to bottom-right, where we can only move right or down.

## Problem Understanding

In a Manhattan grid, we start at the top-left corner (0,0) and need to reach the bottom-right corner (m,n), where we can only move:
- Right (increase column index)
- Down (increase row index)

We want to find the maximum sum path from start to end.

## Solution Approach

This is a classic dynamic programming problem. We'll use a 2D DP table where `dp[i][j]` represents the maximum sum to reach cell (i,j) from (0,0).

## Implementation in Lightning Web Component

```javascript
// Find the Length of a Longest Path in a Manhattan-like Grid
// LWC Component: ManhattanPath.js

import { LightningElement } from 'lwc';

export default class ManhattanPath extends LightningElement {
    grid = [];
    result = 0;

    // Function to find the longest path in Manhattan grid
    findLongestPath(grid) {
        if (!grid || grid.length === 0 || grid[0].length === 0) {
            return 0;
        }

        const rows = grid.length;
        const cols = grid[0].length;
        
        // Create DP table
        const dp = Array(rows).fill().map(() => Array(cols).fill(0));
        
        // Initialize starting point
        dp[0][0] = grid[0][0];
        
        // Fill first row (can only come from left)
        for (let j = 1; j < cols; j++) {
            dp[0][j] = dp[0][j-1] + grid[0][j];
        }
        
        // Fill first column (can only come from above)
        for (let i = 1; i < rows; i++) {
            dp[i][0] = dp[i-1][0] + grid[i][0];
        }
        
        // Fill the rest of the table
        for (let i = 1; i < rows; i++) {
            for (let j = 1; j < cols; j++) {
                dp[i][j] = Math.max(dp[i-1][j], dp[i][j-1]) + grid[i][j];
            }
        }
        
        return dp[rows-1][cols-1];
    }

    // Example usage
    handleCalculate() {
        // Example grid from Rosalind problem
        this.grid = [
            [1, 3, 1, 5],
            [2, 1, 4, 4],
            [5, 2, 3, 1],
            [1, 5, 2, 3]
        ];
        
        this.result = this.findLongestPath(this.grid);
        console.log('Longest path sum:', this.result);
    }

    // Alternative implementation with path tracking
    findLongestPathWithPath(grid) {
        if (!grid || grid.length === 0 || grid[0].length === 0) {
            return { maxSum: 0, path: [] };
        }

        const rows = grid.length;
        const cols = grid[0].length;
        const dp = Array(rows).fill().map(() => Array(cols).fill(0));
        const parent = Array(rows).fill().map(() => Array(cols).fill(''));

        // Initialize
        dp[0][0] = grid[0][0];

        // Fill first row
        for (let j = 1; j < cols; j++) {
            dp[0][j] = dp[0][j-1] + grid[0][j];
            parent[0][j] = 'left';
        }

        // Fill first column
        for (let i = 1; i < rows; i++) {
            dp[i][0] = dp[i-1][0] + grid[i][0];
            parent[i][0] = 'up';
        }

        // Fill the rest
        for (let i = 1; i < rows; i++) {
            for (let j = 1; j < cols; j++) {
                if (dp[i-1][j] > dp[i][j-1]) {
                    dp[i][j] = dp[i-1][j] + grid[i][j];
                    parent[i][j] = 'up';
                } else {
                    dp[i][j] = dp[i][j-1] + grid[i][j];
                    parent[i][j] = 'left';
                }
            }
        }

        // Reconstruct path
        const path = [];
        let i = rows - 1;
        let j = cols - 1;
        
        while (i > 0 || j > 0) {
            path.unshift([i, j]);
            if (parent[i][j] === 'up') {
                i--;
            } else {
                j--;
            }
        }
        path.unshift([0, 0]);

        return {
            maxSum: dp[rows-1][cols-1],
            path: path
        };
    }
}
```

## HTML Template

```html
<!-- ManhattanPath.html -->
<template>
    <div class="container">
        <h2>Manhattan Grid Longest Path</h2>
        <lightning-button 
            label="Calculate Longest Path" 
            onclick={handleCalculate}
            variant="brand">
        </lightning-button>
        <div class="result">
            <p>Maximum path sum: {result}</p>
        </div>
    </div>
</template>
```

## CSS Styling

```css
/* ManhattanPath.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.result {
    margin-top: 20px;
    padding: 15px;
    background-color: #f0f8ff;
    border-radius: 5px;
    font-weight: bold;
}
```

## Algorithm Explanation

1. **Dynamic Programming Approach**: 
   - Create a DP table where `dp[i][j]` represents the maximum sum to reach position (i,j)
   - Initialize the first row and column since there's only one way to reach those positions
   - For each remaining cell, take the maximum of coming from above or left, plus current cell value

2. **Time Complexity**: O(m×n) where m and n are grid dimensions
3. **Space Complexity**: O(m×n) for the DP table

## Example Walkthrough

For grid:
```
1 3 1 5
2 1 4 4
5 2 3 1
1 5 2 3
```

The longest path sum would be: 1→2→5→5→2→3→3 = 21

This solution efficiently computes the maximum sum path in a Manhattan grid using dynamic programming principles.

