# Rosalind Problem: Introduction to Pattern Matching

## Problem Statement
Given two strings `s` and `t`, find all positions where `t` occurs as a substring in `s`.

## Solution in Lightning Web Component

```javascript
// patternMatching.js
import { LightningElement } from 'lwc';

export default class PatternMatching extends LightningElement {
    inputString = '';
    pattern = '';
    result = '';
    
    handleStringChange(event) {
        this.inputString = event.target.value;
    }
    
    handlePatternChange(event) {
        this.pattern = event.target.value;
    }
    
    findPatternPositions() {
        if (!this.inputString || !this.pattern) {
            this.result = 'Please enter both strings';
            return;
        }
        
        const positions = [];
        let startIndex = 0;
        
        // Find all occurrences of pattern in string
        while (startIndex <= this.inputString.length - this.pattern.length) {
            const foundIndex = this.inputString.indexOf(this.pattern, startIndex);
            
            if (foundIndex === -1) {
                break;
            }
            
            // Convert to 1-based indexing as required by Rosalind
            positions.push(foundIndex + 1);
            startIndex = foundIndex + 1;
        }
        
        if (positions.length === 0) {
            this.result = 'Pattern not found';
        } else {
            this.result = positions.join(' ');
        }
    }
    
    handleFindPattern() {
        this.findPatternPositions();
    }
}
```

```html
<!-- patternMatching.html -->
<template>
    <div class="container">
        <h2>Pattern Matching</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Input String (s)" 
                value={inputString}
                onchange={handleStringChange}
                type="text"
                placeholder="Enter the main string">
            </lightning-input>
            
            <lightning-input 
                label="Pattern (t)" 
                value={pattern}
                onchange={handlePatternChange}
                type="text"
                placeholder="Enter the pattern to search">
            </lightning-input>
            
            <lightning-button 
                label="Find Pattern Positions" 
                onclick={handleFindPattern}
                variant="brand">
            </lightning-button>
        </div>
        
        <div class="result-section">
            <h3>Result:</h3>
            <p>{result}</p>
        </div>
    </div>
</template>
```

```css
/* patternMatching.css */
.container {
    max-width: 800px;
    margin: 20px auto;
    padding: 20px;
    border: 1px solid #e5e5e5;
    border-radius: 8px;
}

.input-section {
    margin-bottom: 20px;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #f8f9fa;
    border-radius: 4px;
}

.result-section h3 {
    margin-top: 0;
    color: #333;
}
```

## Example Usage

**Input:**
- String (s): `GATATATGCATATACTT`
- Pattern (t): `ATAT`

**Output:**
```
2 4 10
```

## Explanation

1. **Problem Understanding**: We need to find all starting positions (1-indexed) where the pattern occurs in the main string
2. **Algorithm**: 
   - Use `indexOf()` method to find pattern occurrences
   - Convert from 0-based to 1-based indexing
   - Continue searching from the next position after each match
3. **Edge Cases Handled**:
   - Empty input strings
   - Pattern not found
   - Multiple occurrences

## Time Complexity
- **Time**: O(n × m) where n is the length of the main string and m is the length of the pattern
- **Space**: O(k) where k is the number of matches found

## Key Features
- User-friendly LWC component with input fields
- Real-time pattern matching
- Proper 1-based indexing as required by Rosalind
- Error handling for edge cases
- Clean, responsive UI using Lightning Design System

This solution efficiently finds all pattern occurrences and formats the output as required for Rosalind problems.

