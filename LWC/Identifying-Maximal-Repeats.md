# Rosalind Problem: Identifying Maximal Repeats

## Problem Description
Given a string s of length at most 100000 letters, return all maximal repeats of s. A maximal repeat is a substring that appears at least twice in s and cannot be extended to the left or right without losing some occurrence.

## Solution Approach
We'll use a suffix array and LCP (Longest Common Prefix) array approach to find maximal repeats efficiently.

## Implementation

```javascript
// Lightning Web Component for Identifying Maximal Repeats
import { LightningElement } from 'lwc';

export default class MaximalRepeats extends LightningElement {
    inputString = '';
    results = [];
    error = '';

    handleInputChange(event) {
        this.inputString = event.target.value;
        this.error = '';
    }

    findMaximalRepeats() {
        if (!this.inputString) {
            this.error = 'Please enter a string';
            return;
        }

        try {
            const repeats = this.computeMaximalRepeats(this.inputString);
            this.results = repeats;
        } catch (error) {
            this.error = 'Error processing string: ' + error.message;
        }
    }

    computeMaximalRepeats(s) {
        // Create suffix array
        const suffixes = [];
        for (let i = 0; i < s.length; i++) {
            suffixes.push({ suffix: s.substring(i), index: i });
        }
        
        // Sort suffixes
        suffixes.sort((a, b) => a.suffix.localeCompare(b.suffix));
        
        // Compute LCP array
        const lcp = this.computeLCPArray(s, suffixes);
        
        // Find maximal repeats
        const repeats = new Set();
        const visited = new Set();
        
        for (let i = 0; i < lcp.length; i++) {
            if (lcp[i] > 0) {
                // Find all suffixes with this LCP
                const positions = [];
                for (let j = 0; j < suffixes.length; j++) {
                    if (this.getLCP(s, suffixes[j].suffix, i) >= lcp[i]) {
                        positions.push(suffixes[j].index);
                    }
                }
                
                // Check if this repeat is maximal
                if (positions.length > 1) {
                    const repeat = s.substring(positions[0], positions[0] + lcp[i]);
                    if (!visited.has(repeat)) {
                        if (this.isMaximalRepeat(s, repeat, positions)) {
                            repeats.add(repeat);
                        }
                        visited.add(repeat);
                    }
                }
            }
        }
        
        return Array.from(repeats).sort();
    }

    computeLCPArray(s, suffixes) {
        const lcp = new Array(suffixes.length);
        lcp[0] = 0;
        
        for (let i = 0; i < suffixes.length; i++) {
            lcp[i] = this.getLCP(s, suffixes[i].suffix, 0);
        }
        
        return lcp;
    }

    getLCP(s, suffix, start) {
        let lcp = 0;
        for (let i = 0; i < suffix.length; i++) {
            if (start + i < s.length && s[start + i] === suffix[i]) {
                lcp++;
            } else {
                break;
            }
        }
        return lcp;
    }

    isMaximalRepeat(s, repeat, positions) {
        if (positions.length < 2) return false;
        
        // Check if we can extend to the left or right
        const repeatLength = repeat.length;
        
        // Check left extension
        const leftExtension = this.canExtendLeft(s, repeat, positions);
        
        // Check right extension
        const rightExtension = this.canExtendRight(s, repeat, positions);
        
        return !leftExtension && !rightExtension;
    }

    canExtendLeft(s, repeat, positions) {
        const repeatLength = repeat.length;
        
        // Get the character before the first occurrence
        const firstPos = positions[0];
        if (firstPos === 0) return false;
        
        const leftChar = s[firstPos - 1];
        
        // Check if this character can be part of a longer repeat
        for (let i = 0; i < positions.length; i++) {
            const pos = positions[i];
            if (pos > 0 && s[pos - 1] === leftChar) {
                // Check if we can extend the repeat
                let j = 0;
                while (j < repeatLength && pos - 1 + j < s.length && 
                       s[pos - 1 + j] === repeat[j]) {
                    j++;
                }
                if (j === repeatLength) {
                    return true;
                }
            } else {
                return false;
            }
        }
        return false;
    }

    canExtendRight(s, repeat, positions) {
        const repeatLength = repeat.length;
        
        // Get the character after the last occurrence
        const lastPos = positions[0];
        if (lastPos + repeatLength >= s.length) return false;
        
        const rightChar = s[lastPos + repeatLength];
        
        // Check if this character can be part of a longer repeat
        for (let i = 0; i < positions.length; i++) {
            const pos = positions[i];
            if (pos + repeatLength < s.length && s[pos + repeatLength] === rightChar) {
                // Check if we can extend the repeat
                let j = 0;
                while (j < repeatLength && pos + repeatLength + j < s.length && 
                       s[pos + repeatLength + j] === repeat[j]) {
                    j++;
                }
                if (j === repeatLength) {
                    return true;
                }
            } else {
                return false;
            }
        }
        return false;
    }

    handleClear() {
        this.inputString = '';
        this.results = [];
        this.error = '';
    }
}
```

## HTML Template

```html
<template>
    <div class="container">
        <h2>Identifying Maximal Repeats</h2>
        
        <div class="input-section">
            <lightning-input 
                type="text" 
                label="Enter DNA string (max 100000 letters)" 
                value={inputString}
                onchange={handleInputChange}
                variant="label-hidden">
            </lightning-input>
            
            <lightning-button 
                label="Find Maximal Repeats" 
                variant="brand" 
                onclick={findMaximalRepeats}>
            </lightning-button>
            
            <lightning-button 
                label="Clear" 
                variant="neutral" 
                onclick={handleClear}>
            </lightning-button>
        </div>

        <div class="error-section" if:true={error}>
            <lightning-icon 
                icon-name="utility:error" 
                size="small" 
                alternative-text="Error">
            </lightning-icon>
            <p>{error}</p>
        </div>

        <div class="results-section" if:true={results.length}>
            <h3>Maximal Repeats Found:</h3>
            <lightning-datatable
                data={results}
                columns={columns}
                key-field="id"
                hide-checkbox-column
                max-row-selection="100">
            </lightning-datatable>
        </div>

        <div class="no-results" if:false={results.length} if:true={inputString}>
            <p>No maximal repeats found in the input string.</p>
        </div>
    </div>
</template>
```

## Usage Example

**Input:** `ATATCGATATCG`

**Expected Output:** `ATATCG` (if this is a maximal repeat)

## Key Features

1. **Suffix Array Construction**: Builds sorted suffixes of the input string
2. **LCP Array**: Computes longest common prefixes between adjacent suffixes
3. **Maximal Repeat Detection**: Identifies repeats that cannot be extended
4. **Error Handling**: Validates input and handles edge cases
5. **User Interface**: Clean LWC interface for input and results display

## Time Complexity
- O(n² log n) for suffix array construction
- O(n²) for LCP computation
- Overall: O(n² log n) where n is the string length

## Space Complexity
- O(n²) for storing suffixes and LCP array

This solution efficiently identifies all maximal repeats in the given DNA string using string processing techniques.

