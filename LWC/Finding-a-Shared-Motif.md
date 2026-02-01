# Finding a Shared Motif - Rosalind Problem Solution

## Problem Understanding

The problem asks us to find the longest common substring among all given DNA sequences. This is a classic bioinformatics problem where we need to identify the longest contiguous sequence that appears in every input string.

## Approach

1. **Find all substrings of the shortest sequence**
2. **Check if each substring exists in all other sequences**
3. **Return the longest such substring**

## Solution in Lightning Web Component

```javascript
// Finding_a_Shared_Motif.js
import { LightningElement } from 'lwc';

export default class Finding_a_Shared_Motif extends LightningElement {
    // Sample input data
    inputSequences = [
        "CAGTATGGAATTCATCG",
        "CCTGAGTATGGAATTCATCG",
        "GATTCATCGAGTATGGAATTCATCG"
    ];

    longestCommonSubstring = '';
    
    connectedCallback() {
        this.findSharedMotif();
    }

    findSharedMotif() {
        if (this.inputSequences.length === 0) {
            return '';
        }

        // Find the shortest sequence to optimize search
        const shortestSeq = this.inputSequences.reduce((shortest, current) => 
            current.length < shortest.length ? current : shortest
        );

        let longestMotif = '';

        // Generate all substrings of the shortest sequence
        for (let i = 0; i < shortestSeq.length; i++) {
            for (let j = i + 1; j <= shortestSeq.length; j++) {
                const substring = shortestSeq.substring(i, j);
                
                // Check if this substring exists in all sequences
                if (this.isSubstringInAll(substring)) {
                    // Update longest motif if current is longer
                    if (substring.length > longestMotif.length) {
                        longestMotif = substring;
                    }
                }
            }
        }

        this.longestCommonSubstring = longestMotif;
        console.log('Longest shared motif:', longestMotif);
        return longestMotif;
    }

    isSubstringInAll(substring) {
        return this.inputSequences.every(seq => 
            seq.includes(substring)
        );
    }

    // Alternative optimized approach using binary search
    findSharedMotifOptimized() {
        if (this.inputSequences.length === 0) {
            return '';
        }

        const shortestSeq = this.inputSequences.reduce((shortest, current) => 
            current.length < shortest.length ? current : shortest
        );

        let left = 0;
        let right = shortestSeq.length;
        let result = '';

        while (left <= right) {
            const mid = Math.floor((left + right) / 2);
            const motif = this.findMotifOfLength(shortestSeq, mid);
            
            if (motif) {
                result = motif;
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }

        this.longestCommonSubstring = result;
        return result;
    }

    findMotifOfLength(sequence, length) {
        const seen = new Set();
        
        // Generate all substrings of given length from first sequence
        for (let i = 0; i <= sequence.length - length; i++) {
            const substring = sequence.substring(i, i + length);
            seen.add(substring);
        }

        // Check if any of these substrings exist in all sequences
        for (const motif of seen) {
            if (this.isSubstringInAll(motif)) {
                return motif;
            }
        }

        return null;
    }

    // Get the result for display
    getResult() {
        return this.longestCommonSubstring;
    }
}
```

## HTML Template

```html
<!-- Finding_a_Shared_Motif.html -->
<template>
    <div class="container">
        <h2>Finding a Shared Motif</h2>
        <p>Input Sequences:</p>
        <ul>
            <template for:each={inputSequences} for:item="sequence">
                <li key={sequence}>{sequence}</li>
            </template>
        </ul>
        <p>Longest Common Motif: {longestCommonSubstring}</p>
        <lightning-button 
            label="Find Motif" 
            onclick={findSharedMotif}
            variant="brand">
        </lightning-button>
    </div>
</template>
```

## CSS Styling

```css
/* Finding_a_Shared_Motif.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.container h2 {
    color: #0070d2;
    text-align: center;
}

.container ul {
    background-color: #f4f6f9;
    padding: 15px;
    border-radius: 5px;
    margin: 15px 0;
}

.container li {
    margin: 5px 0;
    font-family: monospace;
    background-color: #e1f5fe;
    padding: 5px;
    border-radius: 3px;
}

.container p {
    font-size: 16px;
    line-height: 1.5;
}
```

## Explanation

### Key Algorithm Steps:

1. **Find Shortest Sequence**: Start with the shortest sequence to minimize substring generation
2. **Generate Substrings**: Create all possible substrings from the shortest sequence
3. **Validate Against All Sequences**: Check if each substring exists in every input sequence
4. **Track Longest**: Keep track of the longest valid substring found

### Time Complexity:
- **Basic Approach**: O(n × m² × k) where n is the number of sequences, m is the length of the shortest sequence, and k is the average length of substrings
- **Optimized Approach**: O(n × m² × log m) using binary search

### Space Complexity: O(m²) for storing substrings

## Example Usage

For input sequences:
- "CAGTATGGAATTCATCG"
- "CCTGAGTATGGAATTCATCG" 
- "GATTCATCGAGTATGGAATTCATCG"

The longest common motif would be: **"ATCG"**

This solution handles the core bioinformatics problem of finding shared motifs among DNA sequences efficiently within the Lightning Web Component framework.

