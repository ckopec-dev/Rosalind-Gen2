# Rosalind Problem: Linguistic Complexity of a Genome

## Problem Statement
The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings of a given length to the total number of possible substrings of that length. For a genome of length n and substring length k, the total number of possible substrings is n-k+1, and the number of distinct substrings is the count of unique substrings of length k.

## Solution in Lightning Web Component

```javascript
// linguisticComplexity.js
import { LightningElement } from 'lwc';

export default class LinguisticComplexity extends LightningElement {
    genome = '';
    k = 3;
    result = null;
    error = '';

    handleGenomeChange(event) {
        this.genome = event.target.value;
        this.error = '';
    }

    handleKChange(event) {
        this.k = parseInt(event.target.value);
        this.error = '';
    }

    calculateComplexity() {
        try {
            if (!this.genome) {
                throw new Error('Please enter a genome sequence');
            }
            
            if (this.k <= 0) {
                throw new Error('k must be a positive integer');
            }
            
            if (this.k > this.genome.length) {
                throw new Error('k cannot be greater than genome length');
            }

            const complexity = this.computeLinguisticComplexity(this.genome, this.k);
            this.result = complexity;
            this.error = '';
        } catch (error) {
            this.error = error.message;
            this.result = null;
        }
    }

    computeLinguisticComplexity(genome, k) {
        const n = genome.length;
        const totalSubstrings = n - k + 1;
        
        // Get all substrings of length k
        const substrings = [];
        for (let i = 0; i <= n - k; i++) {
            substrings.push(genome.substring(i, i + k));
        }
        
        // Count distinct substrings
        const distinctSubstrings = new Set(substrings);
        const distinctCount = distinctSubstrings.size;
        
        // Calculate linguistic complexity
        const complexity = distinctCount / totalSubstrings;
        
        return {
            totalSubstrings: totalSubstrings,
            distinctSubstrings: distinctCount,
            complexity: complexity
        };
    }

    get complexityResult() {
        if (this.result) {
            return `
                Total substrings of length ${this.k}: ${this.result.totalSubstrings}
                Distinct substrings of length ${this.k}: ${this.result.distinctSubstrings}
                Linguistic complexity: ${this.result.complexity.toFixed(4)}
            `;
        }
        return '';
    }
}
```

```html
<!-- linguisticComplexity.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Linguistic Complexity of a Genome</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="genome">Genome Sequence</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    id="genome" 
                    class="slds-input"
                    value={genome}
                    onchange={handleGenomeChange}
                    placeholder="Enter genome sequence (e.g. ATCGATCG)"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label" for="k">Substring Length (k)</label>
            <div class="slds-form-element__control">
                <input 
                    type="number" 
                    id="k" 
                    class="slds-input"
                    value={k}
                    onchange={handleKChange}
                    min="1"
                />
            </div>
        </div>

        <lightning-button 
            label="Calculate Complexity" 
            variant="brand" 
            onclick={calculateComplexity}
            class="slds-m-top_medium"
        ></lightning-button>

        <template if:true={error}>
            <div class="slds-notify slds-notify_alert slds-theme_error" role="alert">
                <span class="slds-assistive-text">Error</span>
                {error}
            </div>
        </template>

        <template if:true={result}>
            <div class="slds-box slds-theme_info slds-m-top_medium">
                <h3>Results</h3>
                <p><strong>Total substrings of length {k}:</strong> {result.totalSubstrings}</p>
                <p><strong>Distinct substrings of length {k}:</strong> {result.distinctSubstrings}</p>
                <p><strong>Linguistic complexity:</strong> {result.complexity}</p>
            </div>
        </template>
    </div>
</template>
```

```css
/* linguisticComplexity.css */
.slds-box {
    padding: 1rem;
}

.slds-form-element {
    margin-bottom: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 500px;
}
```

## How it works:

1. **Input Handling**: The component accepts a genome sequence and substring length k
2. **Validation**: Checks that inputs are valid (non-empty genome, positive k, k ≤ genome length)
3. **Substring Extraction**: Extracts all substrings of length k from the genome
4. **Distinct Counting**: Uses a Set to count unique substrings
5. **Complexity Calculation**: Computes the ratio of distinct substrings to total possible substrings
6. **Display**: Shows detailed results including total substrings, distinct substrings, and the linguistic complexity value

## Example Usage:
For genome "ATCGATCG" with k=3:
- Total substrings of length 3: 6
- Distinct substrings: {ATC, TCG, CGA, GAT, ATC, TCG} = 3 distinct
- Linguistic complexity: 3/6 = 0.5

The solution handles edge cases and provides clear error messages for invalid inputs.

