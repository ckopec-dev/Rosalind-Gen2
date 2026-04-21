# Rosalind Problem: Assessing Assembly Quality with N50 and N75

## Problem Understanding

The N50 and N75 statistics are used to assess the quality of genome assemblies. They represent the contig length at which 50% and 75% of the total assembly length is contained in contigs of that length or longer.

## Solution Approach

1. Read contig lengths from input
2. Sort contigs in descending order
3. Calculate cumulative sum of lengths
4. Find the length where cumulative sum reaches 50% and 75% of total
5. Return N50 and N75 values

## Implementation

```javascript
// Lightning Web Component for N50 and N75 calculation
import { LightningElement } from 'lwc';

export default class AssemblyQuality extends LightningElement {
    contigLengths = [];
    n50 = 0;
    n75 = 0;
    totalLength = 0;

    handleInput(event) {
        const input = event.target.value;
        this.contigLengths = input
            .split('\n')
            .filter(line => line.trim() !== '')
            .map(line => parseInt(line.trim()))
            .filter(num => !isNaN(num) && num > 0);
        
        this.calculateN50AndN75();
    }

    calculateN50AndN75() {
        if (this.contigLengths.length === 0) {
            this.n50 = 0;
            this.n75 = 0;
            return;
        }

        // Sort in descending order
        this.contigLengths.sort((a, b) => b - a);
        
        // Calculate total length
        this.totalLength = this.contigLengths.reduce((sum, length) => sum + length, 0);
        
        // Calculate N50
        const n50Target = this.totalLength / 2;
        let cumulativeSum = 0;
        for (let i = 0; i < this.contigLengths.length; i++) {
            cumulativeSum += this.contigLengths[i];
            if (cumulativeSum >= n50Target) {
                this.n50 = this.contigLengths[i];
                break;
            }
        }
        
        // Calculate N75
        const n75Target = this.totalLength * 0.75;
        cumulativeSum = 0;
        for (let i = 0; i < this.contigLengths.length; i++) {
            cumulativeSum += this.contigLengths[i];
            if (cumulativeSum >= n75Target) {
                this.n75 = this.contigLengths[i];
                break;
            }
        }
    }

    get results() {
        return `N50: ${this.n50}, N75: ${this.n75}`;
    }
}
```

## HTML Template

```html
<template>
    <div class="container">
        <h2>Assembly Quality Assessment (N50 & N75)</h2>
        
        <div class="input-section">
            <label for="contigInput">Enter contig lengths (one per line):</label>
            <textarea 
                id="contigInput" 
                value={inputValue} 
                onchange={handleInput}
                placeholder="Enter contig lengths here (e.g., 1000, 500, 2000, ...)"
                rows="10"
                cols="50">
            </textarea>
        </div>

        <div class="results-section">
            <p>Total Assembly Length: {totalLength}</p>
            <p>Results: {results}</p>
        </div>
    </div>
</template>
```

## CSS Styling

```css
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
}

.input-section label {
    display: block;
    margin-bottom: 10px;
    font-weight: bold;
}

.input-section textarea {
    width: 100%;
    padding: 10px;
    border: 1px solid #ccc;
    border-radius: 4px;
    font-family: monospace;
}

.results-section {
    background-color: #f5f5f5;
    padding: 15px;
    border-radius: 4px;
    border-left: 4px solid #007bff;
}

.results-section p {
    margin: 5px 0;
}
```

## Example Usage

**Input:**
```
1000
500
2000
1500
3000
```

**Expected Output:**
```
N50: 2000
N75: 1500
```

## Explanation

1. **Input Processing**: The component accepts contig lengths as newline-separated values
2. **Sorting**: Contigs are sorted in descending order of length
3. **N50 Calculation**: Find the smallest contig length where cumulative sum reaches 50% of total assembly
4. **N75 Calculation**: Find the smallest contig length where cumulative sum reaches 75% of total assembly
5. **Output**: Displays both N50 and N75 values

The algorithm efficiently computes these statistics by sorting once and then traversing the sorted array to find the required thresholds.

