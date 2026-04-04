# Rosalind Problem: Genome Assembly with Perfect Coverage and Repeats

## Problem Understanding

This problem involves genome assembly from reads with perfect coverage and repeats. We need to find the shortest superstring that contains all given reads as substrings.

## Solution Approach

The problem requires finding the shortest superstring that contains all given DNA sequences as substrings. This is essentially finding the shortest superstring that covers all reads with perfect coverage.

## Implementation

```javascript
// Lightning Web Component for Genome Assembly
import { LightningElement } from 'lwc';

export default class GenomeAssembly extends LightningElement {
    inputSequences = '';
    result = '';
    error = '';

    handleInputChange(event) {
        this.inputSequences = event.target.value;
    }

    handleAssemble() {
        try {
            if (!this.inputSequences.trim()) {
                throw new Error('Please enter DNA sequences');
            }

            const sequences = this.parseInput(this.inputSequences);
            if (sequences.length === 0) {
                throw new Error('No valid sequences found');
            }

            const result = this.assembleGenome(sequences);
            this.result = result;
            this.error = '';
        } catch (error) {
            this.error = error.message;
            this.result = '';
        }
    }

    parseInput(input) {
        // Parse input sequences (assuming FASTA format or newline separated)
        const lines = input.trim().split('\n');
        const sequences = [];
        let currentSequence = '';
        
        for (let line of lines) {
            line = line.trim();
            if (line.startsWith('>')) {
                // FASTA header
                if (currentSequence) {
                    sequences.push(currentSequence);
                }
                currentSequence = '';
            } else if (line) {
                currentSequence += line;
            }
        }
        
        if (currentSequence) {
            sequences.push(currentSequence);
        }
        
        return sequences;
    }

    assembleGenome(sequences) {
        // Handle edge cases
        if (sequences.length === 0) return '';
        if (sequences.length === 1) return sequences[0];

        // Find overlap between sequences
        const overlaps = this.calculateOverlaps(sequences);
        
        // Use greedy approach to find shortest superstring
        let result = this.greedySuperstring(sequences, overlaps);
        
        return result;
    }

    calculateOverlaps(sequences) {
        const overlaps = {};
        const n = sequences.length;
        
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                if (i !== j) {
                    const overlap = this.calculateOverlap(sequences[i], sequences[j]);
                    overlaps[`${i}-${j}`] = overlap;
                }
            }
        }
        
        return overlaps;
    }

    calculateOverlap(s1, s2) {
        // Find maximum overlap between s1 (suffix) and s2 (prefix)
        let maxOverlap = 0;
        const minLen = Math.min(s1.length, s2.length);
        
        for (let i = 1; i <= minLen; i++) {
            if (s1.substring(s1.length - i) === s2.substring(0, i)) {
                maxOverlap = i;
            }
        }
        
        return maxOverlap;
    }

    greedySuperstring(sequences, overlaps) {
        // Greedy approach to find shortest superstring
        const n = sequences.length;
        let remaining = [...Array(n).keys()];
        let result = '';
        
        if (remaining.length === 0) return '';
        
        // Start with first sequence
        result = sequences[remaining[0]];
        remaining.shift();
        
        // Continue adding sequences with maximum overlap
        while (remaining.length > 0) {
            let bestIndex = -1;
            let bestOverlap = -1;
            
            for (let i = 0; i < remaining.length; i++) {
                const seqIndex = remaining[i];
                let maxOverlap = -1;
                
                // Check overlap with current result
                for (let j = 0; j < result.length; j++) {
                    const suffix = result.substring(j);
                    if (suffix.length <= sequences[seqIndex].length) {
                        if (sequences[seqIndex].startsWith(suffix)) {
                            maxOverlap = suffix.length;
                            break;
                        }
                    }
                }
                
                if (maxOverlap > bestOverlap) {
                    bestOverlap = maxOverlap;
                    bestIndex = i;
                }
            }
            
            if (bestIndex !== -1) {
                const seqIndex = remaining[bestIndex];
                const sequence = sequences[seqIndex];
                const overlap = bestOverlap;
                
                // Add sequence to result
                if (overlap > 0) {
                    result += sequence.substring(overlap);
                } else {
                    result += sequence;
                }
                
                remaining.splice(bestIndex, 1);
            } else {
                // No overlap found, just append
                if (remaining.length > 0) {
                    result += sequences[remaining[0]];
                    remaining.shift();
                }
            }
        }
        
        return result;
    }

    handleClear() {
        this.inputSequences = '';
        this.result = '';
        this.error = '';
    }

    // Alternative approach for perfect coverage
    findShortestSuperstring(sequences) {
        // For perfect coverage with repeats, we want minimum superstring
        if (sequences.length === 0) return '';
        if (sequences.length === 1) return sequences[0];
        
        // Build a graph where nodes are sequences and edges represent overlaps
        const n = sequences.length;
        const graph = Array(n).fill().map(() => Array(n).fill(0));
        
        // Calculate overlaps
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                if (i !== j) {
                    graph[i][j] = this.calculateOverlap(sequences[i], sequences[j]);
                }
            }
        }
        
        // Find shortest superstring using greedy approach
        return this.greedySuperstring(sequences, graph);
    }

    get isResultAvailable() {
        return this.result && this.result.length > 0;
    }

    get hasError() {
        return this.error && this.error.length > 0;
    }
}
```

## HTML Template

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Genome Assembly with Perfect Coverage and Repeats</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="sequences">DNA Sequences</label>
            <div class="slds-form-element__control">
                <textarea 
                    id="sequences"
                    class="slds-textarea"
                    placeholder="Enter DNA sequences (one per line or FASTA format)"
                    value={inputSequences}
                    onchange={handleInputChange}
                    rows="10">
                </textarea>
            </div>
        </div>

        <div class="slds-button-group" role="group">
            <button 
                class="slds-button slds-button_brand"
                onclick={handleAssemble}>
                Assemble Genome
            </button>
            <button 
                class="slds-button slds-button_outline-brand"
                onclick={handleClear}>
                Clear
            </button>
        </div>

        <template if:true={hasError}>
            <div class="slds-notify slds-notify_alert slds-theme_error" role="alert">
                <span class="slds-assistive-text">Error</span>
                <h2>{error}</h2>
            </div>
        </template>

        <template if:true={isResultAvailable}>
            <div class="slds-box slds-theme_default slds-m-top_medium">
                <h3>Assembly Result</h3>
                <p class="slds-text-body_regular">{result}</p>
                <p class="slds-text-body_small">Length: {result.length} nucleotides</p>
            </div>
        </template>
    </div>
</template>
```

## Usage Example

For input:
```
>seq1
ACGTACGT
>seq2
CGTACGTA
>seq3
GTACGTAC
```

The output would be the shortest superstring containing all three sequences as substrings.

## Key Features

1. **Input Parsing**: Handles both FASTA format and simple newline-separated sequences
2. **Overlap Calculation**: Computes maximum overlaps between sequences
3. **Greedy Assembly**: Uses greedy approach to find shortest superstring
4. **Error Handling**: Proper error handling for edge cases
5. **User Interface**: Clean LWC interface with clear input/output

## Time Complexity

- Overlap calculation: O(n² × m) where n is number of sequences and m is average sequence length
- Superstring construction: O(n²) in worst case
- Overall: O(n² × m) where n is number of sequences and m is average sequence length

This solution efficiently handles perfect coverage scenarios with repeats by finding the minimum length superstring that contains all input sequences as substrings.

