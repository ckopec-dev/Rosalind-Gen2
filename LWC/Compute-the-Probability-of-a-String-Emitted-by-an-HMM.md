# Rosalind Problem: Compute the Probability of a String Emitted by an HMM

## Problem Understanding

We need to compute the probability of a string being emitted by a Hidden Markov Model (HMM). This is typically solved using the forward algorithm.

## Solution Approach

I'll implement the forward algorithm to calculate the probability of observing a sequence given an HMM.

```javascript
// Lightning Web Component for computing probability of string emitted by HMM

import { LightningElement } from 'lwc';

export default class HMMProbabilityCalculator extends LightningElement {
    // Input data
    hmmInput = '';
    sequence = '';
    
    // Results
    probability = 0;
    isCalculated = false;
    
    // Handle input change
    handleHMMInputChange(event) {
        this.hmmInput = event.target.value;
    }
    
    handleSequenceChange(event) {
        this.sequence = event.target.value;
    }
    
    // Main calculation method
    calculateProbability() {
        if (!this.hmmInput || !this.sequence) {
            return;
        }
        
        try {
            // Parse HMM input
            const hmmData = this.parseHMMInput(this.hmmInput);
            
            // Calculate probability using forward algorithm
            const prob = this.forwardAlgorithm(hmmData, this.sequence);
            
            this.probability = prob;
            this.isCalculated = true;
        } catch (error) {
            console.error('Error calculating probability:', error);
        }
    }
    
    // Parse HMM input format
    parseHMMInput(input) {
        const lines = input.trim().split('\n');
        const hmm = {};
        
        // Parse states
        hmm.states = lines[0].trim().split(/\s+/);
        
        // Parse alphabet
        hmm.alphabet = lines[2].trim().split(/\s+/);
        
        // Parse emission probabilities
        hmm.emission = {};
        const emissionLines = lines.slice(4, 4 + hmm.states.length);
        hmm.states.forEach((state, i) => {
            hmm.emission[state] = {};
            const probs = emissionLines[i].trim().split(/\s+/);
            hmm.alphabet.forEach((symbol, j) => {
                hmm.emission[state][symbol] = parseFloat(probs[j]);
            });
        });
        
        // Parse transition probabilities
        hmm.transition = {};
        const transitionLines = lines.slice(4 + hmm.states.length + 1);
        hmm.states.forEach((state, i) => {
            hmm.transition[state] = {};
            const probs = transitionLines[i].trim().split(/\s+/);
            hmm.states.forEach((targetState, j) => {
                hmm.transition[state][targetState] = parseFloat(probs[j]);
            });
        });
        
        // Parse initial probabilities
        hmm.initial = {};
        const initialProbs = lines[1].trim().split(/\s+/);
        hmm.states.forEach((state, i) => {
            hmm.initial[state] = parseFloat(initialProbs[i]);
        });
        
        return hmm;
    }
    
    // Forward algorithm implementation
    forwardAlgorithm(hmm, sequence) {
        const n = hmm.states.length;
        const m = sequence.length;
        
        // Create forward matrix
        const forward = Array(m).fill().map(() => ({}));
        
        // Initialize first row
        const firstChar = sequence[0];
        hmm.states.forEach(state => {
            forward[0][state] = hmm.initial[state] * hmm.emission[state][firstChar];
        });
        
        // Fill forward matrix
        for (let t = 1; t < m; t++) {
            const char = sequence[t];
            hmm.states.forEach(targetState => {
                forward[t][targetState] = 0;
                hmm.states.forEach(sourceState => {
                    forward[t][targetState] += forward[t-1][sourceState] * hmm.transition[sourceState][targetState];
                });
                forward[t][targetState] *= hmm.emission[targetState][char];
            });
        }
        
        // Sum probabilities for final states
        let totalProbability = 0;
        hmm.states.forEach(state => {
            totalProbability += forward[m-1][state];
        });
        
        return totalProbability;
    }
    
    // Handle form submission
    handleCalculate() {
        this.calculateProbability();
    }
    
    // Reset form
    handleReset() {
        this.hmmInput = '';
        this.sequence = '';
        this.probability = 0;
        this.isCalculated = false;
    }
    
    // Sample input format for testing
    get sampleInput() {
        return `A B
A 0.5 0.5
B 0.5 0.5
A B
A 0.3 0.7
B 0.7 0.3
A 0.5 0.5
A 0.5 0.5
B 0.5 0.5`;
    }
    
    get sampleSequence() {
        return 'AAB';
    }
}
```

## HTML Template

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Compute Probability of String Emitted by HMM</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">HMM Input (lines format)</label>
            <textarea 
                class="slds-textarea"
                value={hmmInput}
                onchange={handleHMMInputChange}
                placeholder="Enter HMM data in the required format..."
                rows="10">
            </textarea>
        </div>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">Sequence to Compute</label>
            <input 
                type="text" 
                class="slds-input"
                value={sequence}
                onchange={handleSequenceChange}
                placeholder="Enter sequence (e.g., AAB)">
        </div>
        
        <div class="slds-form-element">
            <lightning-button 
                label="Calculate Probability" 
                variant="brand" 
                onclick={handleCalculate}>
            </lightning-button>
            <lightning-button 
                label="Reset" 
                variant="neutral" 
                onclick={handleReset}
                style="margin-left: 10px;">
            </lightning-button>
        </div>
        
        <template if:true={isCalculated}>
            <div class="slds-box slds-theme_info" style="margin-top: 20px;">
                <h3>Result</h3>
                <p>Probability: <strong>{probability}</strong></p>
            </div>
        </template>
        
        <div class="slds-box slds-theme_shade" style="margin-top: 20px;">
            <h3>Sample Input Format</h3>
            <p>States: A B</p>
            <p>Initial probabilities: A 0.5 B 0.5</p>
            <p>Emission probabilities:</p>
            <p>&nbsp;&nbsp;&nbsp;&nbsp;A 0.3 0.7</p>
            <p>&nbsp;&nbsp;&nbsp;&nbsp;B 0.7 0.3</p>
            <p>Transition probabilities:</p>
            <p>&nbsp;&nbsp;&nbsp;&nbsp;A 0.5 0.5</p>
            <p>&nbsp;&nbsp;&nbsp;&nbsp;B 0.5 0.5</p>
            <p>Sequence: AAB</p>
        </div>
    </div>
</template>
```

## Key Features

1. **Forward Algorithm Implementation**: Uses dynamic programming to compute the probability
2. **Input Parsing**: Parses HMM data in standard Rosalind format
3. **Error Handling**: Graceful error handling for malformed input
4. **User Interface**: Clean LWC interface for input and result display
5. **Sample Data**: Includes sample input format for testing

## Algorithm Complexity

- **Time Complexity**: O(m × n²) where m is sequence length and n is number of states
- **Space Complexity**: O(m × n) for the forward matrix

This implementation efficiently computes the probability of a given string being emitted by a Hidden Markov Model using the forward algorithm, which is the standard approach for this type of problem.

