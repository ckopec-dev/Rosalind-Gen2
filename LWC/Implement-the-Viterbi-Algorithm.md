# Rosalind Problem: Implement the Viterbi Algorithm

## Problem Description
The Viterbi algorithm is used to find the most likely sequence of hidden states in a Hidden Markov Model (HMM). Given an HMM with states, emissions, and transition/emit probabilities, we need to find the optimal hidden path that maximizes the probability of the observed sequence.

## Solution in Lightning Web Component

```javascript
// viterbiAlgorithm.js
import { LightningElement } from 'lwc';

export default class ViterbiAlgorithm extends LightningElement {
    // Input data
    sequence = '';
    states = [];
    transitionMatrix = {};
    emissionMatrix = {};
    
    // Results
    optimalPath = '';
    probability = 0;

    handleInput(event) {
        const inputType = event.target.name;
        const value = event.target.value;
        
        if (inputType === 'sequence') {
            this.sequence = value;
        } else if (inputType === 'states') {
            this.states = value.split(' ').filter(s => s.trim() !== '');
        } else if (inputType === 'transition') {
            this.parseTransitionMatrix(value);
        } else if (inputType === 'emission') {
            this.parseEmissionMatrix(value);
        }
    }

    parseTransitionMatrix(input) {
        // Parse transition matrix from input
        const lines = input.trim().split('\n');
        this.transitionMatrix = {};
        
        // First line contains state names
        const stateNames = lines[0].trim().split(/\s+/);
        
        for (let i = 1; i < lines.length; i++) {
            const row = lines[i].trim().split(/\s+/);
            const state = row[0];
            this.transitionMatrix[state] = {};
            
            for (let j = 1; j < row.length; j++) {
                this.transitionMatrix[state][stateNames[j-1]] = parseFloat(row[j]);
            }
        }
    }

    parseEmissionMatrix(input) {
        // Parse emission matrix from input
        const lines = input.trim().split('\n');
        this.emissionMatrix = {};
        
        // First line contains state names
        const stateNames = lines[0].trim().split(/\s+/);
        
        for (let i = 1; i < lines.length; i++) {
            const row = lines[i].trim().split(/\s+/);
            const state = row[0];
            this.emissionMatrix[state] = {};
            
            for (let j = 1; j < row.length; j++) {
                this.emissionMatrix[state][stateNames[j-1]] = parseFloat(row[j]);
            }
        }
    }

    viterbi() {
        const sequence = this.sequence;
        const states = this.states;
        
        if (!sequence || !states.length) {
            return;
        }
        
        // Initialize the Viterbi table
        const viterbi = {};
        const backtrack = {};
        
        // Initialize first column
        for (const state of states) {
            viterbi[state] = {};
            backtrack[state] = {};
            
            // Probability of first emission given state
            const firstEmission = sequence[0];
            const emissionProb = this.emissionMatrix[state][firstEmission] || 0;
            
            // Assuming uniform initial probabilities
            viterbi[state][0] = emissionProb * (1 / states.length);
            backtrack[state][0] = '';
        }
        
        // Fill the Viterbi table
        for (let t = 1; t < sequence.length; t++) {
            const currentEmission = sequence[t];
            
            for (const currentState of states) {
                let maxProb = 0;
                let maxPrevState = '';
                
                // Find the previous state that maximizes probability
                for (const prevState of states) {
                    const transitionProb = this.transitionMatrix[prevState][currentState] || 0;
                    const prevProb = viterbi[prevState][t-1];
                    const prob = prevProb * transitionProb;
                    
                    if (prob > maxProb) {
                        maxProb = prob;
                        maxPrevState = prevState;
                    }
                }
                
                // Emission probability
                const emissionProb = this.emissionMatrix[currentState][currentEmission] || 0;
                viterbi[currentState][t] = maxProb * emissionProb;
                backtrack[currentState][t] = maxPrevState;
            }
        }
        
        // Find the most probable final state
        let maxFinalProb = 0;
        let finalState = '';
        
        for (const state of states) {
            const prob = viterbi[state][sequence.length - 1];
            if (prob > maxFinalProb) {
                maxFinalProb = prob;
                finalState = state;
            }
        }
        
        // Backtrack to find the optimal path
        let path = finalState;
        let currentState = finalState;
        let t = sequence.length - 1;
        
        while (t > 0) {
            currentState = backtrack[currentState][t];
            path = currentState + path;
            t--;
        }
        
        this.optimalPath = path;
        this.probability = maxFinalProb;
    }

    handleSolve() {
        this.viterbi();
    }

    get result() {
        return `Optimal path: ${this.optimalPath}\nProbability: ${this.probability.toFixed(10)}`;
    }
}
```

```html
<!-- viterbiAlgorithm.html -->
<template>
    <div class="container">
        <h2>Viterbi Algorithm Implementation</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Sequence" 
                name="sequence"
                value={sequence}
                onchange={handleInput}
                variant="standard"
                type="text">
            </lightning-input>
            
            <lightning-input 
                label="States (space separated)" 
                name="states"
                value={states}
                onchange={handleInput}
                variant="standard"
                type="text">
            </lightning-input>
            
            <lightning-textarea 
                label="Transition Matrix" 
                name="transition"
                value={transitionMatrixInput}
                onchange={handleInput}
                variant="standard"
                placeholder="Enter transition matrix (first row: state names, subsequent rows: transition probabilities)">
            </lightning-textarea>
            
            <lightning-textarea 
                label="Emission Matrix" 
                name="emission"
                value={emissionMatrixInput}
                onchange={handleInput}
                variant="standard"
                placeholder="Enter emission matrix (first row: state names, subsequent rows: emission probabilities)">
            </lightning-textarea>
            
            <lightning-button 
                label="Solve" 
                onclick={handleSolve}
                variant="brand">
            </lightning-button>
        </div>
        
        <div class="result-section">
            <h3>Results:</h3>
            <lightning-textarea 
                value={result}
                variant="readonly"
                label="Optimal Path and Probability">
            </lightning-textarea>
        </div>
    </div>
</template>
```

```css
/* viterbiAlgorithm.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
    padding: 15px;
    border: 1px solid #e5e5e5;
    border-radius: 5px;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    border: 1px solid #e5e5e5;
    border-radius: 5px;
}

.lightning-button {
    margin-top: 10px;
}
```

## How the Algorithm Works

1. **Initialization**: Create a Viterbi table where each cell represents the maximum probability of reaching a state at a given time.

2. **Forward Pass**: For each position in the sequence and each state, calculate the maximum probability of reaching that state by considering all possible previous states.

3. **Backtracking**: Once the optimal path is determined, backtrack through the table to reconstruct the most likely sequence of hidden states.

## Key Features

- **Input Parsing**: Handles sequence, states, transition matrix, and emission matrix inputs
- **Viterbi Implementation**: Uses dynamic programming to find the optimal path
- **Backtracking**: Reconstructs the actual optimal state sequence
- **User Interface**: Clean LWC interface for input and output

## Time Complexity
- **Time**: O(N × M²) where N is the sequence length and M is the number of states
- **Space**: O(N × M) for the Viterbi table

This implementation follows the standard Viterbi algorithm for Hidden Markov Models and can be easily integrated into Salesforce Lightning applications.

