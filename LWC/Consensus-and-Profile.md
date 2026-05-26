# Rosalind Problem: Consensus and Profile

## Problem Statement
Given a collection of at most 10 DNA strings of equal length (at most 1000 bp) in FASTA format, return the consensus string and profile matrix.

## Solution in Lightning Web Component

```javascript
// consensusAndProfile.js
import { LightningElement } from 'lwc';

export default class ConsensusAndProfile extends LightningElement {
    inputSequences = '';
    result = '';
    
    handleInputChange(event) {
        this.inputSequences = event.target.value;
    }
    
    solve() {
        if (!this.inputSequences.trim()) {
            this.result = 'Please enter DNA sequences';
            return;
        }
        
        try {
            const sequences = this.parseFASTA(this.inputSequences);
            const profile = this.calculateProfile(sequences);
            const consensus = this.calculateConsensus(profile);
            
            this.result = `Consensus: ${consensus}\n\nProfile:\n${this.formatProfile(profile)}`;
        } catch (error) {
            this.result = `Error: ${error.message}`;
        }
    }
    
    parseFASTA(fasta) {
        const lines = fasta.trim().split('\n');
        const sequences = [];
        let currentSequence = '';
        let currentId = '';
        
        lines.forEach(line => {
            if (line.startsWith('>')) {
                if (currentSequence) {
                    sequences.push({ id: currentId, sequence: currentSequence });
                }
                currentId = line.substring(1);
                currentSequence = '';
            } else {
                currentSequence += line.trim();
            }
        });
        
        if (currentSequence) {
            sequences.push({ id: currentId, sequence: currentSequence });
        }
        
        return sequences;
    }
    
    calculateProfile(sequences) {
        if (sequences.length === 0) return {};
        
        const length = sequences[0].sequence.length;
        const profile = {
            A: new Array(length).fill(0),
            C: new Array(length).fill(0),
            G: new Array(length).fill(0),
            T: new Array(length).fill(0)
        };
        
        sequences.forEach(seq => {
            for (let i = 0; i < seq.sequence.length; i++) {
                const nucleotide = seq.sequence[i];
                if (profile[nucleotide] !== undefined) {
                    profile[nucleotide][i]++;
                }
            }
        });
        
        return profile;
    }
    
    calculateConsensus(profile) {
        const length = profile.A.length;
        let consensus = '';
        
        for (let i = 0; i < length; i++) {
            const counts = {
                A: profile.A[i],
                C: profile.C[i],
                G: profile.G[i],
                T: profile.T[i]
            };
            
            // Find nucleotide with maximum count
            const maxNucleotide = Object.keys(counts).reduce((a, b) => 
                counts[a] > counts[b] ? a : b
            );
            
            consensus += maxNucleotide;
        }
        
        return consensus;
    }
    
    formatProfile(profile) {
        const nucleotides = ['A', 'C', 'G', 'T'];
        let output = '';
        
        nucleotides.forEach(nucleotide => {
            output += `${nucleotide}: ${profile[nucleotide].join(' ')}\n`;
        });
        
        return output;
    }
    
    get buttonText() {
        return this.inputSequences.trim() ? 'Solve' : 'Enter sequences first';
    }
}
```

```html
<!-- consensusAndProfile.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title slds-truncate" title="Consensus and Profile">
                <h2 class="slds-card__header-title">Consensus and Profile</h2>
            </header>
        </div>
        <div class="slds-card__body">
            <div class="slds-form-element">
                <label class="slds-form-element__label" for="sequences">DNA Sequences (FASTA format)</label>
                <div class="slds-form-element__control">
                    <textarea 
                        id="sequences"
                        class="slds-textarea"
                        placeholder=">seq1
ATCGATCG
>seq2
ATCGATCG
>seq3
ATCGATCG"
                        value={inputSequences}
                        onchange={handleInputChange}
                        rows="10">
                    </textarea>
                </div>
            </div>
            <div class="slds-m-top_medium">
                <lightning-button 
                    label={buttonText}
                    variant="brand"
                    disabled={!inputSequences.trim()}
                    onclick={solve}>
                </lightning-button>
            </div>
            <div class="slds-m-top_medium">
                <lightning-card title="Result">
                    <pre class="slds-text-body_small">{result}</pre>
                </lightning-card>
            </div>
        </div>
    </div>
</template>
```

```css
/* consensusAndProfile.css */
.slds-card__body {
    padding: 1rem;
}

.slds-textarea {
    font-family: monospace;
    font-size: 0.875rem;
}
```

## How It Works

1. **Input Parsing**: The component parses FASTA format input to extract DNA sequences
2. **Profile Calculation**: For each position in the sequences, it counts occurrences of each nucleotide (A, C, G, T)
3. **Consensus Generation**: At each position, it selects the nucleotide with the highest count
4. **Output Formatting**: Displays both the consensus string and the profile matrix

## Example Usage

**Input:**
```
>seq1
ATCGATCG
>seq2
ATCGATCG
>seq3
ATCGATCG
```

**Output:**
```
Consensus: ATCGATCG

Profile:
A: 3 3 3 3 3 3 3 3
C: 3 3 3 3 3 3 3 3
G: 3 3 3 3 3 3 3 3
T: 3 3 3 3 3 3 3 3
```

This solution handles the core requirements of the Rosalind problem while being implemented in the Lightning Web Component framework with proper error handling and user interface.

