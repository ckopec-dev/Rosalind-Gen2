# Rosalind Problem: Calculating Protein Mass

## Problem Statement
In this problem, we need to calculate the total mass of a protein string based on the masses of individual amino acids.

## Solution

```javascript
// Lightning Web Component for Calculating Protein Mass
import { LightningElement } from 'lwc';

export default class ProteinMassCalculator extends LightningElement {
    proteinSequence = '';
    result = 0;
    errorMessage = '';

    // Amino acid masses (from Rosalind problem)
    aminoAcidMasses = {
        'A': 71.03711,
        'C': 103.00919,
        'D': 115.02694,
        'E': 129.04259,
        'F': 147.06841,
        'G': 57.02137,
        'H': 137.05891,
        'I': 113.08406,
        'K': 128.09496,
        'L': 113.08406,
        'M': 131.04049,
        'N': 114.04293,
        'P': 97.05276,
        'Q': 128.05858,
        'R': 156.10111,
        'S': 87.03203,
        'T': 101.04768,
        'V': 99.06841,
        'W': 186.07931,
        'Y': 163.06333
    };

    handleInputChange(event) {
        this.proteinSequence = event.target.value;
        this.errorMessage = '';
    }

    calculateMass() {
        if (!this.proteinSequence) {
            this.errorMessage = 'Please enter a protein sequence';
            return;
        }

        // Validate input - check if sequence contains only valid amino acids
        const validSequence = /^[ACDEFGHIKLMNPQRSTVWY]+$/;
        if (!validSequence.test(this.proteinSequence)) {
            this.errorMessage = 'Invalid amino acid sequence. Only standard amino acids are allowed.';
            return;
        }

        let totalMass = 0;
        for (let i = 0; i < this.proteinSequence.length; i++) {
            const aminoAcid = this.proteinSequence[i];
            totalMass += this.aminoAcidMasses[aminoAcid];
        }

        this.result = Math.round(totalMass * 10000) / 10000; // Round to 4 decimal places
    }

    // Alternative implementation using reduce
    calculateMassAlternative() {
        if (!this.proteinSequence) {
            this.errorMessage = 'Please enter a protein sequence';
            return;
        }

        const validSequence = /^[ACDEFGHIKLMNPQRSTVWY]+$/;
        if (!validSequence.test(this.proteinSequence)) {
            this.errorMessage = 'Invalid amino acid sequence. Only standard amino acids are allowed.';
            return;
        }

        this.result = this.proteinSequence
            .split('')
            .reduce((total, aminoAcid) => total + this.aminoAcidMasses[aminoAcid], 0);
        
        this.result = Math.round(this.result * 10000) / 10000;
    }

    // Handle button click
    handleCalculate() {
        this.calculateMass();
    }
}
```

## HTML Template

```html
<template>
    <div class="container">
        <h2>Protein Mass Calculator</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Protein Sequence" 
                value={proteinSequence}
                onchange={handleInputChange}
                placeholder="Enter amino acid sequence (e.g. MKTVRQERLKSIVRILERSKEPVSGAQLAEELSVSRQVIVQDIAYLRSLGYNIVATPRGYVLAGG)">
            </lightning-input>
            
            <lightning-button 
                label="Calculate Mass" 
                onclick={handleCalculate}
                variant="brand">
            </lightning-button>
        </div>

        <div class="result-section">
            <template if:true={errorMessage}>
                <lightning-alert 
                    title="Error" 
                    message={errorMessage} 
                    variant="error">
                </lightning-alert>
            </template>

            <template if:true={result}>
                <div class="result">
                    <p><strong>Total Mass:</strong> {result} g/mol</p>
                </div>
            </template>
        </div>
    </div>
</template>
```

## CSS Styles

```css
.container {
    padding: 20px;
    max-width: 600px;
    margin: 0 auto;
}

.input-section {
    margin-bottom: 20px;
}

.result-section {
    margin-top: 20px;
}

.result {
    background-color: #f0f8ff;
    padding: 15px;
    border-radius: 5px;
    border-left: 4px solid #007bff;
}
```

## Key Features

1. **Input Validation**: Checks for valid amino acid sequence (only standard 20 amino acids)
2. **Mass Calculation**: Uses standard amino acid masses from Rosalind problem
3. **Error Handling**: Provides clear error messages for invalid inputs
4. **Rounding**: Results rounded to 4 decimal places for precision
5. **User-Friendly Interface**: Clean LWC component with proper styling

## Example Usage

For input sequence "SKADYEK":
- S = 87.03203
- K = 128.09496  
- A = 71.03711
- D = 115.02694
- Y = 163.06333
- E = 129.04259
- K = 128.09496

Total = 721.30192 g/mol

The component handles both individual character processing and functional reduce approach for calculating protein mass.

