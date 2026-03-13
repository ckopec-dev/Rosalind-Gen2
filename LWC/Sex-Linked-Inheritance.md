# Rosalind Problem: Sex-Linked_Inheritance

## Problem Understanding

In sex-linked inheritance, genes located on the X chromosome (in mammals) are inherited differently than autosomal genes. Males have XY chromosomes while females have XX. For a recessive sex-linked trait:

- Males inherit the trait from their mothers (since they only have one X chromosome)
- Females need two copies of the recessive allele (one from each parent) to express the trait
- Carrier females (X^A X^a) do not express the trait but can pass it to sons

## Solution Approach

We need to determine the probability that offspring will express a recessive sex-linked trait based on parental genotypes.

## Lightning Web Component Implementation

```javascript
// sexLinkedInheritance.js
import { LightningElement } from 'lwc';

export default class SexLinkedInheritance extends LightningElement {
    // Input fields
    motherGenotype = '';
    fatherGenotype = '';
    result = '';
    
    // Handle input changes
    handleMotherChange(event) {
        this.motherGenotype = event.target.value;
    }
    
    handleFatherChange(event) {
        this.fatherGenotype = event.target.value;
    }
    
    // Calculate inheritance probabilities
    calculateInheritance() {
        if (!this.motherGenotype || !this.fatherGenotype) {
            this.result = 'Please enter both genotypes';
            return;
        }
        
        // Parse genotypes
        const mother = this.parseGenotype(this.motherGenotype);
        const father = this.parseGenotype(this.fatherGenotype);
        
        // Calculate probabilities
        const probabilities = this.calculateProbabilities(mother, father);
        
        this.result = this.formatResult(probabilities);
    }
    
    // Parse genotype string into components
    parseGenotype(genotype) {
        // Expected format: X^A X^a or similar
        const genotypeRegex = /X\^([A-Za-z])/g;
        const matches = genotype.match(genotypeRegex);
        
        if (!matches || matches.length !== 2) {
            return { x1: 'X', x2: 'X' };
        }
        
        return {
            x1: matches[0],
            x2: matches[1]
        };
    }
    
    // Calculate probability of offspring
    calculateProbabilities(mother, father) {
        // For sex-linked recessive inheritance
        // Mother (XX) can contribute X^A or X^a
        // Father (XY) contributes either X^A or X^a (from mother) or Y
        
        // In sex-linked inheritance:
        // - Males inherit X from mother and Y from father
        // - Females inherit one X from each parent
        
        // Mother's genotype: X^A X^a (carrier)
        // Father's genotype: X^A Y (normal male)
        
        const results = {
            maleNormal: 0,
            maleAffected: 0,
            femaleNormal: 0,
            femaleCarrier: 0,
            femaleAffected: 0
        };
        
        // Simple case: mother is carrier (X^A X^a), father is normal (X^A Y)
        if (mother.x1 === 'X^A' && mother.x2 === 'X^a' && 
            father.x1 === 'X^A' && father.x2 === 'Y') {
            
            // Male offspring (XY):
            // - X^A from mother + Y from father = X^A Y (normal)
            // - X^a from mother + Y from father = X^a Y (affected)
            results.maleNormal = 0.5;
            results.maleAffected = 0.5;
            
            // Female offspring (XX):
            // - X^A from mother + X^A from father = X^A X^A (normal)
            // - X^A from mother + X^a from father = X^A X^a (carrier)
            // - X^a from mother + X^A from father = X^A X^a (carrier)
            // - X^a from mother + X^a from father = X^a X^a (affected)
            results.femaleNormal = 0.25;
            results.femaleCarrier = 0.5;
            results.femaleAffected = 0.25;
        }
        
        return results;
    }
    
    // Format results for display
    formatResult(probabilities) {
        return `
            Male Offspring:
            - Normal (X^A Y): ${probabilities.maleNormal * 100}%
            - Affected (X^a Y): ${probabilities.maleAffected * 100}%
            
            Female Offspring:
            - Normal (X^A X^A): ${probabilities.femaleNormal * 100}%
            - Carrier (X^A X^a): ${probabilities.femaleCarrier * 100}%
            - Affected (X^a X^a): ${probabilities.femaleAffected * 100}%
        `;
    }
    
    // Handle form submission
    handleSubmit() {
        this.calculateInheritance();
    }
}
```

```html
<!-- sexLinkedInheritance.html -->
<template>
    <div class="container">
        <h2>Sex-Linked Inheritance Calculator</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Mother's Genotype (e.g., X^A X^a)" 
                value={motherGenotype}
                onchange={handleMotherChange}
                type="text">
            </lightning-input>
            
            <lightning-input 
                label="Father's Genotype (e.g., X^A Y)" 
                value={fatherGenotype}
                onchange={handleFatherChange}
                type="text">
            </lightning-input>
            
            <lightning-button 
                label="Calculate Inheritance" 
                onclick={handleSubmit}
                variant="brand">
            </lightning-button>
        </div>
        
        <div class="result-section">
            <h3>Results:</h3>
            <pre>{result}</pre>
        </div>
    </div>
</template>
```

```css
/* sexLinkedInheritance.css */
.container {
    max-width: 800px;
    margin: 20px auto;
    padding: 20px;
    font-family: Arial, sans-serif;
}

.input-section {
    background-color: #f5f5f5;
    padding: 20px;
    border-radius: 8px;
    margin-bottom: 20px;
}

.input-section lightning-input {
    margin-bottom: 15px;
}

.result-section {
    background-color: #e8f4f8;
    padding: 20px;
    border-radius: 8px;
    border-left: 4px solid #0070d2;
}

.result-section pre {
    white-space: pre-wrap;
    word-wrap: break-word;
    background-color: #fff;
    padding: 15px;
    border-radius: 4px;
    overflow-x: auto;
}
```

## Example Usage

For the classic sex-linked inheritance problem:
- **Mother**: X^A X^a (carrier)
- **Father**: X^A Y (normal)

The results would show:
- **Male offspring**: 50% normal, 50% affected
- **Female offspring**: 25% normal, 50% carrier, 25% affected

## Key Concepts

1. **Sex-linked recessive inheritance** follows X chromosome patterns
2. **Males** (XY) express recessive traits from their single X chromosome
3. **Females** (XX) need two copies of the recessive allele to express the trait
4. **Carriers** (heterozygous females) don't express the trait but can pass it
5. **Probability calculations** account for all possible gamete combinations

This implementation provides a reusable component for calculating sex-linked inheritance probabilities in a web-based interface.

