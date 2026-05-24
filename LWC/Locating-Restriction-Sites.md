# Rosalind Problem: Locating Restriction Sites

## Problem Statement
A restriction site is a short DNA sequence that is recognized by a restriction enzyme. Given a DNA string, find all locations where a palindromic restriction site occurs.

## Solution in Lightning Web Component

```javascript
// locRestrictionSites.js
import { LightningElement } from 'lwc';

export default class LocRestrictionSites extends LightningElement {
    dnaSequence = '';
    restrictionSites = [];
    
    handleInputChange(event) {
        this.dnaSequence = event.target.value;
        this.findRestrictionSites();
    }
    
    findRestrictionSites() {
        if (!this.dnaSequence) {
            this.restrictionSites = [];
            return;
        }
        
        // Common restriction sites and their recognition sequences
        const restrictionSites = [
            { name: 'EcoRI', sequence: 'GAATTC', length: 6 },
            { name: 'BamHI', sequence: 'GGATCC', length: 6 },
            { name: 'HindIII', sequence: 'AAGCTT', length: 6 },
            { name: 'XbaI', sequence: 'TCTAGA', length: 6 },
            { name: 'NotI', sequence: 'GCGGCCGC', length: 8 },
            { name: 'BglII', sequence: 'AGATCT', length: 6 }
        ];
        
        this.restrictionSites = [];
        
        // Find all occurrences of each restriction site
        restrictionSites.forEach(site => {
            const positions = this.findPattern(this.dnaSequence, site.sequence);
            if (positions.length > 0) {
                this.restrictionSites.push({
                    name: site.name,
                    sequence: site.sequence,
                    positions: positions,
                    length: site.length
                });
            }
        });
    }
    
    findPattern(dna, pattern) {
        const positions = [];
        let pos = 0;
        
        while (pos < dna.length) {
            const foundPos = dna.indexOf(pattern, pos);
            if (foundPos === -1) break;
            positions.push(foundPos + 1); // 1-based indexing
            pos = foundPos + 1;
        }
        
        return positions;
    }
    
    get dnaLength() {
        return this.dnaSequence.length;
    }
    
    get hasResults() {
        return this.restrictionSites.length > 0;
    }
}
```

```html
<!-- locRestrictionSites.html -->
<template>
    <div class="slds-box slds-theme_default">
        <h2>Restriction Site Finder</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="dnaSequence">
                DNA Sequence
            </label>
            <div class="slds-form-element__control">
                <textarea 
                    id="dnaSequence"
                    class="slds-textarea"
                    placeholder="Enter DNA sequence (e.g., GAATTCGGATCC)"
                    value={dnaSequence}
                    onchange={handleInputChange}
                    rows="4">
                </textarea>
            </div>
        </div>
        
        <template if:true={dnaSequence}>
            <div class="slds-box slds-theme_default slds-m-top_medium">
                <h3>Results</h3>
                <p>Sequence length: {dnaLength} nucleotides</p>
                
                <template if:true={hasResults}>
                    <template for:each={restrictionSites} for:item="site">
                        <div key={site.name} class="slds-m-bottom_medium">
                            <h4>{site.name} ({site.sequence})</h4>
                            <p>Length: {site.length} nucleotides</p>
                            <p>Positions: {site.positions.join(', ')}</p>
                        </div>
                    </template>
                </template>
                
                <template if:false={hasResults}>
                    <p>No restriction sites found in the sequence.</p>
                </template>
            </div>
        </template>
    </div>
</template>
```

```css
/* locRestrictionSites.css */
.slds-box {
    padding: 1rem;
}

.slds-textarea {
    width: 100%;
    font-family: monospace;
}
```

## Key Features

1. **Input Handling**: Accepts DNA sequence input with real-time analysis
2. **Pattern Matching**: Searches for common restriction enzyme recognition sites
3. **Position Tracking**: Returns 1-based positions of all matches
4. **Multiple Sites**: Handles multiple restriction sites in the same sequence
5. **Responsive UI**: Clean, user-friendly interface using SLDS components

## Common Restriction Sites

The solution looks for these common restriction sites:
- **EcoRI**: GAATTC (6 bp)
- **BamHI**: GGATCC (6 bp) 
- **HindIII**: AAGCTT (6 bp)
- **XbaI**: TCTAGA (6 bp)
- **NotI**: GCGGCCGC (8 bp)
- **BglII**: AGATCT (6 bp)

## Usage

1. Enter a DNA sequence in the text area
2. The component automatically analyzes the sequence
3. Results show all found restriction sites with their positions
4. Uses 1-based indexing for positions (standard in molecular biology)

This implementation provides a complete, reusable component for finding restriction sites in DNA sequences.

