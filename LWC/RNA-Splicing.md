# Rosalind Problem: RNA Splicing (Lightning Web Component)

## Problem Statement
Given: A DNA string s of length at most 1 kbp and a collection of substrings of s acting as introns. Remove the introns and concatenate the remaining outsequences to form a new string.

## Solution in Lightning Web Component

```javascript
// RNA_Splicing.js
import { LightningElement } from 'lwc';

export default class RNA_Splicing extends LightningElement {
    dnaString = '';
    introns = [];
    result = '';

    handleDnaChange(event) {
        this.dnaString = event.target.value;
    }

    handleIntronChange(event) {
        const index = event.target.dataset.index;
        this.introns[index] = event.target.value;
    }

    addIntron() {
        this.introns = [...this.introns, ''];
    }

    removeIntron(index) {
        this.introns.splice(index, 1);
    }

    spliceRNA() {
        if (!this.dnaString) {
            this.result = 'Please enter a DNA string';
            return;
        }

        let result = this.dnaString;
        
        // Remove each intron from the DNA string
        for (const intron of this.introns) {
            if (intron) {
                result = result.replace(new RegExp(intron, 'g'), '');
            }
        }

        // Convert DNA to RNA (replace T with U)
        const rna = result.replace(/T/g, 'U');
        
        this.result = rna;
    }

    // Alternative implementation using string methods
    spliceRNAAlternative() {
        if (!this.dnaString) {
            this.result = 'Please enter a DNA string';
            return;
        }

        let sequence = this.dnaString;
        
        // Remove all introns
        for (const intron of this.introns) {
            if (intron) {
                sequence = sequence.split(intron).join('');
            }
        }

        // Convert to RNA
        const rna = sequence.replace(/T/g, 'U');
        
        this.result = rna;
    }
}
```

```html
<!-- RNA_Splicing.html -->
<template>
    <div class="container">
        <h2>RNA Splicing</h2>
        
        <div class="input-section">
            <label for="dna">DNA String:</label>
            <input 
                id="dna" 
                type="text" 
                value={dnaString} 
                onchange={handleDnaChange}
                placeholder="Enter DNA sequence (e.g. ATGGTCTACATAGCTCCTGCGG GGCATACCCCGTAGGACCCCTCCC CGGGATCATATCCGGAGAGCATCTCAGAAAGGGGCGCGCGGGGCGGCGGCGCGGCGCGGCGCGGCGGCGCGCGCGCGGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCG

