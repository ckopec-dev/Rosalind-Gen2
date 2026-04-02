# Rosalind Problem: Inferring Protein from Spectrum

## Problem Description
Given a list of masses representing a peptide spectrum, determine the protein sequence that could have produced this spectrum.

## Solution Approach
We need to:
1. Create a mass-to-amino acid mapping
2. Find the protein sequence by matching the spectrum masses to amino acid masses
3. Handle the fact that we need to account for the water molecule (18.01056 Da) that is lost during peptide formation

## Implementation in Lightning Web Component

```javascript
// inferring_protein_from_spectrum.js

import { LightningElement } from 'lwc';

export default class InferringProteinFromSpectrum extends LightningElement {
    spectrumInput = '';
    result = '';
    
    // Amino acid masses (with water molecule subtracted)
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
    
    // Reverse mapping: mass to amino acid
    massToAminoAcid = {};
    
    connectedCallback() {
        // Initialize reverse mapping
        Object.keys(this.aminoAcidMasses).forEach(aa => {
            this.massToAminoAcid[this.aminoAcidMasses[aa]] = aa;
        });
    }
    
    handleInputChange(event) {
        this.spectrumInput = event.target.value;
    }
    
    solveProblem() {
        if (!this.spectrumInput.trim()) {
            this.result = 'Please enter spectrum masses';
            return;
        }
        
        try {
            // Parse the spectrum input
            const spectrum = this.spectrumInput.trim()
                .split(/\s+/)
                .map(Number)
                .sort((a, b) => a - b);
            
            // The first mass should be 0 (empty peptide)
            if (spectrum[0] !== 0) {
                this.result = 'Invalid spectrum: first mass should be 0';
                return;
            }
            
            // Remove the first mass (0) and process
            const masses = spectrum.slice(1);
            
            // Find the protein sequence
            const protein = this.findProteinSequence(masses);
            
            this.result = protein;
        } catch (error) {
            this.result = 'Error processing spectrum: ' + error.message;
        }
    }
    
    findProteinSequence(masses) {
        // Create a list of possible amino acid sequences
        const sequence = [];
        
        // Process masses to find amino acids
        for (let i = 0; i < masses.length; i++) {
            const mass = masses[i];
            
            // Find the closest amino acid mass
            let closestMass = null;
            let closestAminoAcid = null;
            let minDiff = Infinity;
            
            Object.keys(this.aminoAcidMasses).forEach(aa => {
                const diff = Math.abs(mass - this.aminoAcidMasses[aa]);
                if (diff < minDiff) {
                    minDiff = diff;
                    closestMass = this.aminoAcidMasses[aa];
                    closestAminoAcid = aa;
                }
            });
            
            // Check if the mass is close enough to a known amino acid
            if (minDiff < 0.01) { // Tolerance for floating point comparison
                sequence.push(closestAminoAcid);
            } else {
                throw new Error(`No matching amino acid found for mass ${mass}`);
            }
        }
        
        return sequence.join('');
    }
    
    handleSolve() {
        this.solveProblem();
    }
    
    // Example usage
    handleExample() {
        this.spectrumInput = "0 113.08406 128.09496 147.06841 163.06333 186.07931 194.08388 227.10354 257.12201 271.10354 289.10824 301.10708 317.12201 347.14048 365.13518 383.14048 395.13932 425.15779 455.17626 473.17096 487.15249 501.13396 531.15243 545.13396 575.15243 589.13396 619.15243 633.13396 663.15243 677.13396 707.15243 721.13396 751.15243 765.13396 795.15243 809.13396 839.15243 853.13396 883.15243 897.13396 927.15243 941.13396 971.15243 985.13396 1015.15243 1029.13396 1059.15243 1073.13396 1103.15243 1117.13396 1147.15243 1161.13396 1191.15243 1205.13396 1235.15243 1249.13396 1279.15243 1293.13396 1323.15243 1337.13396 1367.15243 1381.13396 1411.15243 1425.13396 1455.15243 1469.13396 1499.15243 1513.13396 1543.15243 1557.13396 1587.15243 1601.13396 1631.15243 1645.13396 1675.15243 1689.13396 1719.15243 1733.13396 1763.15243 1777.13396 1807.15243 1821.13396 1851.15243 1865.13396 1895.15243 1909.13396 1939.15243 1953.13396 1983.15243 1997.13396 2027.15243 2041.13396 2071.15243 2085.13396 2115.15243 2129.13396 2159.15243 2173.13396 2203.15243 2217.13396 2247.15243 2261.13396 2291.15243 2305.13396 2335.15243 2349.13396 2379.15243 2393.13396 2423.15243 2437.13396 2467.15243 2481.13396 2511.15243 2525.13396 2555.15243 2569.13396 2599.15243 2613.13396 2643.15243 2657.13396 2687.15243 2701.13396 2731.15243 2745.13396 2775.15243 2789.13396 2819.15243 2833.13396 2863.15243 2877.13396 2907.15243 2921.13396 2951.15243 2965.13396 2995.15243 3009.13396 3039.15243 3053.13396 3083.15243 3097.13396 3127.15243 3141.13396 3171.15243 3185.13396 3215.15243 3229.13396 3259.15243 3273.13396 3303.15243 3317.13396 3347.15243 3361.13396 3391.15243 3405.13396 3435.15243 3449.13396 3479.15243 3493.13396 3523.15243 3537.13396 3567.15243 3581.13396 3611.15243 3625.13396 3655.15243 3669.13396 3699.15243 3713.13396 3743.15243 3757.13396 3787.15243 3801.13396 3831.15243 3845.13396 3875.15243 3889.13396 3919.15243 3933.13396 3963.15243 3977.13396 4007.15243 4021.13396 4051.15243 4065.13396 4095.15243 4109.13396 4139.15243 4153.13396 4183.15243 4197.13396 4227.15243 4241.13396 4271.15243 4285.13396 4315.15243 4329.13396 4359.15243 4373.13396 4403.15243 4417.13396 4447.15243 4461.13396 4491.15243 4505.13396 4535.15243 4549.13396 4579.15243 4593.13396 4623.15243 4637.13396 4667.15243 4681.13396 4711.15243 4725.13396 4755.15243 4769.13396 4799.15243 4813.13396 4843.15243 4857.13396 4887.15243 4901.13396 4931.15243 4945.13396 4975.15243 4989.13396 5019.15243 5033.13396 5063.15243 5077.13396 5107.15243 5121.13396 5151.15243 5165.13396 5195.15243 5209.13396 5239.15243 5253.13396 5283.15243 5297.13396 5327.15243 5341.13396 5371.15243 5385.13396 5415.15243 5429.13396 5459.15243 5473.13396 5503.15243 5517.13396 5547.15243 5561.13396 5591.15243 5605.13396 5635.15243 5649.13396 5679.15243 5693.13396 5723.15243 5737.13396 5767.15243 5781.13396 5811.15243 5825.13396 5855.15243 5869.13396 5899.15243 5913.13396 5943.15243 5957.13396 5987.15243 6001.13396 6031.15243 6045.13396 6075.15243 6089.13396 6119.15243 6133.13396 6163.15243 6177.13396 6207.15243 6221.13396 6251.15243 6265.13396 6295.15243 6309.13396 6339.15243 6353.13396 6383.15243 6397.13396 6427.15243 6441.13396 6471.15243 6485.13396 6515.15243 6529.13396 6559.15243 6573.13396 6603.15243 6617.13396 6647.15243 6661.13396 6691.15243 6705.13396 6735.15243 6749.13396 6779.15243 6793.13396 6823.15243 6837.1339