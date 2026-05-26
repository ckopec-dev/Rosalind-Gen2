# Rosalind Problem: Compute the Probability of a Spectral Dictionary

## Problem Understanding

This problem asks us to compute the probability of a spectral dictionary, which involves calculating the probability of observing a particular spectral pattern given a set of amino acid masses.

## Solution Approach

I'll solve this using a dynamic programming approach where we build up probabilities for different spectral patterns.

```javascript
// Lightning Web Component for Compute the Probability of a Spectral Dictionary
import { LightningElement } from 'lwc';

export default class SpectralDictionaryProbability extends LightningElement {
    // Input data
    aminoAcidMasses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186];
    targetMass = 100;
    
    // Method to compute probability of spectral dictionary
    computeSpectralProbability(targetMass, aminoAcidMasses) {
        // Create a dynamic programming array to store probabilities
        // dp[i] represents the probability of achieving mass i
        const dp = new Array(targetMass + 1).fill(0);
        dp[0] = 1; // Base case: probability of mass 0 is 1
        
        // For each mass from 1 to targetMass
        for (let mass = 1; mass <= targetMass; mass++) {
            // For each amino acid mass
            for (const aaMass of aminoAcidMasses) {
                // If we can form this mass using the current amino acid
                if (mass >= aaMass) {
                    // Add the probability of reaching (mass - aaMass)
                    dp[mass] += dp[mass - aaMass];
                }
            }
        }
        
        // The probability of the spectral dictionary is the probability of the target mass
        // divided by the total number of possible combinations
        const totalCombinations = dp[targetMass];
        
        return totalCombinations;
    }
    
    // Alternative approach using probability calculation
    computeProbability(targetMass, aminoAcidMasses) {
        // Create a probability array
        const prob = new Array(targetMass + 1).fill(0);
        prob[0] = 1; // Base case
        
        // For each mass from 1 to targetMass
        for (let mass = 1; mass <= targetMass; mass++) {
            // For each amino acid mass
            for (const aaMass of aminoAcidMasses) {
                if (mass >= aaMass) {
                    // Add probability of reaching (mass - aaMass)
                    prob[mass] += prob[mass - aaMass];
                }
            }
        }
        
        return prob[targetMass];
    }
    
    // Method to get all possible combinations
    getAllCombinations(targetMass, aminoAcidMasses) {
        const combinations = [];
        
        // Recursive helper function to find combinations
        function findCombinations(remainingMass, currentCombination, startIndex) {
            if (remainingMass === 0) {
                combinations.push([...currentCombination]);
                return;
            }
            
            for (let i = startIndex; i < aminoAcidMasses.length; i++) {
                if (aminoAcidMasses[i] <= remainingMass) {
                    currentCombination.push(aminoAcidMasses[i]);
                    findCombinations(remainingMass - aminoAcidMasses[i], currentCombination, i);
                    currentCombination.pop();
                }
            }
        }
        
        findCombinations(targetMass, [], 0);
        return combinations;
    }
    
    // Main method to solve the problem
    solveProblem() {
        const result = this.computeProbability(this.targetMass, this.aminoAcidMasses);
        const combinations = this.getAllCombinations(this.targetMass, this.aminoAcidMasses);
        
        console.log(`Total combinations for mass ${this.targetMass}: ${result}`);
        console.log(`Number of unique combinations: ${combinations.length}`);
        
        return {
            probability: result,
            combinations: combinations.length
        };
    }
    
    // Handle button click
    handleSolve() {
        const solution = this.solveProblem();
        console.log('Solution:', solution);
        
        // Display results in the component
        this.dispatchEvent(new CustomEvent('solution', {
            detail: solution
        }));
    }
    
    // Render method
    render() {
        // This would be handled by LWC template
        return `
            <div class="spectral-container">
                <h2>Compute Probability of Spectral Dictionary</h2>
                <p>Target Mass: ${this.targetMass}</p>
                <p>Amino Acid Masses: ${this.aminoAcidMasses.join(', ')}</p>
                <lightning-button 
                    label="Compute Probability" 
                    onclick={handleSolve}>
                </lightning-button>
            </div>
        `;
    }
}
```

## Input/Output Example

**Input:**
- Target mass: 100
- Amino acid masses: [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]

**Output:**
- Total number of ways to form mass 100: [computed value]
- Probability of spectral dictionary: [computed probability]

## Key Concepts

1. **Dynamic Programming**: We build up solutions from smaller subproblems
2. **Probability Calculation**: Each valid combination contributes to the total probability
3. **Spectral Dictionary**: Represents all possible amino acid combinations that sum to target mass
4. **Efficiency**: Time complexity O(targetMass × number of amino acids)

## Time Complexity
- **Time**: O(targetMass × number of amino acids)
- **Space**: O(targetMass)

This solution efficiently computes the probability of a spectral dictionary by counting all possible combinations that sum to the target mass using dynamic programming.

