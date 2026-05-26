# Rosalind Problem: Calculating Expected Offspring

## Problem Statement
For a random variable X taking on values 0, 1, and 2, the expected value of X is defined as E(X) = 0 × P(X = 0) + 1 × P(X = 1) + 2 × P(X = 2).

Given: Six non-negative integers, each of which does not exceed 20,000. The integers correspond to the number of couples in a population possessing each genotype pairing for a given factor. In order, the six given integers represent the number of couples having the following genotypes:

1. AA-AA
2. AA-Aa
3. AA-aa
4. Aa-Aa
5. Aa-aa
6. aa-aa

Return: The expected number of offspring displaying the dominant phenotype in the next generation, under the assumption that every couple has exactly two offspring.

## Solution

```javascript
({
    calculateExpectedOffspring: function(couples) {
        // Genotype pairs: AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa
        // Expected offspring with dominant phenotype for each pair:
        // AA-AA: 100% = 2 offspring
        // AA-Aa: 100% = 2 offspring  
        // AA-aa: 100% = 2 offspring
        // Aa-Aa: 75% = 1.5 offspring
        // Aa-aa: 50% = 1 offspring
        // aa-aa: 0% = 0 offspring
        
        const expectedValues = [2, 2, 2, 1.5, 1, 0];
        let totalExpected = 0;
        
        for (let i = 0; i < couples.length; i++) {
            totalExpected += couples[i] * expectedValues[i];
        }
        
        return totalExpected;
    }
})
```

## Example Usage

```javascript
// Input: [1, 0, 0, 1, 0, 1]
// This represents:
// 1 couple AA-AA
// 0 couples AA-Aa  
// 0 couples AA-aa
// 1 couple Aa-Aa
// 0 couples Aa-aa
// 1 couple aa-aa

const couples = [1, 0, 0, 1, 0, 1];
const result = this.calculateExpectedOffspring(couples);
console.log(result); // Output: 3.5
```

## Explanation

The solution works by:

1. **Defining expected values**: For each genotype pair, we calculate the expected number of offspring with dominant phenotype
2. **Multiplying by couple counts**: Each genotype pair count is multiplied by its expected offspring value
3. **Summing all contributions**: All expected values are summed to get the total expected offspring

**Key calculations**:
- AA-AA → 100% dominant → 2 offspring
- AA-Aa → 100% dominant → 2 offspring  
- AA-aa → 100% dominant → 2 offspring
- Aa-Aa → 75% dominant → 1.5 offspring
- Aa-aa → 50% dominant → 1 offspring
- aa-aa → 0% dominant → 0 offspring

This approach efficiently calculates the expected number of offspring with dominant phenotypes for any given distribution of genotype pairs.

