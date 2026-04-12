# Rosalind Problem: Compute the Squared Error Distortion

## Problem Description
Given a set of data points and a set of centers, compute the squared error distortion, which is the sum of squared distances from each data point to its nearest center.

## Solution in Lightning Web Component

```javascript
// Compute_Squared_Error_Distortion.js
import { LightningElement } from 'lwc';

export default class ComputeSquaredErrorDistortion extends LightningElement {
    // Function to compute squared error distortion
    computeSquaredErrorDistortion(dataPoints, centers) {
        let totalDistortion = 0;
        
        // For each data point
        for (let i = 0; i < dataPoints.length; i++) {
            const point = dataPoints[i];
            let minDistance = Infinity;
            
            // Find the nearest center
            for (let j = 0; j < centers.length; j++) {
                const center = centers[j];
                // Calculate Euclidean distance squared
                let distanceSquared = 0;
                for (let k = 0; k < point.length; k++) {
                    distanceSquared += Math.pow(point[k] - center[k], 2);
                }
                
                if (distanceSquared < minDistance) {
                    minDistance = distanceSquared;
                }
            }
            
            totalDistortion += minDistance;
        }
        
        return totalDistortion;
    }
    
    // Example usage
    handleCompute() {
        // Example data points (2D coordinates)
        const dataPoints = [
            [2.3, 4.1],
            [1.2, 3.7],
            [3.5, 2.8],
            [4.2, 5.1]
        ];
        
        // Example centers (2D coordinates)
        const centers = [
            [2.0, 4.0],
            [3.0, 3.0]
        ];
        
        const distortion = this.computeSquaredErrorDistortion(dataPoints, centers);
        console.log('Squared Error Distortion:', distortion);
    }
}
```

```html
<!-- Compute_Squared_Error_Distortion.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title slds-truncate" title="Compute Squared Error Distortion">
                <h2 class="slds-card__header-title">Compute Squared Error Distortion</h2>
            </header>
        </div>
        <div class="slds-card__body">
            <p>Computes the squared error distortion for a set of data points and centers.</p>
            <lightning-button 
                label="Compute Distortion" 
                variant="brand" 
                onclick={handleCompute}>
            </lightning-button>
            <div class="slds-m-top_medium">
                <p><strong>Formula:</strong> Sum of squared distances from each point to its nearest center</p>
                <p><strong>Distortion:</strong> <span id="distortion-result">-</span></p>
            </div>
        </div>
    </div>
</template>
```

```css
/* Compute_Squared_Error_Distortion.css */
.slds-card__body {
    padding: 1rem;
}

.slds-card__header-title {
    font-size: 1.25rem;
    font-weight: 600;
}
```

## Algorithm Explanation

1. **Input**: 
   - `dataPoints`: Array of n-dimensional points
   - `centers`: Array of k-dimensional centers

2. **Process**:
   - For each data point, find the nearest center using Euclidean distance
   - Calculate squared distance from point to nearest center
   - Sum all squared distances

3. **Output**: 
   - The total squared error distortion value

## Time Complexity
- **Time**: O(n × k × d) where n = number of data points, k = number of centers, d = dimensions
- **Space**: O(1) additional space

## Example
For data points [[2.3, 4.1], [1.2, 3.7], [3.5, 2.8], [4.2, 5.1]] and centers [[2.0, 4.0], [3.0, 3.0]]:
- Point [2.3, 4.1] → nearest center [2.0, 4.0] → distance² = 0.09
- Point [1.2, 3.7] → nearest center [2.0, 4.0] → distance² = 0.64
- Point [3.5, 2.8] → nearest center [3.0, 3.0] → distance² = 0.25
- Point [4.2, 5.1] → nearest center [3.0, 3.0] → distance² = 5.44

**Total Distortion = 0.09 + 0.64 + 0.25 + 5.44 = 6.42**

