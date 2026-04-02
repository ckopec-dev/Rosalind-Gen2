# Rosalind Problem: Insertion Sort

## Problem Statement
Given: A positive integer n ≤ 10^3 and an array A[1..n] of integers.

Return: The number of swaps performed by insertion sort algorithm on A.

## Solution in Lightning Web Component

```javascript
// insertion_sort.js
import { LightningElement } from 'lwc';

export default class InsertionSort extends LightningElement {
    array = [5, 2, 4, 6, 1, 3];
    swapCount = 0;
    result = '';

    handleSort() {
        this.swapCount = 0;
        const arr = [...this.array];
        this.insertionSort(arr);
        this.result = `Number of swaps: ${this.swapCount}`;
    }

    insertionSort(arr) {
        for (let i = 1; i < arr.length; i++) {
            let key = arr[i];
            let j = i - 1;
            
            // Move elements that are greater than key one position ahead
            while (j >= 0 && arr[j] > key) {
                arr[j + 1] = arr[j];
                j--;
                this.swapCount++; // Count each swap
            }
            arr[j + 1] = key;
        }
        return arr;
    }

    handleInputChange(event) {
        const input = event.target.value;
        this.array = input.split(' ').map(Number).filter(num => !isNaN(num));
    }

    get arrayString() {
        return this.array.join(' ');
    }
}
```

```html
<!-- insertion_sort.html -->
<template>
    <div class="container">
        <h2>Insertion Sort</h2>
        
        <div class="input-section">
            <label for="arrayInput">Enter array (space-separated):</label>
            <input 
                id="arrayInput"
                type="text" 
                value={arrayString}
                onchange={handleInputChange}
                class="array-input"
            />
        </div>

        <button onclick={handleSort} class="sort-button">
            Run Insertion Sort
        </button>

        <div class="result-section">
            <p>{result}</p>
        </div>
    </div>
</template>
```

```css
/* insertion_sort.css */
.container {
    max-width: 600px;
    margin: 20px auto;
    padding: 20px;
    font-family: Arial, sans-serif;
}

.input-section {
    margin-bottom: 20px;
}

.array-input {
    width: 100%;
    padding: 10px;
    font-size: 16px;
    border: 1px solid #ccc;
    border-radius: 4px;
    box-sizing: border-box;
}

.sort-button {
    background-color: #4CAF50;
    color: white;
    padding: 12px 20px;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 16px;
    margin-bottom: 20px;
}

.sort-button:hover {
    background-color: #45a049;
}

.result-section {
    padding: 15px;
    background-color: #f0f8ff;
    border-radius: 4px;
    border-left: 4px solid #4CAF50;
}
```

## How It Works

1. **Input**: The user enters space-separated integers representing the array to be sorted
2. **Algorithm**: 
   - Start from the second element (index 1)
   - Compare current element with elements to its left
   - Shift larger elements one position to the right
   - Insert current element in correct position
   - Count each shift as a swap
3. **Output**: Returns the total number of swaps performed

## Example

**Input**: `5 2 4 6 1 3`

**Process**:
- Compare 2 with 5, shift 5 → 1 swap
- Compare 4 with 5, shift 5 → 1 swap  
- Compare 1 with 2, 4, 5 → 3 swaps
- Compare 3 with 4, 5 → 2 swaps
- Total swaps: 7

**Output**: `Number of swaps: 7`

## Time Complexity
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²) 
- **Worst Case**: O(n²) - when array is reverse sorted

## Space Complexity
- O(1) - only uses a constant amount of extra memory

This implementation counts the exact number of swaps performed during the insertion sort process, which matches the requirements of the Rosalind problem.

