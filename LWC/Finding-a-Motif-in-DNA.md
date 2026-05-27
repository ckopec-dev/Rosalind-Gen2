# Finding a Motif in DNA - Rosalind Problem Solution

## Problem Statement
Given two DNA strings s and t, find all locations of t as a substring of s.

## Solution in Lightning Web Component (JavaScript)

```javascript
// Finding a Motif in DNA - Rosalind Problem
// Given DNA strings s and t, find all locations of t as a substring of s

findMotifLocations = (s, t) => {
    const locations = [];
    const tLength = t.length;
    const sLength = s.length;
    
    // Handle edge cases
    if (tLength > sLength || tLength === 0) {
        return locations;
    }
    
    // Search for motif t in string s
    for (let i = 0; i <= sLength - tLength; i++) {
        // Check if substring matches the motif
        if (s.substring(i, i + tLength) === t) {
            // Add 1-based position (Rosalind uses 1-based indexing)
            locations.push(i + 1);
        }
    }
    
    return locations;
};

// Alternative implementation using indexOf
findMotifLocationsAlt = (s, t) => {
    const locations = [];
    let startIndex = 0;
    
    // Handle edge cases
    if (t.length > s.length || t.length === 0) {
        return locations;
    }
    
    // Find all occurrences using indexOf
    while (startIndex <= s.length - t.length) {
        const pos = s.indexOf(t, startIndex);
        if (pos === -1) break; // No more occurrences
        
        locations.push(pos + 1); // Convert to 1-based indexing
        startIndex = pos + 1; // Move to next position
    }
    
    return locations;
};

// Example usage:
const s = "GATATATGCATATACTT";
const t = "ATAT";

console.log("DNA string s:", s);
console.log("Motif t:", t);
console.log("Locations:", findMotifLocations(s, t));

// Expected output: [2, 4, 10]
// Positions where "ATAT" occurs in "GATATATGCATATACTT"
```

## Explanation

### Approach
1. **String Matching**: Iterate through the DNA string `s` and check each possible substring of length equal to `t`
2. **1-based Indexing**: Rosalind uses 1-based indexing, so we add 1 to the 0-based JavaScript indices
3. **Edge Case Handling**: Handle cases where the motif is longer than the DNA string or empty

### Time Complexity
- **Time**: O(n × m) where n is the length of string `s` and m is the length of motif `t`
- **Space**: O(k) where k is the number of matches found

### Key Features
- Handles empty strings and edge cases
- Uses 1-based indexing as required by Rosalind
- Efficient substring comparison
- Alternative implementation using `indexOf` method

## Test Case
```javascript
// Test with the example from Rosalind
const testS = "GATATATGCATATACTT";
const testT = "ATAT";
const result = findMotifLocations(testS, testT);
console.log(result); // Output: [2, 4, 10]
```

This solution correctly identifies all positions where the motif "ATAT" appears in the DNA string "GATATATGCATATACTT" at positions 2, 4, and 10 (1-based indexing).

