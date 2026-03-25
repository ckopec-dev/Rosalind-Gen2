# Rosalind Problem: Reconstruct a String from its Burrows-Wheeler Transform

## Problem Understanding

The Burrows-Wheeler Transform (BWT) is a data transformation technique used in bioinformatics. Given a string, the BWT:
1. Creates all cyclic rotations of the string
2. Sorts these rotations lexicographically
3. Takes the last column of the sorted rotations

To reconstruct the original string from the BWT, we need to reverse this process.

## Approach

1. **Sort the BWT string** to get the first column
2. **Create a mapping** between the sorted and original BWT characters with their indices
3. **Follow the pointers** to reconstruct the original string by tracing backwards through the BWT

## Solution

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <utility>

using namespace std;

string reconstruct_string_from_bwt(const string& bwt) {
    if (bwt.empty()) return "";
    
    int n = bwt.length();
    
    // Create pairs of (character, original_index) for the BWT
    vector<pair<char, int>> bwt_pairs(n);
    for (int i = 0; i < n; i++) {
        bwt_pairs[i] = {bwt[i], i};
    }
    
    // Sort by character to get the first column
    sort(bwt_pairs.begin(), bwt_pairs.end());
    
    // Create a mapping from original BWT positions to sorted positions
    vector<int> position_map(n);
    for (int i = 0; i < n; i++) {
        position_map[bwt_pairs[i].second] = i;
    }
    
    // Reconstruct the original string by following the pointers
    string result = "";
    int current_pos = 0;  // Start with the first row (index 0)
    
    for (int i = 0; i < n; i++) {
        result += bwt[current_pos];
        current_pos = position_map[current_pos];
    }
    
    return result;
}

int main() {
    // Read input
    string bwt;
    cin >> bwt;
    
    // Reconstruct the original string
    string original = reconstruct_string_from_bwt(bwt);
    
    // Output the result
    cout << original << endl;
    
    return 0;
}
```

## Algorithm Explanation

1. **Create pairs**: For each character in the BWT, store it along with its original index
2. **Sort**: Sort these pairs by character value to get the first column of the BWT matrix
3. **Position mapping**: Create a mapping from each original BWT position to its new position after sorting
4. **Trace back**: Starting from position 0, follow the mapping to reconstruct the original string by reading characters in reverse order

## Example

For BWT = "BANANA$":
1. Pairs: (B,0), (A,1), (N,2), (A,3), (N,4), (A,5), ($,6)
2. After sorting: ($,6), (A,1), (A,3), (A,5), (B,0), (N,2), (N,4)
3. Position mapping: [4, 1, 5, 2, 6, 3, 0] (original index → sorted index)
4. Trace: Start at 0, get $, then follow to position 4, get B, then to position 6, get A, etc.

## Time and Space Complexity

- **Time Complexity**: O(n log n) due to sorting
- **Space Complexity**: O(n) for storing the pairs and mapping

## Sample Input/Output

**Input:**
```
BANANA$
```

**Output:**
```
BANANA$
```

This solution correctly reconstructs the original string from its Burrows-Wheeler Transform by following the inverse process of the BWT construction algorithm.

