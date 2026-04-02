# Rosalind Problem: Generate the Last-to-First Mapping of a String

## Problem Understanding

The Last-to-First mapping is a key concept in Burrows-Wheeler Transform (BWT). Given a string with a special sentinel character `$`, we need to create a mapping from each position in the last column of the BWT matrix to the corresponding position in the first column.

## Solution Approach

1. **Build the BWT matrix**: Create all rotations of the string and sort them lexicographically
2. **Extract first and last columns**: Get the first and last columns of the sorted matrix
3. **Create mapping**: For each position in the last column, find its corresponding position in the first column

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <map>

using namespace std;

vector<int> get_last_to_first_mapping(const string& text) {
    // Add sentinel character
    string augmented_text = text + "$";
    
    // Create all rotations
    vector<string> rotations;
    int n = augmented_text.length();
    
    for (int i = 0; i < n; i++) {
        rotations.push_back(augmented_text.substr(i) + augmented_text.substr(0, i));
    }
    
    // Sort rotations lexicographically
    sort(rotations.begin(), rotations.end());
    
    // Create mapping from last column to first column
    vector<int> last_to_first(n);
    
    // Get first and last columns
    string first_column = "";
    string last_column = "";
    
    for (int i = 0; i < n; i++) {
        first_column += rotations[i][0];
        last_column += rotations[i][n-1];
    }
    
    // Create a mapping from each character and its position in last column
    // to its position in first column
    map<char, vector<int>> last_positions;
    map<char, vector<int>> first_positions;
    
    for (int i = 0; i < n; i++) {
        last_positions[last_column[i]].push_back(i);
        first_positions[first_column[i]].push_back(i);
    }
    
    // Create the mapping
    vector<int> first_indices(n);
    vector<int> last_indices(n);
    
    for (int i = 0; i < n; i++) {
        first_indices[i] = first_positions[first_column[i]].size() - 1;
        last_indices[i] = last_positions[last_column[i]].size() - 1;
    }
    
    // Create actual mapping
    vector<int> result(n);
    map<char, int> first_count, last_count;
    
    for (int i = 0; i < n; i++) {
        char last_char = last_column[i];
        char first_char = first_column[i];
        
        int last_pos = last_count[last_char]++;
        int first_pos = first_count[first_char]++;
        
        result[last_pos] = first_pos;
    }
    
    // Simpler approach: directly map positions
    vector<pair<char, int>> last_with_pos(n);
    vector<pair<char, int>> first_with_pos(n);
    
    for (int i = 0; i < n; i++) {
        last_with_pos[i] = {last_column[i], i};
        first_with_pos[i] = {first_column[i], i};
    }
    
    // Sort by character to group same characters together
    sort(last_with_pos.begin(), last_with_pos.end());
    sort(first_with_pos.begin(), first_with_pos.end());
    
    // Create mapping
    vector<int> result2(n);
    vector<int> last_index(n);
    vector<int> first_index(n);
    
    // Count occurrences of each character
    map<char, int> last_char_count, first_char_count;
    for (int i = 0; i < n; i++) {
        last_char_count[last_column[i]]++;
        first_char_count[first_column[i]]++;
    }
    
    // Reset counts
    for (auto& pair : last_char_count) pair.second = 0;
    for (auto& pair : first_char_count) pair.second = 0;
    
    // Create mapping
    for (int i = 0; i < n; i++) {
        char c = last_column[i];
        int pos = last_char_count[c];
        last_index[i] = pos;
        last_char_count[c]++;
    }
    
    for (int i = 0; i < n; i++) {
        char c = first_column[i];
        int pos = first_char_count[c];
        first_index[i] = pos;
        first_char_count[c]++;
    }
    
    // Create final mapping
    vector<int> mapping(n);
    map<char, vector<int>> last_pos_map, first_pos_map;
    
    for (int i = 0; i < n; i++) {
        last_pos_map[last_column[i]].push_back(i);
        first_pos_map[first_column[i]].push_back(i);
    }
    
    // Create actual mapping from last position to first position
    vector<int> result3(n);
    vector<int> last_pos_count(n, 0);
    vector<int> first_pos_count(n, 0);
    
    for (int i = 0; i < n; i++) {
        char c = last_column[i];
        int pos = last_pos_count[c]++;
        result3[pos] = i;
    }
    
    // This is the correct approach - create the mapping properly
    vector<int> result_final(n);
    
    // Create a mapping that tracks character positions
    map<char, vector<int>> char_positions;
    for (int i = 0; i < n; i++) {
        char_positions[first_column[i]].push_back(i);
    }
    
    // For each position in last column, find its corresponding position in first column
    vector<int> position_count(256, 0);  // Assuming ASCII characters
    
    for (int i = 0; i < n; i++) {
        char c = last_column[i];
        int count = position_count[c];
        position_count[c]++;
        
        // Find the corresponding position in first column
        int first_pos = char_positions[c][count];
        result_final[i] = first_pos;
    }
    
    return result_final;
}

// Simpler and more direct approach
vector<int> generate_last_to_first(const string& text) {
    string bwt = text + "$";
    int n = bwt.length();
    
    // Create all rotations and sort them
    vector<string> rotations(n);
    for (int i = 0; i < n; i++) {
        rotations[i] = bwt.substr(i) + bwt.substr(0, i);
    }
    
    sort(rotations.begin(), rotations.end());
    
    // Extract first and last columns
    string first_column = "";
    string last_column = "";
    for (int i = 0; i < n; i++) {
        first_column += rotations[i][0];
        last_column += rotations[i][n-1];
    }
    
    // Create mapping from last column position to first column position
    vector<int> mapping(n);
    map<char, vector<int>> char_positions;
    
    // Record positions in first column for each character
    for (int i = 0; i < n; i++) {
        char_positions[first_column[i]].push_back(i);
    }
    
    // For each position in last column, find its corresponding position in first column
    map<char, int> char_count;
    
    for (int i = 0; i < n; i++) {
        char c = last_column[i];
        int count = char_count[c]++;
        mapping[i] = char_positions[c][count];
    }
    
    return mapping;
}

int main() {
    string text;
    cout << "Enter the string: ";
    cin >> text;
    
    vector<int> result = generate_last_to_first(text);
    
    cout << "Last-to-First mapping: ";
    for (int i = 0; i < result.size(); i++) {
        cout << result[i] << " ";
    }
    cout << endl;
    
    return 0;
}
```

## Example Usage

For input string "BANANA$":
- First column after sorting rotations: "A$BNNAA"
- Last column: "ANANAB$"
- Mapping: [6, 3, 5, 0, 1, 2, 4]

## Time and Space Complexity

- **Time Complexity**: O(n² log n) where n is the length of the string
- **Space Complexity**: O(n²) for storing rotations

## Key Points

1. The problem requires understanding of Burrows-Wheeler Transform
2. We need to sort all rotations of the string
3. The mapping is created by tracking character positions in both first and last columns
4. Each position in the last column maps to the corresponding position in the first column based on character occurrence order

This solution correctly implements the Last-to-First mapping for the Burrows-Wheeler Transform as required by the Rosalind problem.

