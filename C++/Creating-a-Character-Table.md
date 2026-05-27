# Rosalind Problem: Creating a Character Table (C++ Solution)

## Problem Understanding

The problem asks us to create a character table from a set of DNA sequences. A character table represents the presence (1) or absence (0) of specific characters (nucleotides) in each sequence, typically using a binary representation where:
- 1 represents the presence of a character
- 0 represents the absence of a character

## Solution Approach

1. Parse input sequences
2. For each position in the sequences, determine which characters are present
3. Create a binary representation for each sequence
4. Output the character table

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <unordered_set>
#include <algorithm>

using namespace std;

int main() {
    // Read input sequences
    vector<string> sequences;
    string line;
    
    while (getline(cin, line)) {
        if (!line.empty()) {
            sequences.push_back(line);
        }
    }
    
    if (sequences.empty()) {
        return 0;
    }
    
    int num_sequences = sequences.size();
    int sequence_length = sequences[0].length();
    
    // For each position, determine which characters are present
    vector<vector<bool>> character_table(num_sequences, vector<bool>(sequence_length, false));
    
    // Process each sequence
    for (int i = 0; i < num_sequences; i++) {
        for (int j = 0; j < sequence_length; j++) {
            character_table[i][j] = (sequences[i][j] != '-'); // Assuming '-' means absent
        }
    }
    
    // Alternative approach: create a more detailed character table
    // Let's assume we want to create a binary representation for each sequence
    // based on presence/absence of characters
    
    // For each sequence, output its character presence pattern
    for (int i = 0; i < num_sequences; i++) {
        string result = "";
        for (int j = 0; j < sequence_length; j++) {
            if (sequences[i][j] == '-') {
                result += "0";
            } else {
                result += "1";
            }
        }
        cout << result << endl;
    }
    
    return 0;
}
```

## Better Solution for Character Table Creation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>

using namespace std;

int main() {
    vector<string> sequences;
    string line;
    
    // Read all sequences
    while (getline(cin, line)) {
        if (!line.empty()) {
            sequences.push_back(line);
        }
    }
    
    if (sequences.empty()) {
        return 0;
    }
    
    int n = sequences.size();
    int m = sequences[0].length();
    
    // Create character table
    // Each row represents a sequence, each column represents a character position
    // 1 for present, 0 for absent
    
    for (int i = 0; i < n; i++) {
        string row = "";
        for (int j = 0; j < m; j++) {
            // If character is not a gap ('-'), it's present (1)
            // Otherwise it's absent (0)
            if (sequences[i][j] == '-') {
                row += "0";
            } else {
                row += "1";
            }
        }
        cout << row << endl;
    }
    
    return 0;
}
```

## Most Robust Solution

```cpp
#include <iostream>
#include <vector>
#include <string>

using namespace std;

int main() {
    vector<string> sequences;
    string line;
    
    // Read sequences from input
    while (getline(cin, line)) {
        if (!line.empty()) {
            sequences.push_back(line);
        }
    }
    
    if (sequences.empty()) {
        return 0;
    }
    
    int num_sequences = sequences.size();
    int sequence_length = sequences[0].length();
    
    // Generate character table
    for (int i = 0; i < num_sequences; i++) {
        string character_row = "";
        for (int j = 0; j < sequence_length; j++) {
            // Convert each character to binary representation
            // 1 if character is not a gap ('-'), 0 otherwise
            if (sequences[i][j] == '-') {
                character_row += "0";
            } else {
                character_row += "1";
            }
        }
        cout << character_row << endl;
    }
    
    return 0;
}
```

## Input/Output Example

**Input:**
```
ATCG
ATCG
ATCG
```

**Output:**
```
1111
1111
1111
```

**Input:**
```
ATCG
ATCG
ATCG
```

**Output:**
```
1111
1111
1111
```

## Key Points

1. **Input Parsing**: Read sequences line by line until EOF
2. **Character Representation**: Convert each character to binary (1 for present, 0 for absent)
3. **Gap Handling**: Typically '-' represents absent characters
4. **Output Format**: Each sequence becomes a row in the character table
5. **Edge Cases**: Handle empty input and ensure proper sequence length handling

This solution creates a simple binary character table where each position in the table represents whether a character is present (1) or absent (0) in the corresponding sequence.

