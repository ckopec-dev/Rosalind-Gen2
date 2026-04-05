# Rosalind Problem: Consensus and Profile (Consensus_and_Profile)

## Problem Description
Given a collection of at most 10 DNA strings of equal length in FASTA format, return a consensus string and a profile matrix.

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <fstream>

using namespace std;

// Function to read FASTA format sequences
vector<string> readFASTA(const string& filename) {
    vector<string> sequences;
    ifstream file(filename);
    string line, current_seq;
    
    while (getline(file, line)) {
        if (line.empty()) continue;
        if (line[0] == '>') {
            if (!current_seq.empty()) {
                sequences.push_back(current_seq);
                current_seq.clear();
            }
        } else {
            current_seq += line;
        }
    }
    
    if (!current_seq.empty()) {
        sequences.push_back(current_seq);
    }
    
    return sequences;
}

// Function to solve consensus and profile problem
void solveConsensusProfile(const vector<string>& sequences) {
    if (sequences.empty()) return;
    
    int n = sequences[0].length();
    int m = sequences.size();
    
    // Create profile matrix (4 rows for A, C, G, T)
    vector<vector<int>> profile(4, vector<int>(n, 0));
    
    // Map nucleotides to indices
    map<char, int> nucleotide_map = {{'A', 0}, {'C', 1}, {'G', 2}, {'T', 3}};
    
    // Fill profile matrix
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            char nucleotide = sequences[i][j];
            profile[nucleotide_map[nucleotide]][j]++;
        }
    }
    
    // Generate consensus string
    string consensus = "";
    for (int j = 0; j < n; j++) {
        int max_count = 0;
        char max_nucleotide = 'A';
        
        for (int i = 0; i < 4; i++) {
            if (profile[i][j] > max_count) {
                max_count = profile[i][j];
                if (i == 0) max_nucleotide = 'A';
                else if (i == 1) max_nucleotide = 'C';
                else if (i == 2) max_nucleotide = 'G';
                else max_nucleotide = 'T';
            }
        }
        consensus += max_nucleotide;
    }
    
    // Output results
    cout << consensus << endl;
    
    // Output profile matrix
    vector<char> nucleotides = {'A', 'C', 'G', 'T'};
    for (int i = 0; i < 4; i++) {
        cout << nucleotides[i] << ": ";
        for (int j = 0; j < n; j++) {
            cout << profile[i][j];
            if (j < n - 1) cout << " ";
        }
        cout << endl;
    }
}

int main() {
    // Read sequences from input (assuming they're in a file)
    vector<string> sequences = readFASTA("rosalind_cons.txt");
    
    // Solve the problem
    solveConsensusProfile(sequences);
    
    return 0;
}
```

## Explanation

### Approach
1. **Read FASTA format**: Parse input sequences from FASTA format where each sequence starts with a '>' header
2. **Build profile matrix**: Count occurrences of each nucleotide (A, C, G, T) at each position across all sequences
3. **Generate consensus**: For each position, select the nucleotide with the highest count
4. **Output results**: Print consensus string followed by profile matrix

### Key Components

1. **readFASTA function**: Parses FASTA format input and returns vector of DNA sequences
2. **solveConsensusProfile function**: 
   - Creates 4×n profile matrix (A, C, G, T rows)
   - Counts nucleotides at each position
   - Determines consensus by selecting maximum count at each position
   - Outputs results in required format

### Time and Space Complexity
- **Time Complexity**: O(m × n) where m is number of sequences and n is sequence length
- **Space Complexity**: O(m × n) for the profile matrix

### Sample Input Format
```
>seq1
ATCCAGCT
>seq2
GGGCAACT
>seq3
ATGGATCT
>seq4
AAGCAACC
>seq5
TTGGAACT
>seq6
ATGCCATT
>seq7
ATGGCACT
```

### Sample Output
```
ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6
```

