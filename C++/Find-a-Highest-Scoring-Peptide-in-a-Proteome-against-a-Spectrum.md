# Rosalind Problem: Find a Highest-Scoring Peptide in a Proteome against a Spectrum

## Problem Description
Given a list of amino acid masses and a spectrum, find the highest-scoring peptide that can be formed from the proteome that matches the spectrum.

## Solution Approach
This is a dynamic programming problem similar to the peptide sequencing problem. We need to:
1. Build a scoring matrix using dynamic programming
2. Find the highest scoring path through the spectrum
3. Reconstruct the peptide sequence

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <climits>

using namespace std;

// Function to calculate the mass of a peptide
long long calculateMass(const string& peptide, const unordered_map<char, long long>& massMap) {
    long long mass = 0;
    for (char c : peptide) {
        mass += massMap.at(c);
    }
    return mass;
}

// Function to find the highest scoring peptide
string findHighestScoringPeptide(const string& spectrum, 
                                const unordered_map<char, long long>& massMap,
                                const vector<char>& aminoAcids) {
    
    // Convert spectrum string to vector of integers
    vector<long long> spectrumVec;
    string num = "";
    for (int i = 0; i < spectrum.length(); i++) {
        if (spectrum[i] == ' ') {
            if (!num.empty()) {
                spectrumVec.push_back(stoll(num));
                num = "";
            }
        } else {
            num += spectrum[i];
        }
    }
    if (!num.empty()) {
        spectrumVec.push_back(stoll(num));
    }
    
    // Sort spectrum
    sort(spectrumVec.begin(), spectrumVec.end());
    
    // Find maximum mass in spectrum
    long long maxMass = spectrumVec.back();
    
    // DP table: dp[i] = maximum score for mass i
    vector<long long> dp(maxMass + 1, INT_MIN);
    vector<char> backtrack(maxMass + 1, '\0');
    
    // Base case
    dp[0] = 0;
    
    // Fill DP table
    for (long long i = 1; i <= maxMass; i++) {
        for (char aa : aminoAcids) {
            long long mass = massMap.at(aa);
            if (i >= mass && dp[i - mass] != INT_MIN) {
                if (dp[i - mass] + mass > dp[i]) {
                    dp[i] = dp[i - mass] + mass;
                    backtrack[i] = aa;
                }
            }
        }
    }
    
    // Find maximum score in spectrum
    long long maxScore = INT_MIN;
    long long bestMass = 0;
    
    for (long long mass : spectrumVec) {
        if (mass <= maxMass && dp[mass] > maxScore) {
            maxScore = dp[mass];
            bestMass = mass;
        }
    }
    
    // Reconstruct peptide
    string peptide = "";
    long long currentMass = bestMass;
    
    while (currentMass > 0 && backtrack[currentMass] != '\0') {
        peptide = backtrack[currentMass] + peptide;
        currentMass -= massMap.at(backtrack[currentMass]);
    }
    
    return peptide;
}

int main() {
    // Define amino acid masses
    unordered_map<char, long long> massMap = {
        {'A', 71}, {'C', 103}, {'D', 115}, {'E', 129}, {'F', 147},
        {'G', 57}, {'H', 137}, {'I', 113}, {'K', 128}, {'L', 113},
        {'M', 131}, {'N', 114}, {'P', 97}, {'Q', 128}, {'R', 156},
        {'S', 87}, {'T', 101}, {'V', 99}, {'W', 186}, {'Y', 163}
    };
    
    // Amino acids in order
    vector<char> aminoAcids = {'A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L',
                              'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'Y'};
    
    // Read input
    string spectrum;
    getline(cin, spectrum);
    
    // Find highest scoring peptide
    string result = findHighestScoringPeptide(spectrum, massMap, aminoAcids);
    
    cout << result << endl;
    
    return 0;
}
```

## Explanation

This solution works as follows:

1. **Input Processing**: 
   - Parse the spectrum string into a vector of integers
   - Sort the spectrum for easier processing

2. **Dynamic Programming Approach**:
   - Create a DP array `dp` where `dp[i]` represents the maximum score achievable for mass `i`
   - For each mass, try adding each amino acid and update the maximum score
   - Keep track of the backtracking information to reconstruct the peptide

3. **Reconstruction**:
   - Find the maximum score among all masses in the spectrum
   - Backtrack through the DP table to reconstruct the peptide sequence

4. **Time Complexity**: O(maxMass × number_of_amino_acids)
5. **Space Complexity**: O(maxMass)

## Key Points

- The algorithm uses dynamic programming to find the optimal peptide sequence
- It handles the constraint that the peptide mass must match the spectrum
- The backtracking mechanism allows reconstruction of the actual peptide string
- The solution accounts for all amino acids with their respective masses

This approach efficiently solves the problem by leveraging the dynamic programming paradigm to find the highest-scoring peptide that matches the given spectrum.

