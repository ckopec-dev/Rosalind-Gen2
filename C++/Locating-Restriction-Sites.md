# Rosalind Problem: Locating Restriction Sites

## Problem Statement
In the DNA string, find all locations of restriction sites (palindromic sequences that are recognized by restriction enzymes). A restriction site is a palindromic sequence of length 4-12.

## Solution

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

// Function to check if a string is palindromic
bool isPalindrome(const string& s) {
    int n = s.length();
    for (int i = 0; i < n / 2; i++) {
        if (s[i] != s[n - 1 - i]) {
            return false;
        }
    }
    return true;
}

// Function to get reverse complement of a DNA string
string reverseComplement(const string& s) {
    string rc = "";
    for (int i = s.length() - 1; i >= 0; i--) {
        switch (s[i]) {
            case 'A': rc += 'T'; break;
            case 'T': rc += 'A'; break;
            case 'G': rc += 'C'; break;
            case 'C': rc += 'G'; break;
        }
    }
    return rc;
}

// Function to find all restriction sites
vector<pair<int, int>> findRestrictionSites(const string& dna) {
    vector<pair<int, int>> sites;
    int n = dna.length();
    
    // Check all possible palindromic sequences of length 4-12
    for (int len = 4; len <= 12; len += 2) {  // Only even lengths for palindromes
        for (int i = 0; i <= n - len; i++) {
            string substring = dna.substr(i, len);
            string rc = reverseComplement(substring);
            
            // A palindromic sequence is equal to its reverse complement
            if (substring == rc) {
                sites.push_back({i + 1, len});  // 1-indexed positions
            }
        }
    }
    
    return sites;
}

int main() {
    // Read input from file
    ifstream infile("rosalind_revp.txt");
    string line;
    string dna = "";
    
    // Skip the first line (FASTA header)
    getline(infile, line);
    
    // Read the DNA sequence
    while (getline(infile, line)) {
        dna += line;
    }
    infile.close();
    
    // Find restriction sites
    vector<pair<int, int>> sites = findRestrictionSites(dna);
    
    // Sort by position
    sort(sites.begin(), sites.end());
    
    // Output results
    for (const auto& site : sites) {
        cout << site.first << " " << site.second << endl;
    }
    
    return 0;
}
```

## Explanation

1. **Palindrome Check**: The `isPalindrome` function checks if a string is palindromic by comparing characters from both ends.

2. **Reverse Complement**: The `reverseComplement` function generates the reverse complement of a DNA sequence by:
   - Reversing the string
   - Complementing each nucleotide (A↔T, G↔C)

3. **Restriction Site Detection**: The `findRestrictionSites` function:
   - Checks all possible substrings of lengths 4, 6, 8, 10, and 12
   - For each substring, computes its reverse complement
   - If the substring equals its reverse complement, it's a palindromic sequence
   - Returns positions (1-indexed) and lengths of all restriction sites

4. **Input/Output**: 
   - Reads DNA sequence from input file
   - Outputs all restriction sites with position and length

## Sample Input
```
>Rosalind_2423
TCAATGCATGCGGGTCTATATGCAT
```

## Sample Output
```
4 6
12 6
17 6
```

## Time Complexity
- O(n²) where n is the length of the DNA sequence
- For each position, we check up to 12 different lengths
- Space complexity: O(n) for storing the DNA sequence and results

This solution efficiently identifies all palindromic restriction sites in the given DNA sequence, which are essential for molecular biology applications like DNA cloning and analysis.

