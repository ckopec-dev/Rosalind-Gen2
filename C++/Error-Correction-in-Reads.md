# Rosalind Problem: Error Correction in Reads (Error_Correction_in_Reads)

## Problem Description
Given a collection of DNA strings representing reads, we need to identify and correct sequencing errors. An error occurs when a read is the reverse complement of another read, or when a read appears with a single nucleotide difference from another read.

## Solution Approach
1. Count frequency of each read
2. Find reads that appear only once (potential errors)
3. For each single-occurrence read, check if it's a reverse complement or differs by one nucleotide from a read that appears more than once
4. Correct the error by replacing with the correct read

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>

using namespace std;

// Function to get reverse complement of a DNA string
string reverseComplement(const string& s) {
    string rc = s;
    reverse(rc.begin(), rc.end());
    for (char& c : rc) {
        switch (c) {
            case 'A': c = 'T'; break;
            case 'T': c = 'A'; break;
            case 'G': c = 'C'; break;
            case 'C': c = 'G'; break;
        }
    }
    return rc;
}

// Function to calculate Hamming distance between two strings
int hammingDistance(const string& s1, const string& s2) {
    int distance = 0;
    for (size_t i = 0; i < s1.length(); i++) {
        if (s1[i] != s2[i]) {
            distance++;
        }
    }
    return distance;
}

int main() {
    vector<string> reads;
    string read;
    
    // Read all reads from input
    while (cin >> read) {
        reads.push_back(read);
    }
    
    // Count frequency of each read
    map<string, int> readCount;
    for (const string& r : reads) {
        readCount[r]++;
    }
    
    // Find reads that appear only once (potential errors)
    vector<string> singleReads;
    for (const auto& pair : readCount) {
        if (pair.second == 1) {
            singleReads.push_back(pair.first);
        }
    }
    
    // Process each single occurrence read to find corrections
    for (const string& singleRead : singleReads) {
        // Check if it's reverse complement of a read that appears more than once
        string rc = reverseComplement(singleRead);
        if (readCount[rc] > 1) {
            cout << singleRead << "->" << rc << endl;
            continue;
        }
        
        // Check if it differs by one nucleotide from a read that appears more than once
        for (const auto& pair : readCount) {
            const string& correctRead = pair.first;
            if (pair.second > 1 && correctRead.length() == singleRead.length()) {
                if (hammingDistance(singleRead, correctRead) == 1) {
                    cout << singleRead << "->" << correctRead << endl;
                    break;
                }
            }
        }
    }
    
    return 0;
}
```

## How it works:

1. **Input Reading**: Read all DNA reads from standard input
2. **Frequency Counting**: Use a map to count how many times each read appears
3. **Single Occurrence Identification**: Find reads that appear exactly once (potential errors)
4. **Error Correction**:
   - Check if the single read is a reverse complement of a read that appears more than once
   - If not, check if it differs by exactly one nucleotide from a read that appears more than once
5. **Output**: Print corrections in the format "incorrect->correct"

## Time Complexity: O(n²m) where n is the number of reads and m is the average read length
## Space Complexity: O(nm) for storing reads and their counts

The solution handles both types of errors:
- **Reverse complement errors**: When a read is the reverse complement of a correct read
- **Single nucleotide errors**: When a read differs by exactly one nucleotide from a correct read

