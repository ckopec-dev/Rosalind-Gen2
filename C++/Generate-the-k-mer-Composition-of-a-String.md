# Rosalind Problem: Generate the k-mer Composition of a String

## Problem Statement
Given a string Text, return the k-mer composition of Text as a sorted list of k-mers.

## Solution in C++

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

std::vector<std::string> get_kmer_composition(const std::string& text, int k) {
    std::vector<std::string> kmers;
    
    // Generate all k-mers from the text
    for (int i = 0; i <= (int)text.length() - k; i++) {
        kmers.push_back(text.substr(i, k));
    }
    
    // Sort the k-mers lexicographically
    std::sort(kmers.begin(), kmers.end());
    
    return kmers;
}

int main() {
    // Read input
    std::string text;
    int k;
    
    std::cout << "Enter the string Text: ";
    std::cin >> text;
    
    std::cout << "Enter the value of k: ";
    std::cin >> k;
    
    // Get k-mer composition
    std::vector<std::string> composition = get_kmer_composition(text, k);
    
    // Print result
    for (const std::string& kmer : composition) {
        std::cout << kmer << std::endl;
    }
    
    return 0;
}
```

## Alternative Version (for file input/output)

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <fstream>

std::vector<std::string> get_kmer_composition(const std::string& text, int k) {
    std::vector<std::string> kmers;
    
    // Generate all k-mers from the text
    for (size_t i = 0; i <= text.length() - k; i++) {
        kmers.push_back(text.substr(i, k));
    }
    
    // Sort the k-mers lexicographically
    std::sort(kmers.begin(), kmers.end());
    
    return kmers;
}

int main() {
    // For Rosalind input/output
    std::ifstream infile("rosalind_ba3a.txt");
    std::ofstream outfile("output.txt");
    
    std::string text;
    int k;
    
    infile >> text >> k;
    
    std::vector<std::string> composition = get_kmer_composition(text, k);
    
    for (const std::string& kmer : composition) {
        outfile << kmer << std::endl;
    }
    
    infile.close();
    outfile.close();
    
    return 0;
}
```

## Example

**Input:**
```
Text = "CGTAGCCTAGCG"
k = 3
```

**Output:**
```
AGC
ATC
CGC
CTA
TAG
TCC
```

## Explanation

1. **Input Processing**: Read the string Text and integer k
2. **K-mer Generation**: Extract all substrings of length k from Text
3. **Sorting**: Sort the k-mers lexicographically
4. **Output**: Return the sorted list of k-mers

## Time Complexity
- **Time**: O(n × k × log n) where n is the length of Text
  - O(n × k) to generate all k-mers
  - O(n × log n) to sort the k-mers
- **Space**: O(n × k) for storing the k-mers

## Key Points
- The k-mer composition includes all possible k-length substrings
- The result must be sorted lexicographically
- Handle edge cases where k > length of Text (empty result)
- For Rosalind problems, ensure proper file I/O format matching the problem requirements

