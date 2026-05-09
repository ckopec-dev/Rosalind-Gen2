# Rosalind Problem: Linguistic Complexity of a Genome

## Problem Description
The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings of a given length to the total number of possible substrings of that length.

## Solution

```cpp
#include <iostream>
#include <string>
#include <unordered_set>
#include <cmath>
using namespace std;

// Function to calculate linguistic complexity
double linguistic_complexity(const string& genome, int k) {
    if (k > genome.length()) {
        return 0.0;
    }
    
    unordered_set<string> distinct_substrings;
    int total_substrings = genome.length() - k + 1;
    
    // Extract all substrings of length k and store in set
    for (int i = 0; i <= genome.length() - k; i++) {
        string substring = genome.substr(i, k);
        distinct_substrings.insert(substring);
    }
    
    // Calculate linguistic complexity
    int distinct_count = distinct_substrings.size();
    double possible_count = pow(4, k); // 4^k possible substrings (assuming DNA alphabet)
    
    return (double)distinct_count / possible_count;
}

int main() {
    // Read input
    string genome;
    int k;
    
    cout << "Enter genome sequence: ";
    cin >> genome;
    cout << "Enter substring length k: ";
    cin >> k;
    
    // Calculate and output linguistic complexity
    double complexity = linguistic_complexity(genome, k);
    cout.precision(6);
    cout << "Linguistic complexity: " << complexity << endl;
    
    return 0;
}
```

## Explanation

1. **Input Processing**: The program reads a DNA sequence and a substring length `k`.

2. **Substring Extraction**: 
   - For a genome of length `n` and substring length `k`, there are `n-k+1` possible substrings
   - We extract each substring of length `k` using `substr()`

3. **Distinct Substrings**: 
   - We use an `unordered_set` to automatically track distinct substrings
   - The set ensures no duplicates are counted

4. **Linguistic Complexity Calculation**:
   - **Numerator**: Number of distinct substrings (size of the set)
   - **Denominator**: Total possible substrings of length `k` = 4^k (since DNA has 4 nucleotides: A, C, G, T)
   - **Result**: Ratio of distinct to possible substrings

## Example

For genome "ACGTACGT" and k=2:
- Substrings of length 2: "AC", "CG", "GT", "TA", "AC", "CG", "GT"
- Distinct substrings: {"AC", "CG", "GT", "TA"} → count = 4
- Possible substrings: 4^2 = 16
- Linguistic complexity = 4/16 = 0.25

## Time and Space Complexity
- **Time Complexity**: O(n×k) where n is genome length and k is substring length
- **Space Complexity**: O(n×k) for storing substrings in the set

The solution handles edge cases like when k is greater than genome length by returning 0.0.

