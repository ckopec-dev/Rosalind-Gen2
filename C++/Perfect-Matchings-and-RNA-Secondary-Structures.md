# Rosalind Problem: Perfect Matchings and RNA Secondary Structures

## Problem Understanding

In this problem, we need to find the number of perfect matchings in an RNA string where:
- The RNA string has equal numbers of A and U, and equal numbers of G and C
- A perfect matching means every nucleotide is paired with exactly one other nucleotide
- The pairing follows Watson-Crick base pairing rules: A-U, U-A, G-C, C-G

## Approach

1. Count the number of each nucleotide type
2. For perfect matching to exist, we need equal numbers of complementary pairs
3. The number of perfect matchings is the product of factorials of the counts of each pair type

## Solution

```cpp
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <fstream>

using namespace std;

// Function to calculate factorial
long long factorial(int n) {
    if (n <= 1) return 1;
    long long result = 1;
    for (int i = 2; i <= n; i++) {
        result *= i;
    }
    return result;
}

// Function to count nucleotides and calculate perfect matchings
long long countPerfectMatchings(const string& RNA) {
    map<char, int> nucleotide_count;
    
    // Count each nucleotide
    for (char nucleotide : RNA) {
        nucleotide_count[nucleotide]++;
    }
    
    // For perfect matching, we need equal numbers of complementary pairs
    // A must equal U, G must equal C
    int a_count = nucleotide_count['A'];
    int u_count = nucleotide_count['U'];
    int g_count = nucleotide_count['G'];
    int c_count = nucleotide_count['C'];
    
    // Check if perfect matching is possible
    if (a_count != u_count || g_count != c_count) {
        return 0; // Impossible to have perfect matching
    }
    
    // Number of perfect matchings = a! * g! (since we pair A with U and G with C)
    return factorial(a_count) * factorial(g_count);
}

int main() {
    // Read input from file
    ifstream infile("rosalind_pmch.txt");
    string RNA;
    
    if (infile.is_open()) {
        string line;
        getline(infile, line); // Skip first line (header)
        while (getline(infile, line)) {
            RNA += line;
        }
        infile.close();
    }
    
    // Alternative: read from stdin for testing
    // getline(cin, RNA);
    
    // Calculate and output result
    long long result = countPerfectMatchings(RNA);
    cout << result << endl;
    
    return 0;
}
```

## Explanation

The key insight is that for a perfect matching in an RNA secondary structure:

1. **Constraint Check**: For perfect matching to be possible, we must have equal numbers of A and U, and equal numbers of G and C.

2. **Mathematical Formula**: 
   - If there are `n` A's and `n` U's, the number of ways to pair them is `n!`
   - If there are `m` G's and `m` C's, the number of ways to pair them is `m!`
   - Total perfect matchings = `n! × m!`

3. **Implementation Details**:
   - Count each nucleotide type
   - Verify that complementary pairs are equal
   - Calculate the factorial of each pair count
   - Return the product of factorials

## Example

For RNA string "AUGCUAUGCU":
- A: 2, U: 2, G: 2, C: 2
- Perfect matchings = 2! × 2! = 2 × 2 = 4

## Time Complexity
- O(n) for counting nucleotides
- O(n) for calculating factorials
- Overall: O(n) where n is the length of RNA string

## Space Complexity
- O(1) for the map (at most 4 entries)
- O(1) for other variables
- Overall: O(1)

