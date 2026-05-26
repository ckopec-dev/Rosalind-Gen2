# Rosalind Problem: Find the Shortest Non-Shared Substring of Two Strings

## Problem Understanding

We need to find the shortest substring that appears in one string but not in the other string.

## Approach

1. Generate all substrings of both strings
2. Find substrings that exist in one string but not in the other
3. Return the shortest such substring

## Solution

```cpp
#include <iostream>
#include <string>
#include <set>
#include <algorithm>
using namespace std;

// Function to generate all substrings of a string
set<string> getAllSubstrings(const string& s) {
    set<string> substrings;
    int n = s.length();
    
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j <= n; j++) {
            substrings.insert(s.substr(i, j - i));
        }
    }
    
    return substrings;
}

// Function to find shortest non-shared substring
string findShortestNonSharedSubstring(const string& s1, const string& s2) {
    // Get all substrings of both strings
    set<string> subs1 = getAllSubstrings(s1);
    set<string> subs2 = getAllSubstrings(s2);
    
    // Find non-shared substrings
    string result = "";
    int minLength = INT_MAX;
    
    // Check each substring from s1
    for (const string& sub : subs1) {
        // If this substring is not in s2
        if (subs2.find(sub) == subs2.end()) {
            if (sub.length() < minLength) {
                minLength = sub.length();
                result = sub;
            }
        }
    }
    
    // Check each substring from s2
    for (const string& sub : subs2) {
        // If this substring is not in s1
        if (subs1.find(sub) == subs1.end()) {
            if (sub.length() < minLength) {
                minLength = sub.length();
                result = sub;
            }
        }
    }
    
    return result;
}

int main() {
    // Read input strings
    string s1, s2;
    cin >> s1 >> s2;
    
    // Find and print the shortest non-shared substring
    string result = findShortestNonSharedSubstring(s1, s2);
    cout << result << endl;
    
    return 0;
}
```

## Explanation

1. **getAllSubstrings function**: Generates all possible substrings of a given string and stores them in a set for efficient lookup.

2. **findShortestNonSharedSubstring function**: 
   - Gets all substrings of both input strings
   - For each substring from the first string, checks if it exists in the second string
   - For each substring from the second string, checks if it exists in the first string
   - Keeps track of the shortest substring that is not shared between the two strings

3. **Time Complexity**: O(n³) where n is the length of the strings, due to generating all substrings and checking membership.

4. **Space Complexity**: O(n³) for storing all substrings.

## Example

Input:
```
ABCD
BCD
```

Output:
```
A
```

Explanation: "A" is a substring of "ABCD" but not of "BCD", and it's the shortest such substring.

