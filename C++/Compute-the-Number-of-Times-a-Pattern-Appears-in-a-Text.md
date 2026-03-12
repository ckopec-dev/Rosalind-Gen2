# Rosalind Problem: Compute the Number of Times a Pattern Appears in a Text

## Problem Statement
Given two strings `pattern` and `text`, we need to count how many times `pattern` appears in `text` as a substring, including overlapping occurrences.

## Solution

```cpp
#include <iostream>
#include <string>
using namespace std;

int countPattern(const string& pattern, const string& text) {
    int count = 0;
    size_t pos = 0;
    
    // Search for pattern in text starting from each position
    while ((pos = text.find(pattern, pos)) != string::npos) {
        count++;
        pos++; // Move by 1 to find overlapping patterns
    }
    
    return count;
}

int main() {
    string pattern, text;
    
    // Read input
    cin >> pattern >> text;
    
    // Count occurrences
    int result = countPattern(pattern, text);
    
    // Output result
    cout << result << endl;
    
    return 0;
}
```

## Explanation

1. **Function `countPattern`**: 
   - Takes `pattern` and `text` as input parameters
   - Uses `string::find()` to locate occurrences of pattern in text
   - Starts searching from position `pos` and increments `pos` by 1 after each match to catch overlapping patterns
   - Continues until no more matches are found

2. **Key Points**:
   - The `find()` function returns `string::npos` when no more matches are found
   - By incrementing `pos` by 1 (not by pattern length), we ensure overlapping patterns are counted
   - The function handles edge cases like empty strings naturally

3. **Time Complexity**: O(n×m) where n is the length of text and m is the length of pattern
4. **Space Complexity**: O(1) additional space

## Example
Input:
```
ATA
CGATATATCCATAG
```

Output:
```
3
```

The pattern "ATA" appears at positions 2, 4, and 7 in the text "CGATATATCCATAG", giving us a count of 3.

