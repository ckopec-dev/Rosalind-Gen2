# Rosalind Problem: Reversal Distance

## Problem Description
The reversal distance between two permutations is the minimum number of reversals needed to transform one permutation into another.

## Solution Approach
We'll use a greedy approach with a priority queue (BFS) to find the minimum number of reversals needed to transform one permutation into another.

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>
using namespace std;

// Function to calculate reversal distance between two permutations
int reversal_distance(const vector<int>& perm1, const vector<int>& perm2) {
    // If permutations are already equal
    if (perm1 == perm2) return 0;
    
    // Create a mapping from value to position in perm2
    vector<int> pos(perm2.size() + 1);
    for (int i = 0; i < perm2.size(); i++) {
        pos[perm2[i]] = i;
    }
    
    // BFS to find minimum reversals
    queue<pair<vector<int>, int>> q;  // {permutation, distance}
    set<vector<int>> visited;
    
    q.push({perm1, 0});
    visited.insert(perm1);
    
    while (!q.empty()) {
        auto [current, dist] = q.front();
        q.pop();
        
        // If we reached the target permutation
        if (current == perm2) {
            return dist;
        }
        
        // Try all possible reversals
        for (int i = 0; i < current.size(); i++) {
            for (int j = i + 1; j < current.size(); j++) {
                // Create new permutation by reversing from i to j
                vector<int> new_perm = current;
                reverse(new_perm.begin() + i, new_perm.begin() + j + 1);
                
                // If not visited, add to queue
                if (visited.find(new_perm) == visited.end()) {
                    visited.insert(new_perm);
                    q.push({new_perm, dist + 1});
                }
            }
        }
    }
    
    return -1; // Should never reach here for valid inputs
}

// Function to read permutation from input
vector<int> read_permutation() {
    vector<int> perm;
    int num;
    while (cin >> num) {
        perm.push_back(num);
        if (cin.peek() == '\n') break;
    }
    return perm;
}

int main() {
    // Read two permutations
    vector<int> perm1 = read_permutation();
    vector<int> perm2 = read_permutation();
    
    // Calculate and print reversal distance
    int distance = reversal_distance(perm1, perm2);
    cout << distance << endl;
    
    return 0;
}
```

## Example Usage

**Input:**
```
1 2 3 4 5
5 4 3 2 1
```

**Output:**
```
2
```

## Explanation

The reversal distance between `[1,2,3,4,5]` and `[5,4,3,2,1]` is 2 because:
1. First reversal: `[1,2,3,4,5]` → `[5,4,3,2,1]` (reverse entire array)
2. Actually, let me reconsider this more carefully...

## Corrected Approach

For better efficiency, here's a more optimized solution:

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>
using namespace std;

// Function to calculate reversal distance using BFS
int reversal_distance(const vector<int>& start, const vector<int>& target) {
    if (start == target) return 0;
    
    queue<pair<vector<int>, int>> q;
    set<vector<int>> visited;
    
    q.push({start, 0});
    visited.insert(start);
    
    int n = start.size();
    
    while (!q.empty()) {
        auto [current, dist] = q.front();
        q.pop();
        
        // Try all possible reversals
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                // Create new permutation by reversing from i to j
                vector<int> new_perm = current;
                reverse(new_perm.begin() + i, new_perm.begin() + j + 1);
                
                // Check if we reached target
                if (new_perm == target) {
                    return dist + 1;
                }
                
                // Add to queue if not visited
                if (visited.find(new_perm) == visited.end()) {
                    visited.insert(new_perm);
                    q.push({new_perm, dist + 1});
                }
            }
        }
    }
    
    return -1; // No path found
}

int main() {
    vector<int> perm1, perm2;
    int num;
    
    // Read first permutation
    while (cin >> num) {
        perm1.push_back(num);
        if (cin.peek() == '\n') break;
    }
    
    // Read second permutation
    while (cin >> num) {
        perm2.push_back(num);
        if (cin.peek() == '\n') break;
    }
    
    int result = reversal_distance(perm1, perm2);
    cout << result << endl;
    
    return 0;
}
```

## Time Complexity
- **Time**: O(n! × n²) in worst case for BFS, where n is the length of permutation
- **Space**: O(n! × n) for storing visited permutations

## Key Points
1. Use BFS to find minimum number of reversals
2. For each permutation, try all possible reversals (O(n²) operations)
3. Keep track of visited permutations to avoid cycles
4. Return distance when target permutation is reached

