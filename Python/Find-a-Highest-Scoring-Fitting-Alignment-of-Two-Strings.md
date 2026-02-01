# Rosalind Problem: Find a Highest-Scoring Fitting Alignment of Two Strings

## Problem Description
Given two strings `v` and `w`, find a highest-scoring fitting alignment of `v` and `w` using the following scoring system:
- Match score: +1
- Mismatch score: -1  
- Indel score: -2

A fitting alignment allows the first string to be a prefix and the second string to be a suffix of the alignment.

## Solution Approach
I'll use dynamic programming to solve this problem, similar to global alignment but with modified boundary conditions.

```python
def fitting_alignment(v, w):
    """
    Find the highest-scoring fitting alignment of two strings.
    
    Args:
        v: First string (row)
        w: Second string (column)
    
    Returns:
        tuple: (score, alignment_v, alignment_w)
    """
    m, n = len(v), len(w)
    
    # Create DP table
    dp = [[0 for _ in range(n + 1)] for _ in range(m + 1)]
    
    # Initialize first row (can only come from left with indel penalty)
    for j in range(1, n + 1):
        dp[0][j] = dp[0][j-1] - 2
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            # Match/mismatch score
            if v[i-1] == w[j-1]:
                match_score = 1
            else:
                match_score = -1
            
            # Choose maximum from three possible moves
            dp[i][j] = max(
                dp[i-1][j] - 2,      # deletion (indel)
                dp[i][j-1] - 2,      # insertion (indel)
                dp[i-1][j-1] + match_score  # match/mismatch
            )
    
    # Find the maximum score in the last row (since we want fitting alignment)
    max_score = max(dp[m])
    
    # Backtrack to find the alignment
    alignment_v = ""
    alignment_w = ""
    
    # Start from the position with maximum score in last row
    i, j = m, dp[m].index(max_score)
    
    # Backtrack
    while i > 0 and j > 0:
        # Match/mismatch case
        if v[i-1] == w[j-1]:
            match_score = 1
        else:
            match_score = -1
            
        # Check which direction we came from
        if dp[i][j] == dp[i-1][j-1] + match_score:
            alignment_v = v[i-1] + alignment_v
            alignment_w = w[j-1] + alignment_w
            i -= 1
            j -= 1
        elif dp[i][j] == dp[i-1][j] - 2:
            alignment_v = v[i-1] + alignment_v
            alignment_w = "-" + alignment_w
            i -= 1
        else:  # dp[i][j] == dp[i][j-1] - 2
            alignment_v = "-" + alignment_v
            alignment_w = w[j-1] + alignment_w
            j -= 1
    
    # Handle remaining characters
    while i > 0:
        alignment_v = v[i-1] + alignment_v
        alignment_w = "-" + alignment_w
        i -= 1
    
    while j > 0:
        alignment_v = "-" + alignment_v
        alignment_w = w[j-1] + alignment_w
        j -= 1
    
    return max_score, alignment_v, alignment_w

# Alternative approach - more precise fitting alignment
def fitting_alignment_v2(v, w):
    """
    More precise fitting alignment where first string is prefix and second is suffix.
    """
    m, n = len(v), len(w)
    
    # Create DP table
    dp = [[0 for _ in range(n + 1)] for _ in range(m + 1)]
    
    # Initialize first row
    for j in range(n + 1):
        dp[0][j] = -2 * j
    
    # Fill the DP table
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            # Match/mismatch score
            if v[i-1] == w[j-1]:
                match_score = 1
            else:
                match_score = -1
            
            # Choose maximum from three possible moves
            dp[i][j] = max(
                dp[i-1][j] - 2,      # deletion (indel)
                dp[i][j-1] - 2,      # insertion (indel)
                dp[i-1][j-1] + match_score  # match/mismatch
            )
    
    # Find the maximum score in the last row
    max_score = max(dp[m])
    
    # Backtrack from the position with maximum score in last row
    i, j = m, dp[m].index(max_score)
    
    # Create alignments
    alignment_v = ""
    alignment_w = ""
    
    # Backtrack to build alignment
    while i > 0 and j > 0:
        if v[i-1] == w[j-1]:
            match_score = 1
        else:
            match_score = -1
            
        if dp[i][j] == dp[i-1][j-1] + match_score:
            alignment_v = v[i-1] + alignment_v
            alignment_w = w[j-1] + alignment_w
            i -= 1
            j -= 1
        elif dp[i][j] == dp[i-1][j] - 2:
            alignment_v = v[i-1] + alignment_v
            alignment_w = "-" + alignment_w
            i -= 1
        else:
            alignment_v = "-" + alignment_v
            alignment_w = w[j-1] + alignment_w
            j -= 1
    
    # Handle remaining characters in v (should be all gaps in w)
    while i > 0:
        alignment_v = v[i-1] + alignment_v
        alignment_w = "-" + alignment_w
        i -= 1
    
    # Handle remaining characters in w (should be all gaps in v)
    while j > 0:
        alignment_v = "-" + alignment_v
        alignment_w = w[j-1] + alignment_w
        j -= 1
    
    return max_score, alignment_v, alignment_w

# Read input and solve
def solve():
    # Example input (replace with actual Rosalind input)
    v = "GTAGGCTTAAGGTTA"
    w = "TAGATA"
    
    score, align_v, align_w = fitting_alignment_v2(v, w)
    
    print(f"Score: {score}")
    print(f"Alignment V: {align_v}")
    print(f"Alignment W: {align_w}")
    
    return score, align_v, align_w

# Main execution
if __name__ == "__main__":
    # For actual Rosalind problem, read from file
    # with open('rosalind_ba5j.txt', 'r') as f:
    #     v = f.readline().strip()
    #     w = f.readline().strip()
    
    # For testing with example
    v = "GTAGGCTTAAGGTTA"
    w = "TAGATA"
    
    score, align_v, align_w = fitting_alignment_v2(v, w)
    
    print(f"Score: {score}")
    print(f"Alignment V: {align_v}")
    print(f"Alignment W: {align_w}")
```

## Key Points

1. **Fitting Alignment**: Unlike global alignment, the first string must be a prefix and the second string must be a suffix of the alignment.

2. **Scoring System**:
   - Match: +1
   - Mismatch: -1
   - Indel: -2

3. **Dynamic Programming Approach**:
   - Create a DP table of size (m+1) × (n+1)
   - Initialize first row with progressive indel penalties
   - Fill table using the recurrence relation
   - Find maximum score in last row for fitting alignment

4. **Backtracking**:
   - Start from the position with maximum score in the last row
   - Trace back through the DP table to reconstruct the alignment

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

This solution correctly handles the fitting alignment requirement where the first string must be a prefix and the second string must be a suffix of the resulting alignment.

