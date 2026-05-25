# Finding the Longest Multiple Repeat - Rosalind Problem Solution

## Problem Understanding

The task is to find the longest substring that appears at least twice in a given DNA sequence, where the repeats can overlap.

## Approach

I'll use a suffix array approach with binary search to efficiently find the longest repeated substring. The key insight is to:
1. Build a suffix array of the string
2. Find the longest common prefix (LCP) between adjacent suffixes
3. The maximum LCP value gives us the length of the longest repeated substring

## Solution

```python
def longest_multiple_repeat(s):
    """
    Find the longest substring that appears at least twice in string s.
    """
    if not s:
        return ""
    
    # Build suffix array
    suffixes = []
    for i in range(len(s)):
        suffixes.append((s[i:], i))
    
    # Sort suffixes lexicographically
    suffixes.sort()
    
    # Calculate LCP (Longest Common Prefix) array
    lcp = [0] * len(suffixes)
    
    for i in range(len(suffixes)):
        if i > 0:
            # Compare current suffix with previous suffix
            prefix_len = 0
            suffix1 = suffixes[i][0]
            suffix2 = suffixes[i-1][0]
            
            # Find common prefix length
            min_len = min(len(suffix1), len(suffix2))
            for j in range(min_len):
                if suffix1[j] == suffix2[j]:
                    prefix_len += 1
                else:
                    break
            
            lcp[i] = prefix_len
    
    # Find maximum LCP value
    max_lcp = max(lcp)
    
    if max_lcp == 0:
        return ""
    
    # Find the actual substring with maximum LCP
    max_index = lcp.index(max_lcp)
    start_pos = suffixes[max_index][1]
    
    return s[start_pos:start_pos + max_lcp]

def solve_finding_longest_multiple_repeat():
    """
    Main function to solve the problem
    """
    # Read input from file (assuming it's in a file called 'input.txt')
    try:
        with open('input.txt', 'r') as f:
            sequence = f.read().strip()
    except:
        # For testing, use a sample sequence
        sequence = "ATATCGATATCG"
    
    result = longest_multiple_repeat(sequence)
    print(result)
    
    # Write result to output file
    with open('output.txt', 'w') as f:
        f.write(result)

# Alternative simpler approach using brute force for small inputs
def longest_multiple_repeat_simple(s):
    """
    Simple brute force approach for finding longest repeated substring
    """
    if not s:
        return ""
    
    n = len(s)
    max_len = 0
    result = ""
    
    # Check all possible substrings
    for i in range(n):
        for j in range(i + 1, n + 1):
            substring = s[i:j]
            # Count occurrences
            count = 0
            start = 0
            while True:
                pos = s.find(substring, start)
                if pos == -1:
                    break
                count += 1
                start = pos + 1
            
            # If appears at least twice and is longer than current max
            if count >= 2 and len(substring) > max_len:
                max_len = len(substring)
                result = substring
    
    return result

# More efficient approach using suffix array with better implementation
def longest_repeated_substring(s):
    """
    More efficient implementation using suffix array and LCP
    """
    if not s:
        return ""
    
    # Create suffix array
    suffixes = []
    for i in range(len(s)):
        suffixes.append(s[i:])
    
    # Sort suffixes
    suffixes.sort()
    
    # Calculate LCP array
    lcp = [0] * len(suffixes)
    
    for i in range(len(suffixes)):
        if i > 0:
            # Compare current suffix with previous one
            common_len = 0
            min_len = min(len(suffixes[i]), len(suffixes[i-1]))
            for j in range(min_len):
                if suffixes[i][j] == suffixes[i-1][j]:
                    common_len += 1
                else:
                    break
            lcp[i] = common_len
    
    # Find maximum LCP
    max_lcp = max(lcp)
    
    if max_lcp == 0:
        return ""
    
    # Find the actual substring
    max_index = lcp.index(max_lcp)
    # Get the starting position from the suffix array
    start_pos = 0
    for i in range(len(suffixes)):
        if len(suffixes[i]) >= max_lcp:
            # Check if this suffix matches with the previous one
            if i > 0 and len(suffixes[i-1]) >= max_lcp:
                if suffixes[i][:max_lcp] == suffixes[i-1][:max_lcp]:
                    # Find the actual start position in original string
                    # This is a simplified approach - in practice, you'd track positions
                    return suffixes[i][:max_lcp]
    
    return ""

# Complete working solution
def find_longest_multiple_repeat(s):
    """
    Complete solution for finding longest multiple repeat
    """
    if not s:
        return ""
    
    # Brute force approach for simplicity and correctness
    max_length = 0
    result = ""
    
    # Try all possible substrings
    for i in range(len(s)):
        for j in range(i + 1, len(s) + 1):
            substring = s[i:j]
            # Count how many times this substring appears
            count = 0
            start = 0
            while True:
                pos = s.find(substring, start)
                if pos == -1:
                    break
                count += 1
                start = pos + 1
            
            # If appears at least twice and is longer than current result
            if count >= 2 and len(substring) > max_length:
                max_length = len(substring)
                result = substring
    
    return result

# Test with example
if __name__ == "__main__":
    # Test case
    test_sequence = "ATATCGATATCG"
    result = find_longest_multiple_repeat(test_sequence)
    print(f"Input: {test_sequence}")
    print(f"Longest multiple repeat: {result}")
    
    # For Rosalind input, read from file
    try:
        with open('rosalind_lrep.txt', 'r') as f:
            sequence = f.read().strip()
        result = find_longest_multiple_repeat(sequence)
        print(f"Rosalind result: {result}")
    except:
        pass
```

## Explanation

The solution uses a brute-force approach that:

1. **Generates all possible substrings** of the input DNA sequence
2. **Counts occurrences** of each substring in the sequence
3. **Keeps track of the longest substring** that appears at least twice

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the string
- **Space Complexity**: O(n²) for storing all substrings

## Usage

For Rosalind problems, save your input sequence in a file named `rosalind_lrep.txt` and run the script. The output will be written to `output.txt`.

```bash
python longest_multiple_repeat.py
```

The solution handles overlapping repeats correctly and will return the longest substring that appears at least twice in the given DNA sequence.

