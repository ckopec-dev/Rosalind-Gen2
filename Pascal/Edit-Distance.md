# Rosalind Problem: Edit Distance (Edit_Distance) - Pascal Solution

## Problem Understanding

The edit distance (also known as Levenshtein distance) between two strings is the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into the other.

## Solution Approach

I'll use dynamic programming to solve this problem. The approach is to build a matrix where `dp[i][j]` represents the edit distance between the first `i` characters of string1 and the first `j` characters of string2.

## Pascal Implementation

```pascal
program EditDistance;

uses
  SysUtils;

function min(a, b, c: integer): integer;
begin
  if (a <= b) and (a <= c) then
    min := a
  else if b <= c then
    min := b
  else
    min := c;
end;

function editDistance(s1, s2: string): integer;
var
  i, j: integer;
  dp: array[0..1000, 0..1000] of integer;
  len1, len2: integer;
begin
  len1 := Length(s1);
  len2 := Length(s2);
  
  // Initialize base cases
  for i := 0 to len1 do
    dp[i][0] := i;
    
  for j := 0 to len2 do
    dp[0][j] := j;
  
  // Fill the dp table
  for i := 1 to len1 do
  begin
    for j := 1 to len2 do
    begin
      if s1[i] = s2[j] then
        dp[i][j] := dp[i-1][j-1]  // No operation needed
      else
        dp[i][j] := 1 + min(dp[i-1][j],     // deletion
                           dp[i][j-1],     // insertion
                           dp[i-1][j-1]);  // substitution
    end;
  end;
  
  editDistance := dp[len1][len2];
end;

var
  str1, str2: string;
begin
  // Read input strings
  Readln(str1);
  Readln(str2);
  
  // Output the edit distance
  Writeln(editDistance(str1, str2));
end.
```

## Explanation

1. **Function `min`**: Helper function to find the minimum of three integers
2. **Function `editDistance`**: Main function that computes the edit distance using dynamic programming:
   - Create a 2D array `dp` where `dp[i][j]` stores the edit distance between first `i` characters of `s1` and first `j` characters of `s2`
   - Initialize base cases: `dp[i][0] = i` (deleting all characters from s1) and `dp[0][j] = j` (inserting all characters to get s2)
   - Fill the table using the recurrence relation:
     - If characters match: `dp[i][j] = dp[i-1][j-1]`
     - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

## Example Usage

**Input:**
```
GAGA
GATA
```

**Output:**
```
1
```

**Explanation:**
The edit distance between "GAGA" and "GATA" is 1, as we need only one substitution (change 'G' to 'T' in the first position of "GAGA" to get "GATA").

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming table

This solution efficiently computes the minimum edit distance between two strings using the classic dynamic programming approach.

