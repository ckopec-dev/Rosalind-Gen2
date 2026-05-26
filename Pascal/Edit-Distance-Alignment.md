# Rosalind Problem: Edit Distance Alignment in Pascal

## Problem Understanding
The Edit Distance Alignment problem asks us to find the minimum edit distance between two strings and output the actual alignment that achieves this distance.

## Solution Approach
We'll use dynamic programming to solve this problem:
1. Build a DP table to compute edit distance
2. Trace back through the table to construct the alignment
3. Handle insertions, deletions, and substitutions appropriately

## Pascal Implementation

```pascal
program EditDistanceAlignment;

uses
  sysutils, strutils;

const
  INF = 1000000;

var
  str1, str2: string;
  dp: array[0..1000, 0..1000] of integer;
  align1, align2: string;
  i, j: integer;

function min(a, b, c: integer): integer;
begin
  min := a;
  if b < min then min := b;
  if c < min then min := c;
end;

procedure computeEditDistance;
var
  m, n: integer;
  i, j: integer;
begin
  m := length(str1);
  n := length(str2);
  
  // Initialize base cases
  for i := 0 to m do
    dp[i, 0] := i;
  for j := 0 to n do
    dp[0, j] := j;
  
  // Fill the DP table
  for i := 1 to m do
  begin
    for j := 1 to n do
    begin
      if str1[i] = str2[j] then
        dp[i, j] := dp[i-1, j-1]
      else
        dp[i, j] := min(dp[i-1, j] + 1,     // deletion
                       dp[i, j-1] + 1,     // insertion
                       dp[i-1, j-1] + 1);  // substitution
    end;
  end;
end;

procedure traceback;
var
  i, j: integer;
  op: char;
begin
  i := length(str1);
  j := length(str2);
  align1 := '';
  align2 := '';
  
  while (i > 0) or (j > 0) do
  begin
    if (i > 0) and (j > 0) and (str1[i] = str2[j]) then
    begin
      // Match
      align1 := str1[i] + align1;
      align2 := str2[j] + align2;
      i := i - 1;
      j := j - 1;
    end
    else if (i > 0) and (j > 0) and (dp[i, j] = dp[i-1, j-1] + 1) then
    begin
      // Substitution
      align1 := str1[i] + align1;
      align2 := str2[j] + align2;
      i := i - 1;
      j := j - 1;
    end
    else if (i > 0) and (dp[i, j] = dp[i-1, j] + 1) then
    begin
      // Deletion
      align1 := str1[i] + align1;
      align2 := '-' + align2;
      i := i - 1;
    end
    else if (j > 0) and (dp[i, j] = dp[i, j-1] + 1) then
    begin
      // Insertion
      align1 := '-' + align1;
      align2 := str2[j] + align2;
      j := j - 1;
    end;
  end;
end;

procedure printAlignment;
begin
  writeln(dp[length(str1), length(str2)]);
  writeln(align1);
  writeln(align2);
end;

begin
  // Read input strings (assuming they are on separate lines)
  readln(str1);
  readln(str2);
  
  // Add a space at the beginning to make indexing easier (1-based)
  str1 := ' ' + str1;
  str2 := ' ' + str2;
  
  // Compute edit distance
  computeEditDistance;
  
  // Trace back to get alignment
  traceback;
  
  // Print result
  printAlignment;
end.
```

## Example Usage

**Input:**
```
GAGA
GAT
```

**Expected Output:**
```
2
GAGA
GAT-
```

## Key Points

1. **Dynamic Programming Table**: `dp[i,j]` represents the minimum edit distance between `str1[1..i]` and `str2[1..j]`

2. **Base Cases**: 
   - `dp[i,0] = i` (deleting i characters from str1)
   - `dp[0,j] = j` (inserting j characters to get str2)

3. **Recurrence Relation**:
   - If characters match: `dp[i,j] = dp[i-1,j-1]`
   - If different: `dp[i,j] = min(dp[i-1,j]+1, dp[i,j-1]+1, dp[i-1,j-1]+1)`

4. **Traceback**: We work backwards from `dp[m,n]` to construct the actual alignment

5. **Alignment Format**: The alignment shows how to transform the first string into the second using insertions, deletions, and substitutions

This solution has time complexity O(m×n) and space complexity O(m×n) where m and n are the lengths of the input strings.

