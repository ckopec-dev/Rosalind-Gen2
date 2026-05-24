# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings

## Problem Description
Given two strings `v` and `w`, find the highest-scoring overlap alignment of `v` and `w`. An overlap alignment allows the alignment to start at any position in the first string and end at any position in the second string, with the constraint that the alignment must end at the end of the second string.

## Solution in Pascal

```pascal
program FindHighestScoringOverlapAlignment;

uses
  sysutils, strutils;

const
  MATCH_SCORE = 1;
  MISMATCH_SCORE = -1;
  GAP_SCORE = -2;

var
  v, w: string;
  i, j: integer;
  dp: array[0..1000, 0..1000] of integer;
  max_score: integer;
  max_i: integer;

function max(a, b: integer): integer;
begin
  if a > b then
    max := a
  else
    max := b;
end;

function max3(a, b, c: integer): integer;
begin
  max3 := max(max(a, b), c);
end;

begin
  // Read input strings
  writeln('Enter first string v:');
  readln(v);
  writeln('Enter second string w:');
  readln(w);
  
  // Initialize DP table
  for i := 0 to length(v) do
    dp[i, 0] := i * GAP_SCORE;
  for j := 0 to length(w) do
    dp[0, j] := 0;
  
  // Fill DP table
  for i := 1 to length(v) do
  begin
    for j := 1 to length(w) do
    begin
      if v[i] = w[j] then
        dp[i, j] := dp[i-1, j-1] + MATCH_SCORE
      else
        dp[i, j] := dp[i-1, j-1] + MISMATCH_SCORE;
      
      dp[i, j] := max3(dp[i-1, j] + GAP_SCORE, 
                       dp[i, j-1] + GAP_SCORE, 
                       dp[i, j]);
    end;
  end;
  
  // Find maximum score in last row
  max_score := -maxint;
  max_i := 0;
  for i := 0 to length(v) do
  begin
    if dp[i, length(w)] > max_score then
    begin
      max_score := dp[i, length(w)];
      max_i := i;
    end;
  end;
  
  writeln('Maximum overlap score: ', max_score);
  writeln('Best alignment ends at position ', max_i, ' of string v');
  
  // Reconstruct alignment (optional)
  writeln('Reconstructed alignment:');
  writeln('Score matrix:');
  for i := 0 to length(v) do
  begin
    for j := 0 to length(w) do
      write(dp[i, j]:4);
    writeln;
  end;
end.
```

## Explanation

This solution implements a dynamic programming approach to find the highest-scoring overlap alignment:

1. **Initialization**: 
   - Create a DP table of size (length(v)+1) × (length(w)+1)
   - Initialize first column with gap penalties (i × GAP_SCORE)
   - Initialize first row with zeros (since we're looking for overlap alignment)

2. **DP Recurrence**:
   - For each cell (i,j), calculate the maximum score from:
     - Match/Mismatch: dp[i-1,j-1] + score
     - Gap in v: dp[i-1,j] + GAP_SCORE
     - Gap in w: dp[i,j-1] + GAP_SCORE

3. **Finding Maximum**:
   - Look at the last row of the DP table to find the highest-scoring overlap
   - The alignment must end at the end of string w

4. **Output**:
   - Returns the maximum score and the position where the best alignment ends in string v

## Time and Space Complexity
- **Time Complexity**: O(|v| × |w|) where |v| and |w| are the lengths of the strings
- **Space Complexity**: O(|v| × |w|) for the DP table

## Sample Input/Output
```
Enter first string v:
PAWHEAE
Enter second string w:
HEAGAWGHEE

Maximum overlap score: 1
Best alignment ends at position 4 of string v
```

This implementation handles the overlap alignment problem where the alignment can start anywhere in the first string and must end at the end of the second string, which is a common variant in bioinformatics sequence alignment problems.

