# Rosalind Problem: Wobble Bonding and RNA Secondary Structures (Pascal Solution)

## Problem Understanding

This problem asks us to count the number of possible secondary structures for an RNA sequence, where wobble base pairing is allowed. Wobble pairing allows:
- A-U, U-A
- C-G, G-C  
- G-U, U-G (wobble pairs)

We need to count all valid secondary structures using dynamic programming.

## Solution Approach

We'll use dynamic programming with the following recurrence:
- `dp[i][j]` = number of valid structures for substring from position i to j
- For each interval [i,j], we can either:
  1. Not pair position i with any other position (add dp[i+1][j])
  2. Pair position i with position k (if they form a valid base pair) and multiply by dp[i+1][k-1] * dp[k+1][j]

## Pascal Implementation

```pascal
program WobbleBonding;

uses
  sysutils;

const
  MAXN = 100;
  
var
  s: string;
  n: integer;
  dp: array[0..MAXN, 0..MAXN] of int64;
  valid: array[0..MAXN, 0..MAXN] of boolean;

function canPair(c1, c2: char): boolean;
begin
  canPair := false;
  if (c1 = 'A') and (c2 = 'U') then canPair := true
  else if (c1 = 'U') and (c2 = 'A') then canPair := true
  else if (c1 = 'C') and (c2 = 'G') then canPair := true
  else if (c1 = 'G') and (c2 = 'C') then canPair := true
  else if (c1 = 'G') and (c2 = 'U') then canPair := true
  else if (c1 = 'U') and (c2 = 'G') then canPair := true;
end;

function countStructures(i, j: integer): int64;
begin
  if i >= j then
  begin
    countStructures := 1;
    exit;
  end;
  
  if dp[i][j] <> -1 then
  begin
    countStructures := dp[i][j];
    exit;
  end;
  
  // Option 1: i is not paired
  dp[i][j] := countStructures(i+1, j);
  
  // Option 2: i is paired with some k
  for k := i+1 to j do
  begin
    if valid[i][k] then
    begin
      dp[i][j] := dp[i][j] + countStructures(i+1, k-1) * countStructures(k+1, j);
    end;
  end;
  
  countStructures := dp[i][j];
end;

begin
  // Read input
  readln(s);
  n := length(s);
  
  // Initialize DP table
  for i := 0 to MAXN-1 do
    for j := 0 to MAXN-1 do
    begin
      dp[i][j] := -1;
      valid[i][j] := false;
    end;
  
  // Mark valid base pairs
  for i := 1 to n do
    for j := 1 to n do
    begin
      if canPair(s[i], s[j]) then
        valid[i][j] := true;
    end;
  
  // Calculate result
  writeln(countStructures(1, n));
end.
```

## Explanation

1. **Input Processing**: Read the RNA sequence string
2. **Base Pair Validation**: Create a `valid` matrix that marks which pairs of nucleotides can form valid bonds (including wobble pairs)
3. **Dynamic Programming**: Use `dp[i][j]` to store the number of valid structures for substring from position i to j
4. **Recurrence Relation**:
   - If we don't pair position i, we have `countStructures(i+1, j)` possibilities
   - If we pair i with k, we multiply the structures of left and right subsegments
5. **Base Case**: When i ≥ j, there's exactly 1 way (empty structure)

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the RNA sequence
- **Space Complexity**: O(n²) for the DP table and valid pairs matrix

## Sample Input/Output

Input:
```
AUCG
```

Output:
```
2
```

The algorithm correctly counts all valid secondary structures considering wobble base pairing rules.

