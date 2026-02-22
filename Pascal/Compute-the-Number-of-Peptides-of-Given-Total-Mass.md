# Rosalind Problem: Compute the Number of Peptides of Given Total Mass

## Problem Statement
Given an integer n, compute the number of peptides of total mass n, where each peptide is composed of amino acid masses from the standard amino acid mass table.

## Solution in Pascal

```pascal
program ComputeNumberOfPeptides;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function CountPeptides(mass: integer): integer;
var
  dp: array[0..10000] of longint;
  i, j: integer;
  masses: array[1..20] of integer;
begin
  // Standard amino acid masses (rounded to integers)
  masses[1] := 57;   // A
  masses[2] := 71;   // C
  masses[3] := 87;   // D
  masses[4] := 97;   // E
  masses[5] := 99;   // F
  masses[6] := 101;  // G
  masses[7] := 103;  // H
  masses[8] := 113;  // I
  masses[9] := 114;  // K
  masses[10] := 115; // L
  masses[11] := 128; // M
  masses[12] := 129; // N
  masses[13] := 131; // P
  masses[14] := 137; // Q
  masses[15] := 147; // R
  masses[16] := 156; // S
  masses[17] := 163; // T
  masses[18] := 186; // W
  masses[19] := 188; // Y
  masses[20] := 114; // V (Note: this is actually 114, but we'll use 114 for V)

  // Initialize dp array
  for i := 0 to 10000 do
    dp[i] := 0;
    
  dp[0] := 1; // One way to make mass 0 (empty peptide)
  
  // Fill dp array using dynamic programming
  for i := 1 to mass do
  begin
    for j := 1 to 20 do
    begin
      if masses[j] <= i then
        dp[i] := dp[i] + dp[i - masses[j]];
    end;
  end;
  
  CountPeptides := dp[mass];
end;

var
  n: integer;
begin
  // Read input
  readln(n);
  
  // Compute and output result
  writeln(CountPeptides(n));
  
  readln; // Wait for user input before closing
end.
```

## Explanation

This solution uses dynamic programming to count the number of ways to form a given mass using the standard amino acid masses.

### Approach:
1. **Dynamic Programming**: Create a `dp` array where `dp[i]` represents the number of ways to form mass `i`
2. **Base Case**: `dp[0] = 1` (one way to make mass 0 - the empty peptide)
3. **Recurrence Relation**: For each mass `i`, iterate through all amino acid masses and add the number of ways to form `i - mass` to `dp[i]`
4. **Result**: `dp[n]` gives the number of peptides of total mass `n`

### Time Complexity: O(n × 20) = O(n)
### Space Complexity: O(n)

### Note:
The solution uses the standard amino acid masses (rounded to integers) as given in the Rosalind problem. The masses are:
- A: 57, C: 71, D: 87, E: 97, F: 99, G: 101, H: 103, I: 113, K: 114, L: 115, M: 128, N: 129, P: 131, Q: 137, R: 147, S: 156, T: 163, W: 186, Y: 188, V: 114

This approach correctly handles the combinatorial nature of peptide formation where the order of amino acids matters in the counting.

