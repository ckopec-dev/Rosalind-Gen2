# Rosalind Problem: Enumerating Gene Orders (PERM)

## Problem Description
Given a positive integer n ≤ 7, return the total number of permutations of length n, followed by a list of all such permutations (in any order).

## Solution in Pascal

```pascal
program EnumeratingGeneOrders;

{$APPTYPE CONSOLE}

uses
  SysUtils, Math;

var
  n, i, j, k, totalPerms: integer;
  perm, temp: array[1..10] of integer;
  used: array[1..10] of boolean;

procedure GeneratePermutations(pos: integer);
var
  i: integer;
begin
  if pos > n then
  begin
    // Print current permutation
    for i := 1 to n do
      write(perm[i], ' ');
    writeln;
    exit;
  end;
  
  for i := 1 to n do
  begin
    if not used[i] then
    begin
      used[i] := true;
      perm[pos] := i;
      GeneratePermutations(pos + 1);
      used[i] := false;
    end;
  end;
end;

begin
  // Read input
  readln(n);
  
  // Calculate total number of permutations
  totalPerms := 1;
  for i := 1 to n do
    totalPerms := totalPerms * i;
  
  // Print total number of permutations
  writeln(totalPerms);
  
  // Generate and print all permutations
  for i := 1 to n do
    used[i] := false;
  
  GeneratePermutations(1);
  
  readln;
end.
```

## How It Works

1. **Input Reading**: Read the integer `n` from standard input
2. **Calculate Total Permutations**: The total number of permutations of n elements is n! (n factorial)
3. **Backtracking Algorithm**: 
   - Use a recursive function `GeneratePermutations` to build permutations
   - Keep track of which numbers have been used with the `used` array
   - For each position, try all unused numbers
   - When a complete permutation is built (position > n), print it
4. **Output**: First print the total count, then all permutations

## Example

For input `n = 3`:
- Output: `6` (total permutations)
- Then all 6 permutations:
  ```
  1 2 3 
  1 3 2 
  2 1 3 
  2 3 1 
  3 1 2 
  3 2 1 
  ```

## Time and Space Complexity

- **Time Complexity**: O(n! × n) - we generate n! permutations, each of length n
- **Space Complexity**: O(n) - for the arrays and recursion stack

The solution handles the constraint that n ≤ 7 efficiently, as 7! = 5040 which is manageable for this approach.

