# Rosalind Problem: Partial Permutations

## Problem Statement
Partial permutations are an generalization of permutations where we are interested in the number of ways to arrange k objects taken from a set of n objects, where the order matters.

The formula for partial permutations is: P(n,k) = n! / (n-k)!

## Solution in Pascal

```pascal
program PartialPermutations;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function Factorial(n: integer): int64;
begin
  if n <= 1 then
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;

function PartialPermutation(n, k: integer): int64;
var
  i: integer;
  result: int64;
begin
  if k > n then
  begin
    PartialPermutation := 0;
    exit;
  end;
  
  if k = 0 then
  begin
    PartialPermutation := 1;
    exit;
  end;
  
  result := 1;
  for i := n - k + 1 to n do
  begin
    result := result * i;
  end;
  
  PartialPermutation := result;
end;

var
  n, k: integer;
  result: int64;

begin
  // Read input values
  writeln('Enter n (total objects): ');
  readln(n);
  writeln('Enter k (objects to arrange): ');
  readln(k);
  
  // Calculate partial permutation
  result := PartialPermutation(n, k);
  
  // Output result
  writeln('Partial permutation P(', n, ',', k, ') = ', result);
  
  // For large numbers, we might want to take modulo
  // Since Rosalind problems typically require modulo 1000000
  writeln('Result modulo 1000000: ', result mod 1000000);
  
  readln;
end.
```

## Alternative Implementation (More Efficient)

```pascal
program PartialPermutationsOptimized;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function PartialPermutation(n, k: integer): int64;
var
  i: integer;
  result: int64;
begin
  if k > n then
  begin
    PartialPermutation := 0;
    exit;
  end;
  
  if k = 0 then
  begin
    PartialPermutation := 1;
    exit;
  end;
  
  result := 1;
  // Instead of calculating full factorials, multiply directly
  for i := n - k + 1 to n do
  begin
    result := (result * i) mod 1000000;
  end;
  
  PartialPermutation := result;
end;

var
  n, k: integer;
  result: int64;

begin
  // Read input values
  readln(n, k);
  
  // Calculate partial permutation with modulo
  result := PartialPermutation(n, k);
  
  // Output result
  writeln(result);
  
  readln;
end.
```

## Sample Input/Output

**Input:**
```
21 7
```

**Output:**
```
512000
```

## Explanation

The partial permutation P(n,k) represents the number of ways to arrange k objects from a set of n objects where order matters.

For example, with n=21 and k=7:
- We have 21 objects and want to arrange 7 of them
- P(21,7) = 21! / (21-7)! = 21! / 14! = 21 × 20 × 19 × 18 × 17 × 16 × 15 = 512000

## Key Points

1. **Efficiency**: Instead of calculating full factorials, we multiply only the necessary terms
2. **Modulo**: Rosalind problems often require results modulo 1000000
3. **Edge cases**: Handle cases where k > n (returns 0) or k = 0 (returns 1)
4. **Integer overflow**: Using `int64` to handle larger results

The optimized version directly computes the partial permutation by multiplying the terms from (n-k+1) to n, which is more efficient and avoids calculating large factorials.

