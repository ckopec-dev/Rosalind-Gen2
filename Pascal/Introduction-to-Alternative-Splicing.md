# Rosalind Problem: Introduction to Alternative Splicing

## Problem Statement
In this problem, we need to calculate the number of ways to choose k items from n items, where k can vary from 0 to n. This is equivalent to calculating the sum of binomial coefficients C(n,k) for k from 0 to n, which equals 2^n.

## Solution Approach
The problem asks for the number of possible alternative splicing patterns for n exons. This is mathematically equivalent to finding the sum of all binomial coefficients C(n,k) where k ranges from 0 to n, which equals 2^n.

## Pascal Implementation

```pascal
program IntroductionToAlternativeSplicing;

{$mode objfpc}{$H+}

uses
  SysUtils;

function powerOfTwo(n: integer): int64;
begin
  if n = 0 then
    powerOfTwo := 1
  else
    powerOfTwo := 2 * powerOfTwo(n - 1);
end;

function modPow(base, exp, mod: int64): int64;
var
  result: int64;
begin
  result := 1;
  base := base mod mod;
  while exp > 0 do
  begin
    if (exp mod 2) = 1 then
      result := (result * base) mod mod;
    exp := exp shr 1;
    base := (base * base) mod mod;
  end;
  modPow := result;
end;

var
  n: integer;
  result: int64;
  MOD: int64;

begin
  // Read input
  readln(n);
  
  // For large numbers, we need to take modulo
  MOD := 1000000;
  
  // The number of alternative splicing patterns is 2^n
  // But we need to take modulo 1000000
  result := modPow(2, n, MOD);
  
  writeln(result);
end.
```

## Explanation

1. **Problem Understanding**: 
   - Alternative splicing allows different combinations of exons to be included in the final mRNA
   - For n exons, each exon can either be included or excluded
   - This gives us 2^n possible combinations

2. **Mathematical Insight**:
   - The sum of all binomial coefficients C(n,k) for k=0 to n equals 2^n
   - This is a well-known combinatorial identity

3. **Implementation Details**:
   - We read the number of exons `n` from input
   - Since the result can be very large, we compute 2^n mod 1000000
   - Used fast exponentiation (modular exponentiation) to efficiently compute large powers
   - The `modPow` function implements modular exponentiation to avoid overflow

4. **Time Complexity**: O(log n) due to fast exponentiation
5. **Space Complexity**: O(1)

## Sample Input/Output
```
Input:
3

Output:
8

Explanation:
For 3 exons, there are 2^3 = 8 possible alternative splicing patterns:
- Include 0 exons: 1 way
- Include 1 exon: 3 ways  
- Include 2 exons: 3 ways
- Include 3 exons: 1 way
Total: 1 + 3 + 3 + 1 = 8 = 2^3
```

