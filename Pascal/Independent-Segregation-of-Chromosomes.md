# Rosalind Problem: Independent Segregation of Chromosomes (Pascal Solution)

## Problem Understanding

The problem asks us to calculate the probability that at least one of n pairs of homologous chromosomes will separate independently during meiosis, following Mendel's law of independent assortment.

For n pairs of chromosomes, the probability that all n pairs separate independently is (1/2)^n. Therefore, the probability that at least one pair does NOT separate independently is 1 - (1/2)^n.

## Solution Approach

1. Read the number of chromosome pairs n
2. Calculate the probability using the formula: 1 - (1/2)^n
3. Output the result with appropriate precision

## Pascal Implementation

```pascal
program IndependentSegregation;

{$mode objfpc}{$H+}

uses
  SysUtils;

function calculateProbability(n: integer): double;
begin
  // Probability that at least one pair does NOT separate independently
  // = 1 - (1/2)^n
  calculateProbability := 1.0 - Power(0.5, n);
end;

var
  n: integer;
  result: double;

begin
  // Read input
  readln(n);
  
  // Calculate probability
  result := calculateProbability(n);
  
  // Output result with 6 decimal places
  writeln(Format('%.6f', [result]));
end.
```

## Alternative Implementation (More Explicit)

```pascal
program IndependentSegregation;

{$mode objfpc}{$H+}

uses
  SysUtils;

function powerOfHalf(n: integer): double;
var
  i: integer;
  result: double;
begin
  result := 1.0;
  for i := 1 to n do
    result := result * 0.5;
  powerOfHalf := result;
end;

function calculateProbability(n: integer): double;
begin
  // Probability that at least one pair does NOT separate independently
  // = 1 - (1/2)^n
  calculateProbability := 1.0 - powerOfHalf(n);
end;

var
  n: integer;
  result: double;

begin
  // Read input
  readln(n);
  
  // Calculate probability
  result := calculateProbability(n);
  
  // Output result with 6 decimal places
  writeln(Format('%.6f', [result]));
end.
```

## Explanation

The mathematical foundation of this problem is based on Mendel's law of independent assortment:

1. **Independent Assortment**: Each pair of homologous chromosomes separates independently during meiosis
2. **Probability Calculation**: 
   - For n pairs of chromosomes, the probability that all n pairs separate independently is (1/2)^n
   - The probability that at least one pair does NOT separate independently is 1 - (1/2)^n

## Sample Input/Output

**Input:**
```
3
```

**Output:**
```
0.875000
```

**Explanation:**
- For 3 pairs of chromosomes: P(all separate independently) = (1/2)^3 = 1/8 = 0.125
- P(at least one does NOT separate independently) = 1 - 0.125 = 0.875

## Time and Space Complexity

- **Time Complexity**: O(n) for the power calculation or O(1) if using built-in power function
- **Space Complexity**: O(1)

The solution handles the mathematical relationship between independent assortment and probability, which is fundamental to understanding genetic inheritance patterns.

