# Rosalind Problem: Enumerating Unrooted Binary Trees

## Problem Description
Given a collection of n taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

For n taxa, the number of distinct unrooted binary trees is given by the formula: (2n-5)!! (double factorial)

## Solution in Pascal

```pascal
program EnumeratingUnrootedBinaryTrees;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Function to calculate double factorial
function DoubleFactorial(n: Integer): Int64;
begin
  if n <= 0 then
    DoubleFactorial := 1
  else if n = 1 then
    DoubleFactorial := 1
  else if n = 2 then
    DoubleFactorial := 2
  else
    DoubleFactorial := n * DoubleFactorial(n - 2);
end;

// Function to calculate number of unrooted binary trees
function CountUnrootedBinaryTrees(n: Integer): Int64;
begin
  if n < 3 then
    CountUnrootedBinaryTrees := 0
  else
    CountUnrootedBinaryTrees := DoubleFactorial(2 * n - 5);
end;

var
  n: Integer;
  result: Int64;

begin
  // Read input
  Write('Enter number of taxa (n): ');
  Readln(n);
  
  // Calculate result
  result := CountUnrootedBinaryTrees(n);
  
  // Output result
  Writeln('Number of distinct unrooted binary trees: ', result);
  
  // Example for n=4
  Writeln('Example for n=4:');
  Writeln('Double factorial (2*4-5)!! = 3!! = 3*1 = 3');
  
  // Example for n=5
  Writeln('Example for n=5:');
  Writeln('Double factorial (2*5-5)!! = 5!! = 5*3*1 = 15');
  
  Readln;
end.
```

## Explanation

### Mathematical Background
For n taxa, the number of distinct unrooted binary trees is given by the double factorial formula:
- (2n-5)!! = (2n-5) × (2n-7) × (2n-9) × ... × 3 × 1

This formula comes from the fact that:
1. An unrooted binary tree with n leaves has 2n-3 total vertices
2. The number of such trees follows a specific combinatorial pattern
3. For n ≥ 3, the formula is (2n-5)!!

### Key Functions

1. **DoubleFactorial**: Calculates the double factorial of a number
   - For odd numbers: n!! = n × (n-2) × (n-4) × ... × 3 × 1
   - For even numbers: n!! = n × (n-2) × (n-4) × ... × 4 × 2

2. **CountUnrootedBinaryTrees**: Applies the formula (2n-5)!! to count the trees

### Examples
- n=3: (2×3-5)!! = 1!! = 1 tree
- n=4: (2×4-5)!! = 3!! = 3 trees  
- n=5: (2×5-5)!! = 5!! = 15 trees
- n=6: (2×6-5)!! = 7!! = 105 trees

### Sample Input/Output
```
Enter number of taxa (n): 4
Number of distinct unrooted binary trees: 3
Example for n=4:
Double factorial (2*4-5)!! = 3!! = 3*1 = 3
Example for n=5:
Double factorial (2*5-5)!! = 5!! = 5*3*1 = 15
```

The solution handles the mathematical computation efficiently using recursion for the double factorial calculation and provides clear examples to demonstrate the concept.

