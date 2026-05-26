# Rosalind Problem: Compute the Probability of a Spectral Dictionary

## Problem Understanding

We need to compute the probability of a spectral dictionary, which involves calculating the probability of observing a specific spectral dictionary given a set of amino acid masses and their probabilities.

## Solution Approach

Let me break down the problem:
1. We have a set of amino acid masses and their probabilities
2. We need to compute the probability of all possible spectral dictionaries
3. This involves dynamic programming or recursive computation

## Pascal Implementation

```pascal
program ComputeProbabilityOfSpectralDictionary;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

const
  MAX_MASS = 1000;
  EPS = 1e-9;

type
  TDoubleArray = array[0..MAX_MASS] of Double;
  TDoubleArray2D = array[0..MAX_MASS, 0..MAX_MASS] of Double;

var
  masses: array[0..25] of Integer;
  probabilities: array[0..25] of Double;
  n: Integer;
  dp: TDoubleArray;
  totalProb: Double;

// Function to compute the probability of a spectral dictionary
function ComputeSpectralDictionaryProbability(targetMass: Integer): Double;
var
  i, j: Integer;
begin
  // Initialize dp array
  for i := 0 to targetMass do
    dp[i] := 0.0;
  
  // Base case
  dp[0] := 1.0;
  
  // Fill dp array using dynamic programming
  for i := 1 to targetMass do
  begin
    for j := 0 to n - 1 do
    begin
      if (i >= masses[j]) and (dp[i - masses[j]] > EPS) then
      begin
        dp[i] := dp[i] + dp[i - masses[j]] * probabilities[j];
      end;
    end;
  end;
  
  Result := dp[targetMass];
end;

// Function to compute the probability of a specific amino acid
function ComputeAminoAcidProbability(aminoAcidMass: Integer): Double;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to n - 1 do
  begin
    if masses[i] = aminoAcidMass then
    begin
      Result := probabilities[i];
      Break;
    end;
  end;
end;

// Main function to solve the problem
procedure SolveProblem;
var
  targetMass: Integer;
  i: Integer;
  result: Double;
begin
  // Read input data
  WriteLn('Enter number of amino acids:');
  ReadLn(n);
  
  WriteLn('Enter masses and probabilities (mass probability):');
  for i := 0 to n - 1 do
  begin
    ReadLn(masses[i], probabilities[i]);
  end;
  
  WriteLn('Enter target mass:');
  ReadLn(targetMass);
  
  // Compute the probability
  result := ComputeSpectralDictionaryProbability(targetMass);
  
  WriteLn('Probability of spectral dictionary with mass ', targetMass, ': ', 
           FormatFloat('0.000000', result));
end;

// Alternative approach using recursive with memoization
var
  memo: TDoubleArray;

function RecursiveCompute(mass: Integer): Double;
var
  i: Integer;
begin
  if mass < 0 then
  begin
    Result := 0.0;
    Exit;
  end;
  
  if mass = 0 then
  begin
    Result := 1.0;
    Exit;
  end;
  
  if memo[mass] > EPS then
  begin
    Result := memo[mass];
    Exit;
  end;
  
  Result := 0.0;
  for i := 0 to n - 1 do
  begin
    if mass >= masses[i] then
    begin
      Result := Result + RecursiveCompute(mass - masses[i]) * probabilities[i];
    end;
  end;
  
  memo[mass] := Result;
end;

// Complete solution function
function SolveSpectralDictionary(targetMass: Integer): Double;
var
  i: Integer;
begin
  // Initialize memo array
  for i := 0 to targetMass do
    memo[i] := 0.0;
  
  Result := RecursiveCompute(targetMass);
end;

// Example usage with sample data
procedure ExampleUsage;
var
  i: Integer;
  result: Double;
begin
  // Sample data: amino acid masses and probabilities
  n := 5;
  masses[0] := 57; probabilities[0] := 0.075;
  masses[1] := 71; probabilities[1] := 0.100;
  masses[2] := 87; probabilities[2] := 0.100;
  masses[3] := 97; probabilities[3] := 0.100;
  masses[4] := 101; probabilities[4] := 0.100;
  
  WriteLn('Sample amino acid data:');
  for i := 0 to n - 1 do
  begin
    WriteLn('Mass: ', masses[i], ', Probability: ', FormatFloat('0.000', probabilities[i]));
  end;
  
  // Compute probability for mass 100
  result := SolveSpectralDictionary(100);
  WriteLn('Probability for mass 100: ', FormatFloat('0.000000', result));
end;

begin
  WriteLn('Rosalind: Compute the Probability of a Spectral Dictionary');
  WriteLn('========================================================');
  
  // Run example
  ExampleUsage;
  
  WriteLn('');
  WriteLn('Running interactive mode:');
  SolveProblem;
end.
```

## Key Components Explained

1. **Data Structures**: 
   - Arrays to store amino acid masses and their probabilities
   - Dynamic programming array for efficient computation

2. **Algorithm**:
   - Uses dynamic programming approach to compute probabilities
   - For each mass, we consider all possible amino acids that could contribute to it
   - The probability is accumulated through the recurrence relation

3. **Dynamic Programming Approach**:
   - `dp[i]` represents the probability of achieving mass `i`
   - For each mass `i`, we iterate through all amino acids and accumulate probabilities
   - `dp[i] = Σ(dp[i - mass_j] * probability_j)` for all valid j

## Time and Space Complexity

- **Time Complexity**: O(targetMass × n) where n is the number of amino acids
- **Space Complexity**: O(targetMass) for the DP array

## Sample Input/Output

For the sample input with amino acids having masses [57, 71, 87, 97, 101] and equal probabilities of 0.100, the program will compute the probability of forming any specific mass through combinations of these amino acids.

This implementation handles the core mathematical problem of computing spectral dictionary probabilities using dynamic programming principles.

