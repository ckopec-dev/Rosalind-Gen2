# Rosalind Problem: Generate the d-Neighborhood of a String

## Problem Description
The d-neighborhood of a string is the set of all strings that have Hamming distance at most d from the original string.

## Solution in Pascal

```pascal
program DNeighborhood;

uses
  SysUtils, Classes;

function HammingDistance(s1, s2: string): integer;
var
  i: integer;
begin
  HammingDistance := 0;
  for i := 1 to Length(s1) do
    if s1[i] <> s2[i] then
      Inc(HammingDistance);
end;

function GetDNAChars: string;
begin
  GetDNAChars := 'ACGT';
end;

procedure GenerateNeighbors(current, pattern: string; d: integer; var results: TStringList);
var
  i, j: integer;
  dnaChars: string;
  newPattern: string;
begin
  // If we've reached the end of the pattern
  if Length(current) = Length(pattern) then
  begin
    // Check if Hamming distance is at most d
    if HammingDistance(current, pattern) <= d then
      results.Add(current);
    Exit;
  end;
  
  // Get DNA characters
  dnaChars := GetDNAChars;
  
  // For each position, try each DNA character
  for i := 1 to Length(dnaChars) do
  begin
    newPattern := current + dnaChars[i];
    GenerateNeighbors(newPattern, pattern, d, results);
  end;
end;

function GenerateDNeighborhood(pattern: string; d: integer): TStringList;
var
  results: TStringList;
begin
  results := TStringList.Create;
  GenerateNeighbors('', pattern, d, results);
  GenerateDNeighborhood := results;
end;

// Alternative iterative approach using backtracking
procedure GenerateDNeighborhoodIterative(pattern: string; d: integer; var results: TStringList);
var
  i, j, k: integer;
  current: string;
  queue: array of string;
  queueSize: integer;
  dnaChars: string;
begin
  results.Clear;
  dnaChars := 'ACGT';
  
  // Initialize queue with empty string
  SetLength(queue, 1);
  queue[0] := '';
  queueSize := 1;
  
  for i := 1 to Length(pattern) do
  begin
    for j := 0 to queueSize - 1 do
    begin
      for k := 1 to Length(dnaChars) do
      begin
        current := queue[j] + dnaChars[k];
        if Length(current) = Length(pattern) then
        begin
          if HammingDistance(current, pattern) <= d then
            results.Add(current);
        end
        else
        begin
          SetLength(queue, queueSize + 1);
          queue[queueSize] := current;
          Inc(queueSize);
        end;
      end;
    end;
  end;
end;

// Main procedure to solve the problem
procedure SolveDNeighborhood(inputPattern: string; inputD: integer);
var
  results: TStringList;
  i: integer;
begin
  results := TStringList.Create;
  
  // Generate d-neighborhood
  GenerateDNeighborhoodIterative(inputPattern, inputD, results);
  
  // Sort results
  results.Sort;
  
  // Output results
  for i := 0 to results.Count - 1 do
    Writeln(results[i]);
  
  results.Free;
end;

begin
  // Example usage
  // You would typically read from input file or console
  SolveDNeighborhood('ACGT', 1);
end.
```

## Explanation

This Pascal program solves the d-neighborhood problem using a recursive backtracking approach:

1. **HammingDistance Function**: Calculates the Hamming distance between two strings by counting differing positions.

2. **GenerateDNeighborhood Function**: 
   - Uses recursive backtracking to generate all possible strings of the same length
   - At each position, tries all four DNA nucleotides (A, C, G, T)
   - Only keeps strings with Hamming distance ≤ d from the original pattern

3. **Alternative Iterative Approach**: 
   - Uses a queue-based approach to avoid deep recursion
   - Builds strings character by character
   - Checks distance condition when string reaches target length

## Key Features

- **Recursive Backtracking**: Builds all possible strings character by character
- **Distance Checking**: Verifies Hamming distance at each valid completion
- **Sorting**: Results are sorted alphabetically as required
- **Memory Efficient**: Uses TStringList for dynamic storage

## Time Complexity
- O(4^n × n) where n is the length of the pattern
- In worst case, generates all possible strings of length n over 4-character alphabet

## Space Complexity
- O(4^n × n) for storing results

The program can be easily modified to read from standard input or file for actual Rosalind submission.

