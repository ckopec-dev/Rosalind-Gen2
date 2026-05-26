# Rosalind Problem: Locating Restriction Sites

## Problem Description
In this problem, we need to find all locations where a given DNA sequence (restriction site) appears as a substring in a larger DNA sequence. This is a classic pattern matching problem.

## Solution in Pascal

```pascal
program LocatingRestrictionSites;

uses
  SysUtils, Classes;

function FindRestrictionSites(const DNA, Pattern: string): TStringList;
var
  i, patternLength, dnaLength: integer;
  found: boolean;
begin
  Result := TStringList.Create;
  patternLength := Length(Pattern);
  dnaLength := Length(DNA);
  
  // Check each possible position in DNA string
  for i := 1 to dnaLength - patternLength + 1 do
  begin
    // Extract substring of same length as pattern
    if Copy(DNA, i, patternLength) = Pattern then
    begin
      // Add position (1-indexed) to results
      Result.Add(IntToStr(i));
    end;
  end;
end;

procedure ReadInput(var DNA, Pattern: string);
var
  lines: TStringList;
  i: integer;
begin
  lines := TStringList.Create;
  try
    // Read all input lines
    while not EOF do
    begin
      SetLength(DNA, 0);
      ReadLn(DNA);
      lines.Add(DNA);
    end;
    
    // First line is DNA sequence
    DNA := lines[0];
    
    // Second line is restriction site pattern
    Pattern := lines[1];
    
  finally
    lines.Free;
  end;
end;

procedure Solve;
var
  DNA, Pattern: string;
  positions: TStringList;
  i: integer;
begin
  // Read input
  ReadInput(DNA, Pattern);
  
  // Find all restriction sites
  positions := FindRestrictionSites(DNA, Pattern);
  
  // Output results
  for i := 0 to positions.Count - 1 do
  begin
    WriteLn(positions[i]);
  end;
  
  positions.Free;
end;

begin
  Solve;
end.
```

## Alternative Implementation (More Efficient)

```pascal
program LocatingRestrictionSites;

uses
  SysUtils;

function FindRestrictionSites(const DNA, Pattern: string): string;
var
  i, j, patternLength, dnaLength: integer;
  found: boolean;
  result: string;
begin
  result := '';
  patternLength := Length(Pattern);
  dnaLength := Length(DNA);
  
  // Check each possible position in DNA string
  for i := 1 to dnaLength - patternLength + 1 do
  begin
    found := true;
    
    // Check if pattern matches at position i
    for j := 1 to patternLength do
    begin
      if DNA[i + j - 1] <> Pattern[j] then
      begin
        found := false;
        break;
      end;
    end;
    
    if found then
    begin
      if result <> '' then
        result := result + ' ' + IntToStr(i)
      else
        result := IntToStr(i);
    end;
  end;
  
  Result := result;
end;

var
  DNA, Pattern: string;
  positions: string;
begin
  // Read DNA sequence and pattern
  ReadLn(DNA);
  ReadLn(Pattern);
  
  // Find and output positions
  positions := FindRestrictionSites(DNA, Pattern);
  WriteLn(positions);
end.
```

## Sample Input/Output

**Input:**
```
ACGTACGTACGT
ACGT
```

**Output:**
```
1
5
9
```

## Explanation

1. **Input Reading**: The program reads two strings - the DNA sequence and the restriction site pattern
2. **Pattern Matching**: It searches for all occurrences of the pattern in the DNA sequence
3. **Position Tracking**: When a match is found, it records the 1-indexed position
4. **Output**: Returns all positions where the restriction site occurs

## Key Features

- **1-indexed positions**: Returns positions starting from 1 (as required by Rosalind)
- **Efficient searching**: Uses direct character comparison for pattern matching
- **Memory management**: Properly handles string operations and memory allocation
- **Robust input handling**: Works with standard input format

## Time Complexity
- **Time**: O(n × m) where n is the length of DNA sequence and m is the length of pattern
- **Space**: O(k) where k is the number of matches found

This solution efficiently finds all locations where a restriction site appears in a DNA sequence, which is a fundamental operation in bioinformatics.

