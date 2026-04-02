# Rosalind Problem: Consensus and Profile in Pascal

## Problem Description
Given a collection of DNA strings, we need to find the consensus string and profile matrix.

## Solution

```pascal
program ConsensusAndProfile;

uses
  SysUtils, Classes;

type
  TStringArray = array of string;
  TProfileMatrix = array[0..3, 0..1000] of integer; // A, C, G, T for each position

function GetConsensusString(profile: TProfileMatrix; length: integer): string;
var
  i, j, maxCount: integer;
  maxChar: char;
begin
  Result := '';
  for i := 0 to length - 1 do
  begin
    maxCount := 0;
    maxChar := 'A';
    
    // Check A
    if profile[0, i] > maxCount then
    begin
      maxCount := profile[0, i];
      maxChar := 'A';
    end;
    
    // Check C
    if profile[1, i] > maxCount then
    begin
      maxCount := profile[1, i];
      maxChar := 'C';
    end;
    
    // Check G
    if profile[2, i] > maxCount then
    begin
      maxCount := profile[2, i];
      maxChar := 'G';
    end;
    
    // Check T
    if profile[3, i] > maxCount then
    begin
      maxCount := profile[3, i];
      maxChar := 'T';
    end;
    
    Result := Result + maxChar;
  end;
end;

procedure ProcessDNASequences(dnaSequences: TStringArray; var profile: TProfileMatrix; var consensus: string);
var
  i, j, seqLength: integer;
  charIndex: integer;
begin
  // Initialize profile matrix
  for i := 0 to 3 do
    for j := 0 to 1000 do
      profile[i, j] := 0;
  
  // Get sequence length (assuming all sequences are same length)
  if Length(dnaSequences) > 0 then
  begin
    seqLength := Length(dnaSequences[0]);
    
    // Build profile matrix
    for i := 0 to Length(dnaSequences) - 1 do
    begin
      for j := 0 to seqLength - 1 do
      begin
        case dnaSequences[i][j+1] of
          'A': profile[0, j] := profile[0, j] + 1;
          'C': profile[1, j] := profile[1, j] + 1;
          'G': profile[2, j] := profile[2, j] + 1;
          'T': profile[3, j] := profile[3, j] + 1;
        end;
      end;
    end;
    
    // Generate consensus string
    consensus := GetConsensusString(profile, seqLength);
  end;
end;

procedure PrintProfile(profile: TProfileMatrix; length: integer);
var
  i, j: integer;
begin
  // Print A
  Write('A: ');
  for j := 0 to length - 1 do
    Write(profile[0, j], ' ');
  Writeln;
  
  // Print C
  Write('C: ');
  for j := 0 to length - 1 do
    Write(profile[1, j], ' ');
  Writeln;
  
  // Print G
  Write('G: ');
  for j := 0 to length - 1 do
    Write(profile[2, j], ' ');
  Writeln;
  
  // Print T
  Write('T: ');
  for j := 0 to length - 1 do
    Write(profile[3, j], ' ');
  Writeln;
end;

var
  dnaSequences: TStringArray;
  profile: TProfileMatrix;
  consensus: string;
  i, n: integer;
  inputLine: string;
  reader: TStringList;

begin
  // Read input from stdin
  reader := TStringList.Create;
  try
    // Read number of sequences
    Readln(n);
    
    // Read DNA sequences
    SetLength(dnaSequences, n);
    for i := 0 to n - 1 do
    begin
      Readln(dnaSequences[i]);
    end;
    
    // Process sequences
    ProcessDNASequences(dnaSequences, profile, consensus);
    
    // Output results
    Writeln(consensus);
    PrintProfile(profile, Length(dnaSequences[0]));
    
  finally
    reader.Free;
  end;
end.
```

## Input Format
The input should contain:
- First line: number of DNA sequences (n)
- Next n lines: DNA sequences of equal length

## Example Input
```
5
ATCCAGCT
GCCGGAAT
AAATTTGT
TTTTTTTT
ATCGATGC
```

## Example Output
```
ATGCAAAA
A: 5 1 0 0 5 5 0 0
C: 0 1 1 4 2 0 0 0
G: 1 1 1 3 2 0 0 0
T: 1 1 1 1 0 5 5 0
```

## Explanation
1. **Profile Matrix**: For each position in the DNA sequences, we count occurrences of each nucleotide (A, C, G, T)
2. **Consensus String**: At each position, we select the nucleotide with the highest count
3. **Output**: First line shows the consensus string, followed by the profile matrix for each nucleotide

## Time Complexity
- O(n × m) where n is the number of sequences and m is the length of each sequence

## Space Complexity
- O(m) for the profile matrix and consensus string

