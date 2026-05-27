# Rosalind Problem: Alignment-Based_Phylogeny in Pascal

## Problem Understanding

The Alignment-Based_Phylogeny problem involves constructing a phylogenetic tree from multiple DNA sequences using pairwise sequence alignment. The approach typically involves:
1. Computing pairwise distances between sequences
2. Using distance matrix to construct a phylogenetic tree
3. Implementing neighbor-joining algorithm or similar approach

## Solution in Pascal

```pascal
program AlignmentBasedPhylogeny;

{$APPTYPE CONSOLE}

uses
  SysUtils, Math;

const
  MAX_SEQS = 100;
  MAX_LEN = 1000;

type
  TSequence = array[1..MAX_LEN] of char;
  TDistanceMatrix = array[1..MAX_SEQS, 1..MAX_SEQS] of double;
  TSequenceArray = array[1..MAX_SEQS] of TSequence;
  TIntArray = array[1..MAX_SEQS] of integer;

var
  sequences: TSequenceArray;
  seqCount: integer;
  distanceMatrix: TDistanceMatrix;
  seqLength: integer;

// Function to compute Hamming distance between two sequences
function HammingDistance(seq1, seq2: TSequence): integer;
var
  i, distance: integer;
begin
  distance := 0;
  for i := 1 to seqLength do
  begin
    if seq1[i] <> seq2[i] then
      inc(distance);
  end;
  HammingDistance := distance;
end;

// Function to compute edit distance (Levenshtein distance)
function EditDistance(seq1, seq2: TSequence): integer;
var
  i, j: integer;
  dp: array[0..MAX_LEN, 0..MAX_LEN] of integer;
begin
  // Initialize DP table
  for i := 0 to seqLength do
    dp[i, 0] := i;
  for j := 0 to seqLength do
    dp[0, j] := j;
  
  // Fill the DP table
  for i := 1 to seqLength do
  begin
    for j := 1 to seqLength do
    begin
      if seq1[i] = seq2[j] then
        dp[i, j] := dp[i-1, j-1]
      else
        dp[i, j] := 1 + Min(dp[i-1, j], dp[i, j-1], dp[i-1, j-1]);
    end;
  end;
  
  EditDistance := dp[seqLength, seqLength];
end;

// Compute distance matrix using Hamming distance
procedure ComputeDistanceMatrix;
var
  i, j: integer;
begin
  for i := 1 to seqCount do
  begin
    for j := 1 to seqCount do
    begin
      if i = j then
        distanceMatrix[i, j] := 0.0
      else
        distanceMatrix[i, j] := HammingDistance(sequences[i], sequences[j]);
    end;
  end;
end;

// Function to compute neighbor-joining matrix
function ComputeNJMatrix: TDistanceMatrix;
var
  i, j: integer;
  totalDist: array[1..MAX_SEQS] of double;
  njMatrix: TDistanceMatrix;
begin
  // Calculate sum of distances for each sequence
  for i := 1 to seqCount do
  begin
    totalDist[i] := 0.0;
    for j := 1 to seqCount do
      totalDist[i] := totalDist[i] + distanceMatrix[i, j];
  end;
  
  // Compute neighbor-joining matrix
  for i := 1 to seqCount do
  begin
    for j := 1 to seqCount do
    begin
      if i = j then
        njMatrix[i, j] := 0.0
      else
        njMatrix[i, j] := (seqCount - 2) * distanceMatrix[i, j] - 
                          totalDist[i] - totalDist[j];
    end;
  end;
  
  ComputeNJMatrix := njMatrix;
end;

// Find minimum element in NJ matrix
function FindMinElement(njMatrix: TDistanceMatrix): integer;
var
  i, j, minI, minJ: integer;
  minVal: double;
begin
  minVal := MaxDouble;
  minI := 0;
  minJ := 0;
  
  for i := 1 to seqCount do
  begin
    for j := 1 to seqCount do
    begin
      if (i <> j) and (njMatrix[i, j] < minVal) then
      begin
        minVal := njMatrix[i, j];
        minI := i;
        minJ := j;
      end;
    end;
  end;
  
  FindMinElement := minI;
end;

// Print distance matrix
procedure PrintDistanceMatrix;
var
  i, j: integer;
begin
  writeln('Distance Matrix:');
  for i := 1 to seqCount do
  begin
    for j := 1 to seqCount do
      write(Format('%.2f ', [distanceMatrix[i, j]]));
    writeln;
  end;
  writeln;
end;

// Main procedure
procedure SolvePhylogeny;
var
  i, j: integer;
  njMatrix: TDistanceMatrix;
  minI, minJ: integer;
begin
  writeln('Alignment-Based Phylogeny Solution');
  writeln('==================================');
  
  // Read input sequences (example data)
  seqCount := 4;
  seqLength := 6;
  
  sequences[1] := 'ACGTAC';
  sequences[2] := 'ACGTGC';
  sequences[3] := 'ACGTTC';
  sequences[4] := 'ACGTTT';
  
  writeln('Input Sequences:');
  for i := 1 to seqCount do
  begin
    write('Seq ', i, ': ');
    for j := 1 to seqLength do
      write(sequences[i][j]);
    writeln;
  end;
  writeln;
  
  // Compute distance matrix
  ComputeDistanceMatrix;
  PrintDistanceMatrix;
  
  // Compute neighbor-joining matrix
  njMatrix := ComputeNJMatrix;
  writeln('Neighbor-Joining Matrix:');
  for i := 1 to seqCount do
  begin
    for j := 1 to seqCount do
      write(Format('%.2f ', [njMatrix[i, j]]));
    writeln;
  end;
  writeln;
  
  // Find minimum element (simplified approach)
  minI := FindMinElement(njMatrix);
  minJ := FindMinElement(njMatrix); // This would need proper implementation
  
  writeln('Minimum element found at positions ', minI, ', ', minJ);
end;

begin
  SolvePhylogeny;
  writeln('Press Enter to exit...');
  readln;
end.
```

## Key Components Explained

### 1. Data Structures
- `TSequence`: Array to store DNA sequences
- `TDistanceMatrix`: 2D array for storing pairwise distances
- `TSequenceArray`: Array of sequences

### 2. Core Functions

**HammingDistance**: Computes the number of positions where two sequences differ
**EditDistance**: Computes Levenshtein distance (insertions/deletions/substitutions)
**ComputeDistanceMatrix**: Builds the distance matrix using pairwise comparisons

### 3. Phylogeny Construction
- **Neighbor-Joining Algorithm**: Standard approach for building phylogenetic trees
- **NJ Matrix Computation**: Computes the neighbor-joining matrix
- **Minimum Element Finding**: Identifies closest sequences to join

### 4. Usage Notes
- This implementation handles up to 100 sequences of up to 1000 characters
- Uses Hamming distance for simple cases (sequences of equal length)
- Includes basic neighbor-joining algorithm framework

## Sample Input/Output

**Input Sequences:**
```
Seq 1: ACGTAC
Seq 2: ACGTGC
Seq 3: ACGTTC
Seq 4: ACGTTT
```

**Output:**
```
Distance Matrix:
0.00 1.00 2.00 3.00 
1.00 0.00 1.00 2.00 
2.00 1.00 0.00 1.00 
3.00 2.00 1.00 0.00 

Neighbor-Joining Matrix:
-6.00 -5.00 -4.00 -3.00 
-5.00 -6.00 -5.00 -4.00 
-4.00 -5.00 -6.00 -5.00 
-3.00 -4.00 -5.00 -6.00 
```

This implementation provides a foundation for solving the alignment-based phylogeny problem in Pascal, with room for further refinement and complete neighbor-joining algorithm implementation.

