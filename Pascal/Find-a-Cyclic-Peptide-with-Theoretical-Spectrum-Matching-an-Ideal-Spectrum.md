# Rosalind Problem: Find a Cyclic Peptide with Theoretical Spectrum Matching an Ideal Spectrum

## Problem Understanding

This problem asks us to find a cyclic peptide whose theoretical spectrum matches a given ideal spectrum. We need to:
1. Generate all possible cyclic peptides of a given length
2. Calculate their theoretical spectra
3. Find the peptide whose spectrum matches the ideal spectrum exactly

## Solution Approach

I'll implement a solution using:
- A function to generate theoretical spectra for cyclic peptides
- A function to compare spectra
- A backtracking approach to generate peptides

## Pascal Implementation

```pascal
program FindCyclicPeptide;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Math;

type
  TIntArray = array of Integer;
  TIntegerList = array of Integer;

function GetAminoAcidMasses: TIntegerList;
begin
  SetLength(Result, 20);
  Result[0] := 57;   // A
  Result[1] := 71;   // C
  Result[2] := 87;   // D
  Result[3] := 97;   // E
  Result[4] := 99;   // F
  Result[5] := 101;  // G
  Result[6] := 103;  // H
  Result[7] := 113;  // I
  Result[8] := 114;  // K
  Result[9] := 115;  // L
  Result[10] := 128; // M
  Result[11] := 129; // N
  Result[12] := 131; // P
  Result[13] := 137; // Q
  Result[14] := 147; // R
  Result[15] := 156; // S
  Result[16] := 163; // T
  Result[17] := 186; // V
  Result[18] := 197; // W
  Result[19] := 206; // Y
end;

function GetMassesFromPeptide(peptide: string): TIntegerList;
var
  i: Integer;
  aminoAcids: TIntegerList;
begin
  aminoAcids := GetAminoAcidMasses;
  SetLength(Result, Length(peptide));
  for i := 1 to Length(peptide) do
  begin
    case peptide[i] of
      'A': Result[i-1] := aminoAcids[0];
      'C': Result[i-1] := aminoAcids[1];
      'D': Result[i-1] := aminoAcids[2];
      'E': Result[i-1] := aminoAcids[3];
      'F': Result[i-1] := aminoAcids[4];
      'G': Result[i-1] := aminoAcids[5];
      'H': Result[i-1] := aminoAcids[6];
      'I': Result[i-1] := aminoAcids[7];
      'K': Result[i-1] := aminoAcids[8];
      'L': Result[i-1] := aminoAcids[9];
      'M': Result[i-1] := aminoAcids[10];
      'N': Result[i-1] := aminoAcids[11];
      'P': Result[i-1] := aminoAcids[12];
      'Q': Result[i-1] := aminoAcids[13];
      'R': Result[i-1] := aminoAcids[14];
      'S': Result[i-1] := aminoAcids[15];
      'T': Result[i-1] := aminoAcids[16];
      'V': Result[i-1] := aminoAcids[17];
      'W': Result[i-1] := aminoAcids[18];
      'Y': Result[i-1] := aminoAcids[19];
    end;
  end;
end;

function GetTheoreticalSpectrum(peptide: string): TIntegerList;
var
  i, j, k, n, totalMass: Integer;
  masses: TIntegerList;
  spectrum: TIntegerList;
  temp: Integer;
begin
  masses := GetMassesFromPeptide(peptide);
  n := Length(masses);
  
  // Initialize spectrum with 0 (empty peptide)
  SetLength(spectrum, (n * (n - 1)) div 2 + 1);
  spectrum[0] := 0;
  k := 1;
  
  // Generate all subpeptides (including the full peptide)
  for i := 0 to n - 1 do
  begin
    totalMass := 0;
    for j := 0 to n - 1 do
    begin
      totalMass := totalMass + masses[(i + j) mod n];
      if j > 0 then
      begin
        spectrum[k] := totalMass;
        Inc(k);
      end;
    end;
  end;
  
  SetLength(spectrum, k);
  // Sort spectrum
  for i := 0 to k - 2 do
    for j := i + 1 to k - 1 do
      if spectrum[i] > spectrum[j] then
      begin
        temp := spectrum[i];
        spectrum[i] := spectrum[j];
        spectrum[j] := temp;
      end;
      
  Result := spectrum;
end;

function CompareSpectra(spectrum1, spectrum2: TIntegerList): Boolean;
var
  i: Integer;
begin
  if Length(spectrum1) <> Length(spectrum2) then
  begin
    Result := False;
    Exit;
  end;
  
  for i := 0 to Length(spectrum1) - 1 do
  begin
    if spectrum1[i] <> spectrum2[i] then
    begin
      Result := False;
      Exit;
    end;
  end;
  
  Result := True;
end;

function GeneratePeptides(length: Integer): array of string;
var
  i, j, k, n: Integer;
  aminoAcids: string;
  peptides: array of string;
  current: string;
begin
  aminoAcids := 'ACDEFGHIKLMNPQRSTVWY';
  n := Length(aminoAcids);
  SetLength(peptides, Power(n, length));
  
  for i := 0 to Power(n, length) - 1 do
  begin
    current := '';
    j := i;
    for k := 1 to length do
    begin
      current := current + aminoAcids[(j mod n) + 1];
      j := j div n;
    end;
    peptides[i] := current;
  end;
  
  Result := peptides;
end;

function FindCyclicPeptideWithMatchingSpectrum(idealSpectrum: TIntegerList; peptideLength: Integer): string;
var
  i: Integer;
  peptides: array of string;
  theoreticalSpectrum: TIntegerList;
  peptide: string;
begin
  peptides := GeneratePeptides(peptideLength);
  
  for i := 0 to Length(peptides) - 1 do
  begin
    peptide := peptides[i];
    theoreticalSpectrum := GetTheoreticalSpectrum(peptide);
    
    if CompareSpectra(theoreticalSpectrum, idealSpectrum) then
    begin
      Result := peptide;
      Exit;
    end;
  end;
  
  Result := '';
end;

procedure PrintSpectrum(spectrum: TIntegerList);
var
  i: Integer;
begin
  Write('[');
  for i := 0 to Length(spectrum) - 1 do
  begin
    Write(spectrum[i]);
    if i < Length(spectrum) - 1 then
      Write(' ');
  end;
  Write(']');
  Writeln;
end;

var
  idealSpectrum: TIntegerList;
  peptideLength: Integer;
  resultPeptide: string;
  i: Integer;

begin
  // Example input - replace with actual input
  // This is an example of an ideal spectrum
  SetLength(idealSpectrum, 7);
  idealSpectrum[0] := 0;
  idealSpectrum[1] := 113;
  idealSpectrum[2] := 129;
  idealSpectrum[3] := 147;
  idealSpectrum[4] := 163;
  idealSpectrum[5] := 186;
  idealSpectrum[6] := 206;
  
  peptideLength := 4;
  
  Writeln('Ideal Spectrum:');
  PrintSpectrum(idealSpectrum);
  
  resultPeptide := FindCyclicPeptideWithMatchingSpectrum(idealSpectrum, peptideLength);
  
  if resultPeptide <> '' then
  begin
    Writeln('Found peptide: ', resultPeptide);
    Writeln('Theoretical spectrum:');
    PrintSpectrum(GetTheoreticalSpectrum(resultPeptide));
  end
  else
  begin
    Writeln('No matching peptide found');
  end;
  
  Readln;
end.
```

## Key Components

1. **GetAminoAcidMasses**: Returns the mass values for all 20 amino acids
2. **GetMassesFromPeptide**: Converts a peptide string to its mass array
3. **GetTheoreticalSpectrum**: Computes the theoretical spectrum for a cyclic peptide
4. **CompareSpectra**: Checks if two spectra match exactly
5. **FindCyclicPeptideWithMatchingSpectrum**: Main function that searches for matching peptides

## Algorithm Steps

1. Generate all possible peptides of the given length
2. For each peptide, calculate its theoretical spectrum
3. Compare each theoretical spectrum with the ideal spectrum
4. Return the first peptide whose spectrum matches exactly

## Time Complexity

- Generating all peptides: O(A^n) where A is the number of amino acids and n is peptide length
- Computing each spectrum: O(n²)
- Overall: O(A^n × n²)

## Usage

To use this solution with actual Rosalind data, you would need to:
1. Parse the input spectrum from standard input
2. Set the correct peptide length
3. Call the main function with appropriate parameters

The solution handles the cyclic nature of peptides by generating all subpeptides and their masses, then sorting the results to create the theoretical spectrum.

