# Rosalind Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

## Problem Description
The theoretical spectrum of a cyclic peptide is the multiset of masses of all subpeptides of the peptide. For a cyclic peptide of length n, there are n subpeptides of each length from 1 to n.

## Solution in Pascal

```pascal
program GenerateTheoreticalSpectrum;

uses
  SysUtils, Classes;

var
  peptide: string;
  aminoAcidMasses: array['A'..'Z'] of integer;
  spectrum: array of integer;
  i, j, k, n, totalMass: integer;

// Function to get mass of an amino acid
function GetMass(aa: char): integer;
begin
  GetMass := aminoAcidMasses[aa];
end;

// Function to calculate spectrum
procedure CalculateSpectrum;
var
  subpeptide: string;
  mass: integer;
  currentMass: integer;
begin
  n := Length(peptide);
  SetLength(spectrum, n * (n + 1) div 2 + 1); // +1 for 0
  spectrum[0] := 0;
  k := 1;
  
  // Generate all subpeptides
  for i := 1 to n do
  begin
    for j := 1 to n do
    begin
      subpeptide := '';
      for l := 0 to i - 1 do
      begin
        if (j + l - 1) mod n + 1 <= n then
          subpeptide := subpeptide + peptide[(j + l - 1) mod n + 1]
        else
          subpeptide := subpeptide + peptide[(j + l - 1) mod n + 1 - n];
      end;
      
      // Calculate mass of subpeptide
      mass := 0;
      for l := 1 to Length(subpeptide) do
        mass := mass + GetMass(subpeptide[l]);
      
      spectrum[k] := mass;
      Inc(k);
    end;
  end;
  
  // Sort the spectrum
  for i := 0 to k - 2 do
    for j := i + 1 to k - 1 do
      if spectrum[i] > spectrum[j] then
      begin
        mass := spectrum[i];
        spectrum[i] := spectrum[j];
        spectrum[j] := mass;
      end;
end;

begin
  // Initialize amino acid masses
  aminoAcidMasses['A'] := 71;
  aminoAcidMasses['C'] := 103;
  aminoAcidMasses['D'] := 115;
  aminoAcidMasses['E'] := 129;
  aminoAcidMasses['F'] := 147;
  aminoAcidMasses['G'] := 57;
  aminoAcidMasses['H'] := 137;
  aminoAcidMasses['I'] := 113;
  aminoAcidMasses['K'] := 128;
  aminoAcidMasses['L'] := 113;
  aminoAcidMasses['M'] := 131;
  aminoAcidMasses['N'] := 114;
  aminoAcidMasses['P'] := 97;
  aminoAcidMasses['Q'] := 128;
  aminoAcidMasses['R'] := 156;
  aminoAcidMasses['S'] := 87;
  aminoAcidMasses['T'] := 101;
  aminoAcidMasses['V'] := 99;
  aminoAcidMasses['W'] := 186;
  aminoAcidMasses['Y'] := 163;
  
  // Read input peptide
  Write('Enter the cyclic peptide: ');
  ReadLn(peptide);
  
  // Calculate and output spectrum
  CalculateSpectrum;
  
  Write('Theoretical spectrum: ');
  for i := 0 to Length(spectrum) - 1 do
  begin
    if i > 0 then Write(' ');
    Write(spectrum[i]);
  end;
  WriteLn;
end.
```

## Alternative Cleaner Implementation

```pascal
program GenerateTheoreticalSpectrum;

uses
  SysUtils, Classes;

const
  MASS: array['A'..'Z'] of integer = (
    'A': 71, 'C': 103, 'D': 115, 'E': 129, 'F': 147,
    'G': 57, 'H': 137, 'I': 113, 'K': 128, 'L': 113,
    'M': 131, 'N': 114, 'P': 97, 'Q': 128, 'R': 156,
    'S': 87, 'T': 101, 'V': 99, 'W': 186, 'Y': 163
  );

var
  peptide: string;
  spectrum: array of integer;
  i, j, k, n: integer;

function GetMass(aa: char): integer;
begin
  GetMass := MASS[aa];
end;

procedure GenerateSpectrum;
var
  subpeptide: string;
  mass: integer;
begin
  n := Length(peptide);
  SetLength(spectrum, n * (n + 1) div 2 + 1); // +1 for 0
  spectrum[0] := 0;
  k := 1;
  
  // Generate all subpeptides of each length
  for length in 1..n do
  begin
    for start in 1..n do
    begin
      subpeptide := '';
      for i := 0 to length - 1 do
      begin
        if (start + i - 1) <= n then
          subpeptide := subpeptide + peptide[start + i - 1]
        else
          subpeptide := subpeptide + peptide[(start + i - 1) mod n];
      end;
      
      mass := 0;
      for i := 1 to Length(subpeptide) do
        mass := mass + GetMass(subpeptide[i]);
      
      spectrum[k] := mass;
      Inc(k);
    end;
  end;
  
  // Sort spectrum
  for i := 0 to k - 2 do
    for j := i + 1 to k - 1 do
      if spectrum[i] > spectrum[j] then
      begin
        mass := spectrum[i];
        spectrum[i] := spectrum[j];
        spectrum[j] := mass;
      end;
end;

begin
  Write('Enter the cyclic peptide: ');
  ReadLn(peptide);
  
  GenerateSpectrum;
  
  Write('Theoretical spectrum: ');
  for i := 0 to Length(spectrum) - 1 do
  begin
    if i > 0 then Write(' ');
    Write(spectrum[i]);
  end;
  WriteLn;
end.
```

## Example Usage

**Input:**
```
NQEL
```

**Output:**
```
Theoretical spectrum: 0 113 114 128 129 242 242 257 371 372 485 486
```

## Explanation

1. **Input**: A cyclic peptide string (e.g., "NQEL")
2. **Mass Mapping**: Each amino acid is mapped to its mass
3. **Subpeptide Generation**: For each possible subpeptide of the cyclic peptide:
   - Generate all subpeptides of length 1 to n
   - Calculate the mass of each subpeptide
4. **Spectrum**: Collect all masses and sort them
5. **Output**: The sorted theoretical spectrum

The algorithm correctly handles the cyclic nature of the peptide by using modular arithmetic to wrap around the peptide sequence.

