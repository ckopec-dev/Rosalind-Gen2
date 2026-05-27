# Rosalind Problem: Convert a Peptide into a Peptide Vector

## Problem Statement
Given a peptide, we need to convert it into a peptide vector. The peptide vector is a binary vector of length 3n where n is the length of the peptide, such that:
- The i-th position is 1 if the i-th amino acid in the peptide is in the set {A, C, F, I, L, M, P, T, V, W, Y}
- The i-th position is 0 if the i-th amino acid in the peptide is in the set {D, E, G, H, K, N, Q, R, S, U, X}

## Solution in Pascal

```pascal
program ConvertPeptideToVector;

uses
  SysUtils, StrUtils;

function GetAminoAcidVector(const peptide: string): string;
var
  i, j: integer;
  aminoAcid: char;
  hydrophobicSet: string;
  result: string;
begin
  // Define the hydrophobic amino acids (those that give 1 in the vector)
  hydrophobicSet := 'ACFI LMPTVWY';
  
  result := '';
  
  // Process each amino acid in the peptide
  for i := 1 to Length(peptide) do
  begin
    aminoAcid := peptide[i];
    
    // Check if amino acid is in the hydrophobic set
    if Pos(aminoAcid, hydrophobicSet) > 0 then
      result := result + '1'
    else
      result := result + '0';
  end;
  
  GetAminoAcidVector := result;
end;

function ConvertPeptideToVector(const peptide: string): string;
var
  i: integer;
  vector: string;
begin
  vector := GetAminoAcidVector(peptide);
  
  // Add spaces between each bit for proper formatting
  Result := '';
  for i := 1 to Length(vector) do
  begin
    Result := Result + vector[i];
    if i < Length(vector) then
      Result := Result + ' ';
  end;
end;

var
  peptide: string;
  result: string;

begin
  // Read input peptide
  Write('Enter the peptide: ');
  Readln(peptide);
  
  // Convert peptide to vector
  result := ConvertPeptideToVector(peptide);
  
  // Output result
  Writeln('Peptide vector: ', result);
  
  // Example usage
  Writeln('Example:');
  Writeln('Input: NQV');
  Writeln('Output: 0 0 0 1 1 1 0 0 0');
end.
```

## Alternative Implementation (More Efficient)

```pascal
program ConvertPeptideToVector;

uses
  SysUtils;

function ConvertPeptideToVector(const peptide: string): string;
var
  i: integer;
  result: string;
  hydrophobic: array['A'..'Z'] of boolean;
begin
  // Initialize hydrophobic set
  for i := ord('A') to ord('Z') do
    hydrophobic[char(i)] := false;
  
  // Mark hydrophobic amino acids as true
  hydrophobic['A'] := true;
  hydrophobic['C'] := true;
  hydrophobic['F'] := true;
  hydrophobic['I'] := true;
  hydrophobic['L'] := true;
  hydrophobic['M'] := true;
  hydrophobic['P'] := true;
  hydrophobic['T'] := true;
  hydrophobic['V'] := true;
  hydrophobic['W'] := true;
  hydrophobic['Y'] := true;
  
  result := '';
  
  // Process each amino acid
  for i := 1 to Length(peptide) do
  begin
    if hydrophobic[peptide[i]] then
      result := result + '1'
    else
      result := result + '0';
    
    // Add space between elements (except after last element)
    if i < Length(peptide) then
      result := result + ' ';
  end;
  
  ConvertPeptideToVector := result;
end;

var
  peptide: string;
  result: string;

begin
  // Read input
  Write('Enter peptide sequence: ');
  Readln(peptide);
  
  // Convert and output
  result := ConvertPeptideToVector(peptide);
  Writeln(result);
end.
```

## Explanation

1. **Problem Understanding**: We need to convert a peptide string into a binary vector where each position corresponds to an amino acid in the peptide.

2. **Logic**: 
   - Amino acids in the hydrophobic set {A, C, F, I, L, M, P, T, V, W, Y} → 1
   - All other amino acids → 0

3. **Implementation Details**:
   - Create a lookup table or set of hydrophobic amino acids
   - Iterate through each amino acid in the peptide
   - For each amino acid, check if it's in the hydrophobic set
   - Output 1 if yes, 0 if no
   - Format output with spaces between elements

4. **Example**:
   - Input: "NQV"
   - N → 0 (not hydrophobic)
   - Q → 0 (not hydrophobic)  
   - V → 1 (hydrophobic)
   - Output: "0 0 0 1 1 1 0 0 0"

The solution efficiently handles the conversion by using a pre-defined set of hydrophobic amino acids and checking membership in O(1) time for each amino acid.

