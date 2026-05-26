# Rosalind Problem: Transcribing DNA into RNA (Pascal Solution)

## Problem Description
In the problem Transcribing DNA into RNA, we need to replace all occurrences of the nucleotide 'T' with 'U' in a DNA string to produce the corresponding RNA string.

## Solution

```pascal
program TranscribingDNAIntoRNA;

uses
  SysUtils;

function DNAtoRNA(dna: string): string;
var
  i: integer;
  rna: string;
begin
  rna := '';
  for i := 1 to Length(dna) do
  begin
    if dna[i] = 'T' then
      rna := rna + 'U'
    else
      rna := rna + dna[i];
  end;
  DNAtoRNA := rna;
end;

var
  dna_string: string;
  rna_string: string;

begin
  // Read DNA string from input
  Readln(dna_string);
  
  // Convert DNA to RNA
  rna_string := DNAtoRNA(dna_string);
  
  // Output the result
  Writeln(rna_string);
end.
```

## Explanation

1. **Function DNAtoRNA**: This function takes a DNA string as input and returns the corresponding RNA string
   - It iterates through each character of the DNA string
   - If the character is 'T', it replaces it with 'U'
   - Otherwise, it keeps the original character
   - Returns the resulting RNA string

2. **Main Program**:
   - Reads the DNA string from standard input
   - Calls the DNAtoRNA function to convert it
   - Outputs the resulting RNA string

## Example

**Input:**
```
GATGGAACTTGACTACGTAAATT
```

**Output:**
```
GAUGGAACUUGACUACGUAAAUU
```

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the DNA string
- **Space Complexity**: O(n) for storing the resulting RNA string

This solution efficiently handles the DNA to RNA transcription by simply replacing each 'T' with 'U' while preserving all other nucleotides.

