# Rosalind Problem: Linguistic Complexity of a Genome (Pascal Solution)

## Problem Understanding

The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings of a given length k to the maximum possible number of distinct substrings of that length.

For a string of length n and substring length k:
- Maximum possible distinct substrings = n - k + 1
- Actual distinct substrings = number of unique substrings of length k
- Linguistic complexity = actual distinct / maximum possible

## Solution

```pascal
program LinguisticComplexity;

uses
  sysutils, sets;

var
  genome: string;
  k: integer;
  i: integer;
  substring: string;
  distinctSubstrings: TStringList;
  maxPossible: integer;
  complexity: real;

begin
  // Read input from stdin
  readln(genome);
  readln(k);
  
  // Initialize
  distinctSubstrings := TStringList.Create;
  distinctSubstrings.Sorted := true;
  distinctSubstrings.Duplicates := dupIgnore;
  
  // Find all distinct substrings of length k
  for i := 1 to Length(genome) - k + 1 do
  begin
    substring := Copy(genome, i, k);
    distinctSubstrings.Add(substring);
  end;
  
  // Calculate linguistic complexity
  maxPossible := Length(genome) - k + 1;
  complexity := distinctSubstrings.Count / maxPossible;
  
  // Output result
  writeln(Format('%.6f', [complexity]));
  
  // Clean up
  distinctSubstrings.Free;
end.
```

## Explanation

1. **Input Reading**: The program reads the genome string and the substring length k from standard input.

2. **Substring Extraction**: For each possible position in the genome (from 1 to n-k+1), it extracts a substring of length k.

3. **Distinct Substrings**: Uses a TStringList with sorted and duplicate handling to automatically track unique substrings.

4. **Complexity Calculation**: 
   - Maximum possible distinct substrings = n - k + 1
   - Actual distinct substrings = count of unique substrings found
   - Complexity = actual / maximum

5. **Output**: Prints the result formatted to 6 decimal places.

## Example

For genome "ACGTACGT" and k=3:
- Substrings of length 3: "ACG", "CGT", "GTA", "TAC", "ACG", "CGT"
- Distinct substrings: {"ACG", "CGT", "GTA", "TAC"} = 4 distinct
- Maximum possible: 8 - 3 + 1 = 6
- Complexity: 4/6 = 0.666667

## Time Complexity
- O(n×k) where n is the genome length and k is the substring length
- The substring extraction and set operations are the main contributors

## Space Complexity
- O(n×k) for storing the substrings in the set

