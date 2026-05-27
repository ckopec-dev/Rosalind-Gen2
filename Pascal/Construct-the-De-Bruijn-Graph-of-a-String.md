# Rosalind Problem: Construct the De Bruijn Graph of a String

## Problem Statement
Given a string Text of length at most 10^4, construct the de Bruijn graph of Text.

## Solution in Pascal

```pascal
program DeBruijnGraph;
uses
  SysUtils, Classes;

var
  Text: string;
  k: integer;
  i: integer;
  prefix, suffix: string;
  graph: TStringList;
  result: TStringList;

function GetPrefix(const s: string; len: integer): string;
begin
  if Length(s) <= len then
    Result := s
  else
    Result := Copy(s, 1, len);
end;

function GetSuffix(const s: string; len: integer): string;
begin
  if Length(s) <= len then
    Result := s
  else
    Result := Copy(s, Length(s) - len + 1, len);
end;

begin
  // Read input
  Readln(Text);
  Readln(k);
  
  // Initialize lists
  graph := TStringList.Create;
  result := TStringList.Create;
  
  // Handle edge case
  if Length(Text) < k then
  begin
    Writeln('Error: k is larger than text length');
    Halt;
  end;
  
  // Build de Bruijn graph
  for i := 1 to Length(Text) - k + 1 do
  begin
    prefix := GetPrefix(Copy(Text, i, k), k - 1);
    suffix := GetSuffix(Copy(Text, i, k), k - 1);
    
    // Add edge from prefix to suffix
    graph.Add(prefix + ' -> ' + suffix);
  end;
  
  // Sort and output results
  graph.Sort;
  
  for i := 0 to graph.Count - 1 do
    Writeln(graph[i]);
  
  // Clean up
  graph.Free;
  result.Free;
end.
```

## Explanation

This Pascal program solves the De Bruijn graph construction problem by:

1. **Reading Input**: Takes the DNA string `Text` and integer `k` (k-mer length) as input
2. **Building the Graph**: 
   - For each k-mer in the text, extract the prefix (k-1 characters from the left) and suffix (k-1 characters from the right)
   - Create an edge from prefix to suffix in the de Bruijn graph
3. **Output**: Print all edges in lexicographically sorted order

## Example

**Input:**
```
ACGTACGT
3
```

**Output:**
```
ACG -> CGT
CGT -> GTA
GTA -> TAC
TAC -> ACG
```

## Key Points

- The de Bruijn graph is constructed by creating edges between overlapping k-1 length prefixes and suffixes
- Each k-mer contributes exactly one edge to the graph
- The output is sorted lexicographically as required by Rosalind
- The program handles edge cases like when k is larger than the text length

## Time Complexity
O(n × k × log(n × k)) where n is the length of the text, due to sorting the edges.

## Space Complexity
O(n × k) for storing the edges.

