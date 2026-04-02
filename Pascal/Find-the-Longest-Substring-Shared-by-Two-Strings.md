# Rosalind Problem: Find the Longest Substring Shared by Two Strings (Pascal Solution)

## Problem Understanding

We need to find the longest substring that appears in both input strings. This is a classic longest common substring problem.

## Approach

I'll use dynamic programming to solve this problem:
1. Create a 2D table to store lengths of common substrings
2. Fill the table by comparing characters from both strings
3. Track the maximum length and ending position
4. Extract the actual substring from the tracked position

## Solution

```pascal
program LongestCommonSubstring;

{$APPTYPE CONSOLE}

uses
  SysUtils, StrUtils;

function longestCommonSubstring(s1, s2: string): string;
var
  m, n, i, j, maxLength, endingPos: integer;
  dp: array of array of integer;
  result: string;
begin
  m := Length(s1);
  n := Length(s2);
  
  // Handle edge cases
  if (m = 0) or (n = 0) then
  begin
    Result := '';
    Exit;
  end;
  
  // Create DP table
  SetLength(dp, m + 1, n + 1);
  
  // Initialize variables to track maximum
  maxLength := 0;
  endingPos := 0;
  
  // Fill the DP table
  for i := 1 to m do
  begin
    for j := 1 to n do
    begin
      if s1[i] = s2[j] then
      begin
        dp[i][j] := dp[i-1][j-1] + 1;
        if dp[i][j] > maxLength then
        begin
          maxLength := dp[i][j];
          endingPos := i;
        end;
      end
      else
      begin
        dp[i][j] := 0;
      end;
    end;
  end;
  
  // Extract the longest common substring
  if maxLength = 0 then
    Result := ''
  else
    Result := Copy(s1, endingPos - maxLength + 1, maxLength);
end;

var
  s1, s2, result: string;

begin
  // Read input strings
  Readln(s1);
  Readln(s2);
  
  // Find and print the longest common substring
  result := longestCommonSubstring(s1, s2);
  Writeln(result);
  
  Readln; // Wait for user input before closing
end.
```

## Explanation

### Algorithm Steps:

1. **Input Processing**: Read two strings from standard input
2. **Dynamic Programming Table**: Create a 2D array `dp` where `dp[i][j]` represents the length of common substring ending at position `i` in `s1` and position `j` in `s2`
3. **Table Filling**: 
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = 0`
4. **Track Maximum**: Keep track of the maximum length found and the ending position
5. **Extract Result**: Use the ending position and maximum length to extract the actual substring

### Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

### Example:
For input strings "ABABC" and "BABCA":
- Common substrings: "B", "A", "B", "AB", "ABC", "BC"
- Longest common substring: "ABC"

This solution efficiently finds the longest substring shared by two strings using dynamic programming approach.

