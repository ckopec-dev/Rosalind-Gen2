# Rosalind Problem: Majority Element in Pascal

## Problem Understanding
The majority element in an array is the element that appears more than ⌊n/2⌋ times, where n is the array size. We need to find such an element if it exists.

## Solution Approach
I'll use Boyer-Moore Majority Vote Algorithm which works in O(n) time and O(1) space:
1. Find a candidate using voting process
2. Verify if candidate is actually majority element

## Pascal Implementation

```pascal
program MajorityElement;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TIntArray = array of integer;

function FindMajorityElement(arr: TIntArray): integer;
var
  candidate, count, i: integer;
begin
  // Phase 1: Find candidate using Boyer-Moore voting
  candidate := arr[0];
  count := 1;
  
  for i := 1 to High(arr) do
  begin
    if arr[i] = candidate then
      count := count + 1
    else
      count := count - 1;
      
    if count = 0 then
    begin
      candidate := arr[i];
      count := 1;
    end;
  end;
  
  // Phase 2: Verify candidate is majority element
  count := 0;
  for i := 0 to High(arr) do
  begin
    if arr[i] = candidate then
      count := count + 1;
  end;
  
  // Return candidate if it's actually majority, otherwise -1
  if count > Length(arr) div 2 then
    FindMajorityElement := candidate
  else
    FindMajorityElement := -1;
end;

function ReadArrayFromFile(filename: string): TIntArray;
var
  fileHandle: TextFile;
  line: string;
  values: TStringArray;
  i: integer;
begin
  SetLength(ReadArrayFromFile, 0);
  
  AssignFile(fileHandle, filename);
  Reset(fileHandle);
  
  // Skip first line (problem description)
  ReadLn(fileHandle);
  
  // Read the array
  ReadLn(fileHandle, line);
  values := SplitString(line, ' ');
  
  SetLength(ReadArrayFromFile, Length(values));
  for i := 0 to High(values) do
    ReadArrayFromFile[i] := StrToInt(values[i]);
  
  CloseFile(fileHandle);
end;

procedure SolveRosalindProblem;
var
  arr: TIntArray;
  result: integer;
  i: integer;
begin
  // For demonstration, let's create a sample array
  // In practice, you would read from file or stdin
  SetLength(arr, 5);
  arr[0] := 3;
  arr[1] := 3;
  arr[2] := 4;
  arr[3] := 2;
  arr[4] := 3;
  
  result := FindMajorityElement(arr);
  
  if result <> -1 then
    WriteLn('Majority element: ', result)
  else
    WriteLn('No majority element found');
    
  // Alternative approach for multiple test cases
  WriteLn('Testing with different arrays:');
  
  // Test case 1: [3, 3, 4, 2, 3]
  SetLength(arr, 5);
  arr[0] := 3; arr[1] := 3; arr[2] := 4; arr[3] := 2; arr[4] := 3;
  result := FindMajorityElement(arr);
  WriteLn('Array [3,3,4,2,3]: ', result);
  
  // Test case 2: [1, 1, 1, 2, 2]
  SetLength(arr, 5);
  arr[0] := 1; arr[1] := 1; arr[2] := 1; arr[3] := 2; arr[4] := 2;
  result := FindMajorityElement(arr);
  WriteLn('Array [1,1,1,2,2]: ', result);
  
  // Test case 3: [1, 2, 3, 4, 5]
  SetLength(arr, 5);
  arr[0] := 1; arr[1] := 2; arr[2] := 3; arr[3] := 4; arr[4] := 5;
  result := FindMajorityElement(arr);
  WriteLn('Array [1,2,3,4,5]: ', result);
end;

begin
  SolveRosalindProblem;
end.
```

## Alternative Simpler Version

```pascal
program MajorityElementSimple;

{$mode objfpc}{$H+}

uses
  SysUtils;

function MajorityElement(arr: array of integer): integer;
var
  i, j, count: integer;
  n: integer;
begin
  n := Length(arr);
  if n = 0 then
  begin
    MajorityElement := -1;
    exit;
  end;
  
  // Check each element
  for i := 0 to n - 1 do
  begin
    count := 0;
    for j := 0 to n - 1 do
    begin
      if arr[j] = arr[i] then
        count := count + 1;
    end;
    
    if count > n div 2 then
    begin
      MajorityElement := arr[i];
      exit;
    end;
  end;
  
  MajorityElement := -1; // No majority element
end;

begin
  // Example usage
  var arr1: array[0..4] of integer = (3, 3, 4, 2, 3);
  var arr2: array[0..4] of integer = (1, 1, 1, 2, 2);
  var arr3: array[0..4] of integer = (1, 2, 3, 4, 5);
  
  WriteLn('Array [3,3,4,2,3]: ', MajorityElement(arr1));
  WriteLn('Array [1,1,1,2,2]: ', MajorityElement(arr2));
  WriteLn('Array [1,2,3,4,5]: ', MajorityElement(arr3));
end.
```

## Key Features of the Solution

1. **Efficient Algorithm**: Uses Boyer-Moore Majority Vote Algorithm for O(n) time complexity
2. **Space Optimized**: Uses O(1) extra space
3. **Verification**: Properly verifies that the candidate is actually the majority element
4. **Error Handling**: Returns -1 when no majority element exists
5. **Multiple Test Cases**: Includes test cases to demonstrate functionality

## Time and Space Complexity

- **Time Complexity**: O(n) - two passes through the array
- **Space Complexity**: O(1) - only using a constant amount of extra space

The solution correctly handles the Rosalind majority element problem requirements while being efficient and readable in Pascal.

