# Rosalind Problem: Counting Inversions in Pascal

## Problem Understanding

An inversion in an array A[1..n] is a pair of indices (i, j) such that i < j and A[i] > A[j]. The task is to count the number of inversions in a given array.

## Approach

I'll use the merge sort algorithm to count inversions efficiently in O(n log n) time. During the merge process, when we merge two sorted subarrays, we can count inversions by:
- When an element from the right subarray is smaller than an element from the left subarray, it forms inversions with all remaining elements in the left subarray

## Pascal Solution

```pascal
program CountingInversions;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TIntArray = array of integer;

var
  n: integer;
  arr: TIntArray;

// Merge function that counts inversions
function Merge(arr: TIntArray; left, mid, right: integer): int64;
var
  i, j, k: integer;
  invCount: int64;
  leftArr, rightArr: TIntArray;
  n1, n2: integer;
begin
  n1 := mid - left + 1;
  n2 := right - mid;
  
  // Create temporary arrays
  SetLength(leftArr, n1);
  SetLength(rightArr, n2);
  
  // Copy data to temporary arrays
  for i := 0 to n1 - 1 do
    leftArr[i] := arr[left + i];
  for j := 0 to n2 - 1 do
    rightArr[j] := arr[mid + 1 + j];
  
  i := 0;
  j := 0;
  k := left;
  invCount := 0;
  
  // Merge the temporary arrays back
  while (i < n1) and (j < n2) do
  begin
    if leftArr[i] <= rightArr[j] then
    begin
      arr[k] := leftArr[i];
      inc(i);
    end
    else
    begin
      arr[k] := rightArr[j];
      inc(j);
      // All elements from i to n1-1 form inversions with rightArr[j]
      invCount := invCount + (n1 - i);
    end;
    inc(k);
  end;
  
  // Copy remaining elements
  while i < n1 do
  begin
    arr[k] := leftArr[i];
    inc(i);
    inc(k);
  end;
  
  while j < n2 do
  begin
    arr[k] := rightArr[j];
    inc(j);
    inc(k);
  end;
  
  Merge := invCount;
end;

// Merge sort function that counts inversions
function MergeSort(arr: TIntArray; left, right: integer): int64;
var
  mid: integer;
  invCount: int64;
begin
  invCount := 0;
  if left < right then
  begin
    mid := (left + right) div 2;
    
    invCount := invCount + MergeSort(arr, left, mid);
    invCount := invCount + MergeSort(arr, mid + 1, right);
    invCount := invCount + Merge(arr, left, mid, right);
  end;
  MergeSort := invCount;
end;

// Main function
procedure Solve;
var
  i: integer;
  result: int64;
begin
  // Read input
  ReadLn(n);
  SetLength(arr, n);
  
  for i := 0 to n - 1 do
  begin
    Read(arr[i]);
  end;
  
  // Count inversions
  result := MergeSort(arr, 0, n - 1);
  
  // Output result
  WriteLn(result);
end;

begin
  Solve;
end.
```

## Input/Output Format

**Input:**
- First line: n (size of array)
- Second line: n space-separated integers

**Output:**
- Single integer: number of inversions in the array

## Example

**Input:**
```
5
2 3 8 6 1
```

**Output:**
```
5
```

**Explanation:**
The inversions are: (2,1), (3,1), (8,6), (8,1), (6,1) = 5 inversions

## Time and Space Complexity

- **Time Complexity:** O(n log n) - Standard merge sort complexity
- **Space Complexity:** O(n) - For temporary arrays during merge process

## Key Points

1. The solution uses the divide-and-conquer approach of merge sort
2. During the merge step, when we take an element from the right subarray, we know it's smaller than all remaining elements in the left subarray
3. This gives us the count of inversions in O(n) time during each merge operation
4. The overall complexity is O(n log n) which is efficient for large inputs

