# Rosalind Problem: Heap Sort in Pascal

## Problem Understanding

Heap sort is a comparison-based sorting algorithm that uses a binary heap data structure. The algorithm works by:
1. Building a max heap from the input array
2. Repeatedly extracting the maximum element and rebuilding the heap

## Solution

```pascal
program HeapSort;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure Heapify(var arr: array of integer; n, i: integer);
var
  largest, left, right: integer;
  temp: integer;
begin
  largest := i;
  left := 2 * i + 1;
  right := 2 * i + 2;

  // If left child exists and is greater than root
  if (left < n) and (arr[left] > arr[largest]) then
    largest := left;

  // If right child exists and is greater than largest so far
  if (right < n) and (arr[right] > arr[largest]) then
    largest := right;

  // If largest is not root
  if largest <> i then
  begin
    temp := arr[i];
    arr[i] := arr[largest];
    arr[largest] := temp;

    // Recursively heapify the affected sub-tree
    Heapify(arr, n, largest);
  end;
end;

procedure HeapSort(var arr: array of integer; n: integer);
var
  i, temp: integer;
begin
  // Build heap (rearrange array)
  for i := n div 2 - 1 downto 0 do
    Heapify(arr, n, i);

  // One by one extract elements from heap
  for i := n - 1 downto 1 do
  begin
    // Move current root to end
    temp := arr[0];
    arr[0] := arr[i];
    arr[i] := temp;

    // Call heapify on the reduced heap
    Heapify(arr, i, 0);
  end;
end;

procedure PrintArray(arr: array of integer; n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
    write(arr[i], ' ');
  writeln;
end;

var
  n, i: integer;
  arr: array of integer;
  input: string;
  numbers: array of integer;
  count: integer;

begin
  // Read input from stdin
  readln(n);
  
  // Read the array elements
  setlength(arr, n);
  for i := 0 to n - 1 do
    read(arr[i]);
  
  // Perform heap sort
  HeapSort(arr, n);
  
  // Print sorted array
  PrintArray(arr, n);
  
  readln;
end.
```

## Explanation

The solution implements the heap sort algorithm with the following key components:

1. **Heapify procedure**: Maintains the heap property by ensuring that the largest element is at the root
2. **HeapSort procedure**: 
   - First builds a max heap from the input array
   - Then repeatedly extracts the maximum element and rebuilds the heap
3. **Main program**: Reads input, calls the sorting function, and outputs the result

## Time and Space Complexity

- **Time Complexity**: O(n log n) for all cases
- **Space Complexity**: O(1) as it's an in-place sorting algorithm

## Sample Input/Output

**Input:**
```
5
1 3 5 7 2
```

**Output:**
```
1 2 3 5 7
```

The algorithm correctly sorts the array using the heap sort technique, which first converts the array into a max heap and then repeatedly extracts the maximum element to build the sorted output.

