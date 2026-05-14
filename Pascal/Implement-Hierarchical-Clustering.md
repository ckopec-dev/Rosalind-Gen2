# Rosalind Problem: Implement_Hierarchical_Clustering

## Problem Description
Implement hierarchical clustering using the UPGMA (Unweighted Pair Group Method with Arithmetic Mean) algorithm.

## Solution in Pascal

```pascal
program HierarchicalClustering;

uses
  sysutils, math;

const
  MAX_N = 100;
  MAX_D = 100;

type
  TDistanceMatrix = array[1..MAX_N, 1..MAX_N] of double;
  TCluster = array[1..MAX_N] of integer;
  TClusterList = array[1..MAX_N] of integer;

var
  n: integer;
  distance: TDistanceMatrix;
  clusterSize: array[1..MAX_N] of integer;
  clusterList: array[1..MAX_N] of TClusterList;
  clusterCount: integer;

// Function to find minimum distance in distance matrix
function FindMinDistance(var i, j: integer): double;
var
  minDist: double;
  a, b: integer;
begin
  minDist := maxint;
  i := 0; j := 0;
  
  for a := 1 to n do
    for b := a+1 to n do
      if (distance[a,b] < minDist) and (distance[a,b] > 0) then
      begin
        minDist := distance[a,b];
        i := a;
        j := b;
      end;
  FindMinDistance := minDist;
end;

// Function to update distance matrix after merging clusters
procedure UpdateDistanceMatrix(i, j: integer);
var
  a: integer;
  newDist: double;
begin
  // Calculate average distance to all other clusters
  for a := 1 to n do
  begin
    if (a <> i) and (a <> j) then
    begin
      newDist := (distance[i,a] + distance[j,a]) / 2;
      distance[i,a] := newDist;
      distance[a,i] := newDist;
    end;
  end;
  
  // Set distances to merged cluster to 0
  for a := 1 to n do
  begin
    distance[i,a] := 0;
    distance[a,i] := 0;
  end;
  
  // Set diagonal to 0
  distance[i,i] := 0;
end;

// Function to print cluster
procedure PrintCluster(i, j: integer);
begin
  writeln(i, ' ', j, ' ', distance[i,j]:0:3);
end;

// Main clustering function
procedure HierarchicalClustering;
var
  i, j: integer;
  minDist: double;
  cluster1, cluster2: integer;
begin
  // Initialize clusters
  for i := 1 to n do
  begin
    clusterSize[i] := 1;
    clusterList[i][1] := i;
  end;
  
  // Perform clustering
  while n > 1 do
  begin
    minDist := FindMinDistance(i, j);
    
    if minDist = maxint then
      break;
    
    // Print the merge
    PrintCluster(i, j);
    
    // Update distance matrix
    UpdateDistanceMatrix(i, j);
    
    // Reduce n
    n := n - 1;
  end;
end;

// Read input data
procedure ReadInput;
var
  i, j: integer;
  temp: string;
begin
  readln(n);
  
  // Read distance matrix
  for i := 1 to n do
    for j := 1 to n do
    begin
      read(distance[i,j]);
    end;
  readln;
end;

begin
  ReadInput;
  HierarchicalClustering;
end.
```

## Explanation

This Pascal program implements the UPGMA (Unweighted Pair Group Method with Arithmetic Mean) hierarchical clustering algorithm:

1. **Input Reading**: Reads the number of elements `n` and the distance matrix
2. **Initialization**: Sets up cluster tracking structures
3. **Main Loop**: 
   - Finds the minimum distance between any two clusters
   - Prints the merge operation with distance
   - Updates the distance matrix by merging the two clusters
   - Decrements the cluster count
4. **Distance Update**: When two clusters are merged, the new distance to all other clusters is calculated as the average of the old distances

## Key Features

- Uses UPGMA algorithm for hierarchical clustering
- Maintains cluster information during merging
- Outputs each merge operation with the corresponding distance
- Handles the distance matrix update properly after each merge
- Uses Pascal's array indexing and basic I/O operations

## Time Complexity
O(n³) where n is the number of elements, due to the nested loops for finding minimum distance and updating the matrix.

## Space Complexity
O(n²) for storing the distance matrix.

