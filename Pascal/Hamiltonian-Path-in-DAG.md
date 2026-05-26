# Rosalind Problem: Hamiltonian Path in DAG - Pascal Solution

## Problem Understanding

A Hamiltonian path in a directed graph is a path that visits each vertex exactly once. For a Directed Acyclic Graph (DAG), we need to find if there exists a path that visits all vertices exactly once.

## Approach

1. **Topological Sort**: Since it's a DAG, we can perform topological sorting to get vertices in order
2. **Path Validation**: Check if there's a Hamiltonian path by ensuring each consecutive pair in topological order has a direct edge
3. **Alternative Approach**: Use dynamic programming with bitmasks to check all possible paths

## Solution

```pascal
program HamiltonianPathInDAG;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TGraph = record
    vertices: integer;
    adjList: array of array of integer;
    inDegree: array of integer;
  end;

  TStack = array of integer;
  TQueue = array of integer;

procedure ReadGraph(var g: TGraph; var edges: array of array of integer; 
                   n, m: integer);
var
  i, u, v: integer;
begin
  g.vertices := n;
  SetLength(g.adjList, n);
  SetLength(g.inDegree, n);
  
  for i := 0 to m - 1 do
  begin
    u := edges[i][0] - 1;  // Convert to 0-based indexing
    v := edges[i][1] - 1;
    SetLength(g.adjList[u], Length(g.adjList[u]) + 1);
    g.adjList[u][High(g.adjList[u])] := v;
    Inc(g.inDegree[v]);
  end;
end;

function HasHamiltonianPath(var g: TGraph): boolean;
var
  i, j, u, v, currentVertex, count, topologicalOrder: integer;
  visited: array of boolean;
  queue: TQueue;
  topOrder: array of integer;
  inDeg: array of integer;
begin
  SetLength(inDeg, g.vertices);
  for i := 0 to g.vertices - 1 do
    inDeg[i] := g.inDegree[i];
  
  SetLength(queue, 0);
  SetLength(topOrder, 0);
  
  // Find vertices with in-degree 0
  for i := 0 to g.vertices - 1 do
  begin
    if inDeg[i] = 0 then
    begin
      SetLength(queue, Length(queue) + 1);
      queue[High(queue)] := i;
    end;
  end;
  
  // Topological sort
  count := 0;
  while Length(queue) > 0 do
  begin
    currentVertex := queue[0];
    SetLength(topOrder, Length(topOrder) + 1);
    topOrder[High(topOrder)] := currentVertex;
    Inc(count);
    
    // Remove vertex from queue
    for i := 0 to High(queue) - 1 do
      queue[i] := queue[i + 1];
    SetLength(queue, Length(queue) - 1);
    
    // Update in-degrees
    for i := 0 to High(g.adjList[currentVertex]) do
    begin
      v := g.adjList[currentVertex][i];
      Dec(inDeg[v]);
      if inDeg[v] = 0 then
      begin
        SetLength(queue, Length(queue) + 1);
        queue[High(queue)] := v;
      end;
    end;
  end;
  
  // Check if all vertices were processed (no cycles)
  if count <> g.vertices then
  begin
    Result := false;
    Exit;
  end;
  
  // Check if there's a Hamiltonian path
  // In a DAG, if we can find a topological order where each consecutive pair
  // has a direct edge, then it's a Hamiltonian path
  if Length(topOrder) = 1 then
  begin
    Result := true;
    Exit;
  end;
  
  // Check if consecutive vertices in topological order are connected
  for i := 0 to Length(topOrder) - 2 do
  begin
    u := topOrder[i];
    v := topOrder[i + 1];
    
    // Check if there's an edge from u to v
    Result := false;
    for j := 0 to High(g.adjList[u]) do
    begin
      if g.adjList[u][j] = v then
      begin
        Result := true;
        Break;
      end;
    end;
    
    if not Result then
    begin
      Exit;
    end;
  end;
  
  Result := true;
end;

function FindHamiltonianPath(var g: TGraph): string;
var
  i, j, u, v, currentVertex, count, topologicalOrder: integer;
  visited: array of boolean;
  queue: TQueue;
  topOrder: array of integer;
  inDeg: array of integer;
begin
  SetLength(inDeg, g.vertices);
  for i := 0 to g.vertices - 1 do
    inDeg[i] := g.inDegree[i];
  
  SetLength(queue, 0);
  SetLength(topOrder, 0);
  
  // Find vertices with in-degree 0
  for i := 0 to g.vertices - 1 do
  begin
    if inDeg[i] = 0 then
    begin
      SetLength(queue, Length(queue) + 1);
      queue[High(queue)] := i;
    end;
  end;
  
  // Topological sort
  count := 0;
  while Length(queue) > 0 do
  begin
    currentVertex := queue[0];
    SetLength(topOrder, Length(topOrder) + 1);
    topOrder[High(topOrder)] := currentVertex;
    Inc(count);
    
    // Remove vertex from queue
    for i := 0 to High(queue) - 1 do
      queue[i] := queue[i + 1];
    SetLength(queue, Length(queue) - 1);
    
    // Update in-degrees
    for i := 0 to High(g.adjList[currentVertex]) do
    begin
      v := g.adjList[currentVertex][i];
      Dec(inDeg[v]);
      if inDeg[v] = 0 then
      begin
        SetLength(queue, Length(queue) + 1);
        queue[High(queue)] := v;
      end;
    end;
  end;
  
  // Check if all vertices were processed (no cycles)
  if count <> g.vertices then
  begin
    Result := '-1';
    Exit;
  end;
  
  // Check if there's a Hamiltonian path
  if Length(topOrder) = 1 then
  begin
    Result := IntToStr(topOrder[0] + 1);
    Exit;
  end;
  
  // Check if consecutive vertices in topological order are connected
  for i := 0 to Length(topOrder) - 2 do
  begin
    u := topOrder[i];
    v := topOrder[i + 1];
    
    // Check if there's an edge from u to v
    Result := 'false';
    for j := 0 to High(g.adjList[u]) do
    begin
      if g.adjList[u][j] = v then
      begin
        Result := 'true';
        Break;
      end;
    end;
    
    if Result = 'false' then
    begin
      Result := '-1';
      Exit;
    end;
  end;
  
  // Build path string
  Result := '';
  for i := 0 to High(topOrder) do
  begin
    if i > 0 then
      Result := Result + ' ';
    Result := Result + IntToStr(topOrder[i] + 1);
  end;
end;

var
  n, m, i, j: integer;
  edges: array of array of integer;
  g: TGraph;
  result: string;

begin
  // Read input
  ReadLn(n, m);
  
  SetLength(edges, m, 2);
  for i := 0 to m - 1 do
  begin
    ReadLn(edges[i][0], edges[i][1]);
  end;
  
  // Build graph
  ReadGraph(g, edges, n, m);
  
  // Find Hamiltonian path
  result := FindHamiltonianPath(g);
  
  // Output result
  WriteLn(result);
end.
```

## Explanation

This solution works as follows:

1. **Graph Representation**: Uses adjacency list representation for the DAG
2. **Topological Sort**: Performs topological sorting using Kahn's algorithm to order vertices
3. **Hamiltonian Path Check**: 
   - If we can sort all vertices in topological order
   - And each consecutive pair in this order has a direct edge
   - Then we have a Hamiltonian path

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing the graph and auxiliary data structures

## Key Points
- The algorithm works specifically for DAGs where we can perform topological sorting
- If the topological order exists and consecutive vertices are connected, we have a Hamiltonian path
- The solution handles edge cases like single vertex graphs and disconnected components

The output will be either:
- A space-separated list of vertices representing the Hamiltonian path (1-indexed)
- "-1" if no Hamiltonian path exists

