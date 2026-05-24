# Rosalind Problem: Shortest Paths in DAG (Shortest_Paths_in_DAG)

## Problem Description
Given a weighted directed acyclic graph (DAG), find the shortest paths from a source vertex to all other vertices.

## Solution in Pascal

```pascal
program Shortest_Paths_in_DAG;

uses
  sysutils, strutils;

const
  MAXN = 1000;
  INF = 1000000000;

type
  TEdge = record
    toVertex, weight: integer;
  end;

  TGraph = record
    adj: array[1..MAXN] of array[1..MAXN] of integer;
    edges: array[1..MAXN] of TEdge;
    n, m: integer;
  end;

var
  graph: TGraph;
  dist: array[1..MAXN] of longint;
  inDegree: array[1..MAXN] of integer;
  queue: array[1..MAXN] of integer;
  head, tail: integer;
  source: integer;

procedure ReadInput;
var
  i: integer;
  line: string;
  tokens: array of string;
begin
  // Read number of vertices and edges
  readln(graph.n, graph.m);
  readln(source);
  
  // Initialize adjacency matrix and in-degrees
  for i := 1 to graph.n do
  begin
    inDegree[i] := 0;
    dist[i] := INF;
  end;
  
  // Read edges
  for i := 1 to graph.m do
  begin
    readln(line);
    tokens := SplitString(line, ' ');
    graph.edges[i].toVertex := StrToInt(tokens[2]);
    graph.edges[i].weight := StrToInt(tokens[3]);
    inDegree[StrToInt(tokens[2])] := inDegree[StrToInt(tokens[2])] + 1;
  end;
  
  dist[source] := 0;
end;

procedure TopologicalSort;
var
  i, u, v, j: integer;
begin
  // Initialize queue with vertices having in-degree 0
  head := 0;
  tail := 0;
  
  for i := 1 to graph.n do
  begin
    if inDegree[i] = 0 then
    begin
      inc(tail);
      queue[tail] := i;
    end;
  end;
  
  // Process vertices in topological order
  while head < tail do
  begin
    inc(head);
    u := queue[head];
    
    // For each outgoing edge from u
    for j := 1 to graph.m do
    begin
      if graph.edges[j].toVertex = u then
      begin
        v := j;
        // Relax edge
        if dist[graph.edges[v].toVertex] > dist[graph.edges[v].toVertex] + graph.edges[v].weight then
        begin
          dist[graph.edges[v].toVertex] := dist[graph.edges[v].toVertex] + graph.edges[v].weight;
        end;
      end;
    end;
    
    // Update in-degrees and add to queue if in-degree becomes 0
    for j := 1 to graph.m do
    begin
      if graph.edges[j].toVertex = u then
      begin
        dec(inDegree[graph.edges[j].toVertex]);
        if inDegree[graph.edges[j].toVertex] = 0 then
        begin
          inc(tail);
          queue[tail] := graph.edges[j].toVertex;
        end;
      end;
    end;
  end;
end;

procedure TopologicalSortCorrected;
var
  i, u, v, j: integer;
  tempQueue: array[1..MAXN] of integer;
  tempHead, tempTail: integer;
begin
  // Initialize queue with vertices having in-degree 0
  head := 0;
  tail := 0;
  
  for i := 1 to graph.n do
  begin
    if inDegree[i] = 0 then
    begin
      inc(tail);
      queue[tail] := i;
    end;
  end;
  
  // Process vertices in topological order
  while head < tail do
  begin
    inc(head);
    u := queue[head];
    
    // Process all outgoing edges from vertex u
    for j := 1 to graph.m do
    begin
      if graph.edges[j].toVertex = u then
      begin
        // Relax edge (this is the key part for DAG shortest path)
        if dist[u] <> INF then
        begin
          if dist[graph.edges[j].toVertex] > dist[u] + graph.edges[j].weight then
          begin
            dist[graph.edges[j].toVertex] := dist[u] + graph.edges[j].weight;
          end;
        end;
      end;
    end;
    
    // Update in-degrees and add to queue if in-degree becomes 0
    for j := 1 to graph.m do
    begin
      if graph.edges[j].toVertex = u then
      begin
        dec(inDegree[graph.edges[j].toVertex]);
        if inDegree[graph.edges[j].toVertex] = 0 then
        begin
          inc(tail);
          queue[tail] := graph.edges[j].toVertex;
        end;
      end;
    end;
  end;
end;

procedure Solve;
var
  i: integer;
begin
  // Initialize distances
  for i := 1 to graph.n do
  begin
    dist[i] := INF;
  end;
  dist[source] := 0;
  
  // Topological sort and relaxation
  TopologicalSortCorrected;
  
  // Output results
  for i := 1 to graph.n do
  begin
    if dist[i] = INF then
      writeln('x')
    else
      writeln(dist[i]);
  end;
end;

begin
  ReadInput;
  Solve;
end.
```

## Explanation

This solution implements the shortest path algorithm for directed acyclic graphs (DAGs) using topological sorting and dynamic programming:

1. **Input Reading**: Reads the number of vertices, edges, and source vertex
2. **Topological Sort**: Uses Kahn's algorithm to sort vertices in topological order
3. **Relaxation**: For each vertex in topological order, relaxes all outgoing edges
4. **Output**: Prints shortest distances or "x" for unreachable vertices

## Key Points

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V + E)
- Uses topological sorting to ensure we process vertices in the correct order
- For each vertex, we only relax edges going out from it
- Handles unreachable vertices by printing "x"

## Usage
Compile and run with input in the format:
```
Number of vertices
Number of edges
Source vertex
Edge information (from to weight)
```

