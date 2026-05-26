# Rosalind Problem: Bellman-Ford Algorithm in Pascal

## Problem Description
The Bellman-Ford algorithm finds the shortest paths from a single source vertex to all other vertices in a weighted graph, even when negative edge weights are present.

## Solution

```pascal
program BellmanFord;
{$APPTYPE CONSOLE}

uses
  SysUtils, StrUtils;

const
  INF = 2147483647;  // Large number representing infinity
  MAXN = 1000;      // Maximum number of vertices

type
  TEdge = record
    u, v, w: integer;  // source, destination, weight
  end;

var
  n, m: integer;           // number of vertices and edges
  edges: array[1..MAXN] of TEdge;
  dist: array[1..MAXN] of integer;
  parent: array[1..MAXN] of integer;
  i, j, u, v, w: integer;
  updated: boolean;
  negativeCycle: boolean;

procedure ReadInput;
var
  line: string;
  tokens: array of string;
  edgeIndex: integer;
begin
  // Read number of vertices and edges
  Readln(n, m);
  
  // Read edges
  edgeIndex := 1;
  for i := 1 to m do
  begin
    Readln(line);
    tokens := SplitString(line, ' ');
    u := StrToInt(tokens[0]);
    v := StrToInt(tokens[1]);
    w := StrToInt(tokens[2]);
    edges[edgeIndex].u := u;
    edges[edgeIndex].v := v;
    edges[edgeIndex].w := w;
    inc(edgeIndex);
  end;
end;

procedure BellmanFordAlgorithm;
var
  k: integer;
begin
  // Initialize distances
  for i := 1 to n do
  begin
    dist[i] := INF;
    parent[i] := 0;
  end;
  
  // Set source distance to 0 (assuming source is vertex 1)
  dist[1] := 0;
  
  // Relax edges repeatedly
  for k := 1 to n - 1 do
  begin
    updated := false;
    for i := 1 to m do
    begin
      u := edges[i].u;
      v := edges[i].v;
      w := edges[i].w;
      
      if (dist[u] < INF) and (dist[u] + w < dist[v]) then
      begin
        dist[v] := dist[u] + w;
        parent[v] := u;
        updated := true;
      end;
    end;
    
    // If no updates, we can stop early
    if not updated then
      break;
  end;
  
  // Check for negative cycles
  negativeCycle := false;
  for i := 1 to m do
  begin
    u := edges[i].u;
    v := edges[i].v;
    w := edges[i].w;
    
    if (dist[u] < INF) and (dist[u] + w < dist[v]) then
    begin
      negativeCycle := true;
      break;
    end;
  end;
end;

procedure PrintResults;
var
  i: integer;
begin
  if negativeCycle then
  begin
    Writeln('negative cycle');
    exit;
  end;
  
  for i := 1 to n do
  begin
    if dist[i] = INF then
      Writeln('infinity')
    else
      Writeln(dist[i]);
  end;
end;

begin
  ReadInput;
  BellmanFordAlgorithm;
  PrintResults;
end.
```

## Explanation

This Pascal implementation of the Bellman-Ford algorithm:

1. **Input Reading**: Reads the number of vertices `n` and edges `m`, followed by `m` edges in the format "u v w" (source, destination, weight)

2. **Initialization**: 
   - Sets all distances to infinity except the source (vertex 1)
   - Initializes parent array for path reconstruction

3. **Main Algorithm**:
   - Runs `n-1` iterations to relax all edges
   - In each iteration, updates distances if a shorter path is found
   - Early termination if no updates occur

4. **Negative Cycle Detection**:
   - After `n-1` iterations, checks if any further improvements are possible
   - If so, a negative cycle exists

5. **Output**:
   - Prints "negative cycle" if detected
   - Otherwise, prints shortest distances from source to all vertices

## Time Complexity
- **Time**: O(n×m) where n is vertices and m is edges
- **Space**: O(n+m) for storing edges and distances

## Sample Input/Output

**Input:**
```
4 5
1 2 4
1 3 2
2 3 3
2 4 2
3 4 4
```

**Output:**
```
0
4
2
6
```

This implementation handles the standard Bellman-Ford algorithm requirements and correctly detects negative cycles in the graph.

