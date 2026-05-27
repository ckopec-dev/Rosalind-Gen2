# Rosalind Problem: Shortest Cycle Through a Given Edge

## Problem Understanding

We need to find the shortest cycle in an undirected graph that passes through a given edge. This is a classic graph theory problem that can be solved by:
1. Removing the given edge from the graph
2. Finding the shortest path between the two vertices of that edge in the remaining graph
3. Adding the original edge back to form the cycle

## Solution Approach

The key insight is that the shortest cycle through a given edge (u,v) is formed by:
- The edge (u,v) itself
- The shortest path between u and v in the graph with (u,v) removed
- This creates the shortest possible cycle containing the given edge

## Pascal Implementation

```pascal
program ShortestCycleThroughEdge;

uses
  sysutils, classes;

const
  MAXN = 1000;
  INF = 1000000000;

type
  TGraph = array[1..MAXN, 1..MAXN] of integer;
  TQueue = array[1..MAXN] of integer;
  TBoolArray = array[1..MAXN] of boolean;

var
  graph: TGraph;
  visited: TBoolArray;
  queue: TQueue;
  n, m: integer;
  u, v: integer;

// Function to perform BFS and find shortest path
function bfs(start, end: integer): integer;
var
  head, tail, current, i: integer;
begin
  // Initialize queue and visited array
  head := 1;
  tail := 1;
  queue[1] := start;
  fillchar(visited, sizeof(visited), false);
  visited[start] := true;
  
  // BFS loop
  while head <= tail do
  begin
    current := queue[head];
    inc(head);
    
    // If we reached the destination
    if current = end then
    begin
      bfs := 0; // This shouldn't happen in our case, but return 0
      exit;
    end;
    
    // Check all neighbors
    for i := 1 to n do
    begin
      if (graph[current, i] = 1) and (not visited[i]) then
      begin
        visited[i] := true;
        inc(tail);
        queue[tail] := i;
      end;
    end;
  end;
  
  // If we get here, there's no path
  bfs := INF;
end;

// Function to find shortest cycle through edge (u,v)
function shortestCycleThroughEdge: integer;
var
  pathLength: integer;
begin
  // Temporarily remove the edge (u,v)
  graph[u, v] := 0;
  graph[v, u] := 0;
  
  // Find shortest path between u and v in the remaining graph
  pathLength := bfs(u, v);
  
  // Restore the edge
  graph[u, v] := 1;
  graph[v, u] := 1;
  
  // If no path exists, no cycle exists through this edge
  if pathLength = INF then
  begin
    shortestCycleThroughEdge := -1; // No cycle
  end
  else
  begin
    // Cycle length = path length + 1 (the edge itself)
    shortestCycleThroughEdge := pathLength + 1;
  end;
end;

// Read input from stdin
procedure readInput;
var
  i: integer;
  a, b: integer;
begin
  readln(n, m);
  
  // Initialize graph
  fillchar(graph, sizeof(graph), 0);
  
  // Read edges
  for i := 1 to m do
  begin
    readln(a, b);
    graph[a, b] := 1;
    graph[b, a] := 1;
  end;
  
  // Read the edge for which we want to find shortest cycle
  readln(u, v);
end;

// Main program
begin
  readInput;
  writeln(shortestCycleThroughEdge);
end.
```

## Alternative Implementation with Proper BFS Distance Tracking

```pascal
program ShortestCycleThroughEdge;

uses
  sysutils;

const
  MAXN = 1000;
  INF = 1000000000;

type
  TGraph = array[1..MAXN, 1..MAXN] of integer;
  TBoolArray = array[1..MAXN] of boolean;
  TIntArray = array[1..MAXN] of integer;

var
  graph: TGraph;
  visited: TBoolArray;
  distance: TIntArray;
  n, m: integer;
  u, v: integer;

// BFS to find shortest path between two vertices
function bfsShortestPath(start, end: integer): integer;
var
  queue: array[1..MAXN] of integer;
  head, tail, current, i: integer;
begin
  // Initialize
  head := 1;
  tail := 1;
  queue[1] := start;
  fillchar(visited, sizeof(visited), false);
  fillchar(distance, sizeof(distance), 0);
  visited[start] := true;
  
  // BFS
  while head <= tail do
  begin
    current := queue[head];
    inc(head);
    
    // If we reached the destination
    if current = end then
    begin
      bfsShortestPath := distance[current];
      exit;
    end;
    
    // Check all neighbors
    for i := 1 to n do
    begin
      if (graph[current, i] = 1) and (not visited[i]) then
      begin
        visited[i] := true;
        distance[i] := distance[current] + 1;
        inc(tail);
        queue[tail] := i;
      end;
    end;
  end;
  
  // No path found
  bfsShortestPath := INF;
end;

// Find shortest cycle through edge (u,v)
function findShortestCycle: integer;
var
  shortestPath: integer;
begin
  // Remove the edge temporarily
  graph[u, v] := 0;
  graph[v, u] := 0;
  
  // Find shortest path between u and v
  shortestPath := bfsShortestPath(u, v);
  
  // Restore the edge
  graph[u, v] := 1;
  graph[v, u] := 1;
  
  // If no path exists, no cycle exists
  if shortestPath = INF then
  begin
    findShortestCycle := -1; // No cycle exists
  end
  else
  begin
    // Cycle length = path + 1 (the edge)
    findShortestCycle := shortestPath + 1;
  end;
end;

// Read input
procedure readInput;
var
  i, a, b: integer;
begin
  readln(n, m);
  
  // Initialize graph
  fillchar(graph, sizeof(graph), 0);
  
  // Read edges
  for i := 1 to m do
  begin
    readln(a, b);
    graph[a, b] := 1;
    graph[b, a] := 1;
  end;
  
  // Read the specific edge
  readln(u, v);
end;

begin
  readInput;
  writeln(findShortestCycle);
end.
```

## Key Points

1. **Algorithm**: Remove the given edge, find shortest path between the two vertices, add the edge back
2. **Time Complexity**: O(V + E) for BFS, where V is vertices and E is edges
3. **Space Complexity**: O(V²) for adjacency matrix representation
4. **Edge Cases**: If no path exists between the vertices, return -1 (no cycle possible)

## Example Usage

Input format:
```
4 5
1 2
2 3
3 4
4 1
1 3
2 4
```

The algorithm will find the shortest cycle through edge (2,4) by removing it, finding the shortest path between vertices 2 and 4, and then adding the edge back to form the cycle.

