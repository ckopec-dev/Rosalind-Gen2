# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Description
Given a directed acyclic graph (DAG), find a topological ordering of its vertices.

## Solution in Pascal

```pascal
program FindTopologicalOrdering;

uses
  sysutils, classes;

type
  TGraph = class
  private
    vertices: array of integer;
    adjList: array of array of integer;
    inDegree: array of integer;
    vertexCount: integer;
    edgeCount: integer;
  public
    constructor Create(n: integer);
    destructor Destroy; override;
    procedure AddEdge(u, v: integer);
    function TopologicalSort: TStringList;
    procedure PrintGraph;
  end;

constructor TGraph.Create(n: integer);
begin
  vertexCount := n;
  SetLength(vertices, n);
  SetLength(adjList, n);
  SetLength(inDegree, n);
  for var i := 0 to n-1 do
    vertices[i] := i + 1;
end;

destructor TGraph.Destroy;
begin
  inherited Destroy;
end;

procedure TGraph.AddEdge(u, v: integer);
begin
  // Convert to 0-based indexing
  u := u - 1;
  v := v - 1;
  SetLength(adjList[u], Length(adjList[u]) + 1);
  adjList[u][High(adjList[u])] := v;
  inDegree[v] := inDegree[v] + 1;
  edgeCount := edgeCount + 1;
end;

function TGraph.TopologicalSort: TStringList;
var
  queue: array of integer;
  result: TStringList;
  front, rear: integer;
  current: integer;
  i: integer;
begin
  result := TStringList.Create;
  SetLength(queue, vertexCount);
  front := 0;
  rear := 0;
  
  // Initialize queue with vertices having in-degree 0
  for i := 0 to vertexCount - 1 do
  begin
    if inDegree[i] = 0 then
    begin
      queue[rear] := i;
      rear := rear + 1;
    end;
  end;
  
  // Process vertices in topological order
  while front < rear do
  begin
    current := queue[front];
    front := front + 1;
    result.Add(IntToStr(current + 1)); // Convert back to 1-based
    
    // Remove current vertex and update in-degrees
    for i := 0 to High(adjList[current]) do
    begin
      var neighbor := adjList[current][i];
      inDegree[neighbor] := inDegree[neighbor] - 1;
      if inDegree[neighbor] = 0 then
      begin
        queue[rear] := neighbor;
        rear := rear + 1;
      end;
    end;
  end;
  
  // Check if there's a cycle (not a DAG)
  if result.Count <> vertexCount then
  begin
    result.Clear;
    result.Add('Graph contains a cycle - not a DAG');
  end;
  
  TopologicalSort := result;
end;

procedure TGraph.PrintGraph;
var
  i, j: integer;
begin
  writeln('Graph structure:');
  for i := 0 to vertexCount - 1 do
  begin
    write('Vertex ', i + 1, ': ');
    for j := 0 to High(adjList[i]) do
      write(adjList[i][j] + 1, ' ');
    writeln;
  end;
  writeln('In-degrees:');
  for i := 0 to vertexCount - 1 do
    write('Vertex ', i + 1, ': ', inDegree[i], ' ');
  writeln;
end;

var
  graph: TGraph;
  result: TStringList;
  i: integer;
  n, m: integer;
  u, v: integer;
  input: TextFile;

begin
  // Read input from file (assuming standard input format)
  // Format: First line contains number of vertices and edges
  // Next lines contain edges (u v)
  
  Assign(input, 'rosalind_topo.txt');
  Reset(input);
  
  ReadLn(input, n, m);
  
  graph := TGraph.Create(n);
  
  // Read edges
  for i := 1 to m do
  begin
    ReadLn(input, u, v);
    graph.AddEdge(u, v);
  end;
  
  Close(input);
  
  // Perform topological sort
  result := graph.TopologicalSort;
  
  // Output result
  for i := 0 to result.Count - 1 do
    writeln(result[i]);
  
  result.Free;
  graph.Free;
end.
```

## Alternative Simpler Version

```pascal
program TopologicalSort;

uses
  sysutils, classes;

var
  n, m: integer;
  i, u, v: integer;
  adjList: array[1..1000, 1..1000] of integer;
  inDegree: array[1..1000] of integer;
  queue: array[1..1000] of integer;
  front, rear: integer;
  result: array[1..1000] of integer;
  resultCount: integer;

begin
  // Read input
  readln(n, m);
  
  // Initialize
  for i := 1 to n do
    inDegree[i] := 0;
  front := 1;
  rear := 0;
  resultCount := 0;
  
  // Read edges and build adjacency list and in-degree
  for i := 1 to m do
  begin
    readln(u, v);
    inDegree[v] := inDegree[v] + 1;
    inc(rear);
    adjList[u, rear] := v;
  end;
  
  // Initialize queue with vertices of in-degree 0
  for i := 1 to n do
    if inDegree[i] = 0 then
    begin
      inc(rear);
      queue[rear] := i;
    end;
  
  // Process vertices
  while front <= rear do
  begin
    u := queue[front];
    front := front + 1;
    inc(resultCount);
    result[resultCount] := u;
    
    // Process neighbors
    for i := 1 to n do
    begin
      if adjList[u, i] <> 0 then
      begin
        v := adjList[u, i];
        inDegree[v] := inDegree[v] - 1;
        if inDegree[v] = 0 then
        begin
          inc(rear);
          queue[rear] := v;
        end;
      end;
    end;
  end;
  
  // Output result
  for i := 1 to resultCount do
    write(result[i], ' ');
  writeln;
end.
```

## Key Points:

1. **Algorithm**: Uses Kahn's algorithm for topological sorting
2. **Time Complexity**: O(V + E) where V is vertices and E is edges
3. **Space Complexity**: O(V + E)
4. **Cycle Detection**: If the number of vertices in result is less than total vertices, there's a cycle
5. **Output**: A valid topological ordering of the DAG

The solution handles the standard input format where the first line contains the number of vertices and edges, followed by the edges themselves.

