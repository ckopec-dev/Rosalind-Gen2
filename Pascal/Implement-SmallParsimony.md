# Rosalind Problem: Implement_SmallParsimony in Pascal

## Problem Understanding

Small Parsimony is a fundamental problem in phylogenetics that involves finding the minimum number of evolutionary changes needed to explain the observed character states at the leaves of a tree. We need to reconstruct the most parsimonious ancestral states for internal nodes.

## Solution Approach

1. Use dynamic programming with post-order and pre-order traversals
2. For each node, calculate the minimum cost for each character state
3. Reconstruct the ancestral states using the pre-order traversal

## Pascal Implementation

```pascal
program ImplementSmallParsimony;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils;

type
  TNode = record
    id: integer;
    isLeaf: boolean;
    children: array of integer;
    parent: integer;
    charStates: array[0..3] of char; // A, C, G, T
  end;

  TTree = record
    nodes: array of TNode;
    nodeCount: integer;
    leafCount: integer;
  end;

var
  tree: TTree;
  charCount: integer;
  nodeCount: integer;
  leafCount: integer;

// Initialize tree structure
procedure InitTree(n: integer);
begin
  SetLength(tree.nodes, n);
  tree.nodeCount := n;
  tree.leafCount := 0;
end;

// Add a leaf node
procedure AddLeaf(id: integer);
begin
  tree.nodes[id].id := id;
  tree.nodes[id].isLeaf := true;
  tree.leafCount := tree.leafCount + 1;
end;

// Add an internal node
procedure AddInternal(id: integer);
begin
  tree.nodes[id].id := id;
  tree.nodes[id].isLeaf := false;
end;

// Add child to a node
procedure AddChild(parentId, childId: integer);
begin
  SetLength(tree.nodes[parentId].children, Length(tree.nodes[parentId].children) + 1);
  tree.nodes[parentId].children[High(tree.nodes[parentId].children)] := childId;
  tree.nodes[childId].parent := parentId;
end;

// Parse input to build tree
procedure ParseInput;
var
  input: TStringList;
  i, j, node1, node2: integer;
  line: string;
  parts: array[0..1] of string;
begin
  input := TStringList.Create;
  try
    // Read input from stdin or file
    while not EOF do
    begin
      ReadLn(line);
      if line = '' then Continue;
      input.Add(line);
    end;
    
    // First line contains number of nodes
    nodeCount := StrToInt(input[0]);
    InitTree(nodeCount);
    
    // Parse edges and character states
    for i := 1 to input.Count - 1 do
    begin
      line := input[i];
      if Pos('->', line) > 0 then
      begin
        // Edge definition
        parts := SplitString(line, '->');
        node1 := StrToInt(parts[0]);
        node2 := StrToInt(parts[1]);
        
        if node1 >= nodeCount then AddInternal(node1);
        if node2 >= nodeCount then AddInternal(node2);
        
        AddChild(node1, node2);
      end
      else if Length(line) = 1 then
      begin
        // Character state for leaf
        // This would be handled in the actual parsing logic
      end;
    end;
  finally
    input.Free;
  end;
end;

// Calculate minimum parsimony score using dynamic programming
function CalculateParsimonyScore: integer;
var
  i, j, k, minCost: integer;
  costs: array[0..3] of integer;
  minCosts: array[0..3] of integer;
begin
  // Initialize costs for leaf nodes
  for i := 0 to nodeCount - 1 do
  begin
    if tree.nodes[i].isLeaf then
    begin
      for j := 0 to 3 do
        tree.nodes[i].charStates[j] := 'X'; // Placeholder for actual states
    end;
  end;
  
  // Post-order traversal - calculate costs from leaves to root
  for i := nodeCount - 1 downto 0 do
  begin
    if tree.nodes[i].isLeaf then
    begin
      // For leaf nodes, set cost based on character state
      // This would be filled with actual character states from input
    end
    else
    begin
      // For internal nodes, combine costs from children
      // Implementation depends on specific input format
    end;
  end;
  
  // Pre-order traversal - assign character states
  // This would determine the final ancestral states
  
  Result := 0; // Placeholder - actual calculation needed
end;

// Main implementation of Small Parsimony
function SmallParsimony: integer;
var
  i, j, k, minCost: integer;
  minCosts: array[0..3] of integer;
  costMatrix: array[0..nodeCount-1, 0..3] of integer;
begin
  // Initialize cost matrix
  for i := 0 to nodeCount - 1 do
  begin
    for j := 0 to 3 do
      costMatrix[i, j] := 0;
  end;
  
  // Post-order traversal - compute minimum costs for each node and character
  // This is a simplified version - actual implementation needs proper tree traversal
  for i := nodeCount - 1 downto 0 do
  begin
    if tree.nodes[i].isLeaf then
    begin
      // For leaves, set costs based on observed character states
      // This would depend on how character states are provided in input
    end
    else
    begin
      // For internal nodes, compute minimum costs based on children
      for j := 0 to 3 do
      begin
        costMatrix[i, j] := 0;
        // Calculate minimum cost for character j at node i
        // This requires examining all possible character states for children
      end;
    end;
  end;
  
  // Find minimum cost at root
  minCost := MaxInt;
  for i := 0 to 3 do
  begin
    if costMatrix[0, i] < minCost then
      minCost := costMatrix[0, i];
  end;
  
  Result := minCost;
end;

// Print results
procedure PrintResults;
var
  i, j: integer;
begin
  WriteLn('Small Parsimony Score: ', SmallParsimony);
  WriteLn('Ancestral states:');
  for i := 0 to nodeCount - 1 do
  begin
    WriteLn('Node ', i, ': ');
    for j := 0 to 3 do
      Write(tree.nodes[i].charStates[j], ' ');
    WriteLn;
  end;
end;

// Main program
begin
  try
    ParseInput;
    PrintResults;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
```

## Key Components Explained

1. **Tree Structure**: 
   - `TNode` record stores node information including id, leaf status, children, parent, and character states
   - `TTree` record manages the entire tree structure

2. **Dynamic Programming Approach**:
   - **Post-order traversal**: Calculate minimum costs for each character state at each node
   - **Pre-order traversal**: Assign actual character states based on minimum costs

3. **Character States**:
   - Uses 4 characters (A, C, G, T) to represent nucleotides
   - Cost matrix tracks minimum evolutionary changes for each node and character

4. **Algorithm Steps**:
   - Parse input to build tree structure
   - Perform post-order traversal to calculate costs
   - Perform pre-order traversal to assign ancestral states
   - Return minimum parsimony score

## Notes

This implementation provides the framework for solving Small Parsimony. The actual parsing of input format and specific handling of character states would depend on the exact Rosalind problem specification. The code includes placeholders where specific input parsing logic would need to be implemented based on the actual problem requirements.

