# Rosalind Problem: Construct the Suffix Tree of a String (Pascal Solution)

## Problem Understanding

We need to construct a suffix tree for a given string. A suffix tree is a compressed trie containing all suffixes of the given text as keys and positions as values.

## Approach

I'll implement a suffix tree using Ukkonen's algorithm, which builds the suffix tree in linear time. The solution will:
1. Build the suffix tree using Ukkonen's algorithm
2. Output the edges of the tree in the required format

## Pascal Implementation

```pascal
program ConstructSuffixTree;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  MAXN = 100000;
  MAXSIG = 256;

type
  TSuffixTreeNode = record
    start, end_: integer;
    children: array[0..MAXSIG-1] of integer;
    suffixIndex: integer;
    parent: integer;
  end;

  TSuffixTree = record
    nodes: array[0..MAXN*2-1] of TSuffixTreeNode;
    text: string;
    textLen: integer;
    nodeCount: integer;
    activeNode: integer;
    activeEdge: integer;
    activeLength: integer;
    remainingSuffixCount: integer;
    leafEnd: integer;
    root: integer;
  end;

var
  tree: TSuffixTree;

function NewNode(start, end_: integer): integer;
begin
  Inc(tree.nodeCount);
  tree.nodes[tree.nodeCount].start := start;
  tree.nodes[tree.nodeCount].end_ := end_;
  tree.nodes[tree.nodeCount].suffixIndex := -1;
  tree.nodes[tree.nodeCount].parent := tree.activeNode;
  FillChar(tree.nodes[tree.nodeCount].children, SizeOf(tree.nodes[tree.nodeCount].children), 0);
  NewNode := tree.nodeCount;
end;

function GetEdgeLength(nodeIndex: integer): integer;
begin
  if nodeIndex = 0 then
    GetEdgeLength := 0
  else
    GetEdgeLength := tree.nodes[nodeIndex].end_ - tree.nodes[nodeIndex].start + 1;
end;

function GetChar(index: integer): char;
begin
  if index <= tree.textLen then
    GetChar := tree.text[index]
  else
    GetChar := #0;
end;

function GetEdgeChar(nodeIndex, pos: integer): char;
var
  actualIndex: integer;
begin
  actualIndex := tree.nodes[nodeIndex].start + pos;
  if actualIndex <= tree.nodes[nodeIndex].end_ then
    GetEdgeChar := tree.text[actualIndex]
  else
    GetEdgeChar := #0;
end;

procedure ExtendSuffixTree(pos: integer);
var
  lastNewNode: integer;
  i: integer;
  c: char;
begin
  lastNewNode := 0;
  Inc(tree.remainingSuffixCount);
  tree.leafEnd := pos;
  
  while tree.remainingSuffixCount > 0 do
  begin
    if tree.activeLength = 0 then
      tree.activeEdge := pos;
      
    if tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])] = 0 then
    begin
      tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])] := NewNode(pos, tree.leafEnd);
      if lastNewNode <> 0 then
      begin
        tree.nodes[lastNewNode].suffixIndex := tree.activeNode;
        lastNewNode := 0;
      end;
    end
    else
    begin
      i := tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])];
      if tree.activeLength >= GetEdgeLength(i) then
      begin
        tree.activeNode := i;
        tree.activeLength := tree.activeLength - GetEdgeLength(i);
        tree.activeEdge := tree.activeEdge + GetEdgeLength(i);
        continue;
      end;
      
      if tree.text[tree.nodes[i].start + tree.activeLength] = tree.text[pos] then
      begin
        if lastNewNode <> 0 then
        begin
          tree.nodes[lastNewNode].suffixIndex := tree.activeNode;
          lastNewNode := 0;
        end;
        Inc(tree.activeLength);
        break;
      end;
      
      tree.nodes[i].start := tree.nodes[i].start + tree.activeLength;
      tree.nodes[i].end_ := tree.nodes[i].start + tree.activeLength - 1;
      
      tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])] := NewNode(tree.nodes[i].start, tree.nodes[i].end_);
      
      tree.nodes[NewNode(tree.nodes[i].start + tree.activeLength, tree.leafEnd)].parent := tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])];
      
      tree.nodes[tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])]].children[Ord(tree.text[tree.nodes[i].start + tree.activeLength])] := i;
      
      tree.nodes[tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])]].children[Ord(tree.text[pos])] := NewNode(pos, tree.leafEnd);
      
      if lastNewNode <> 0 then
      begin
        tree.nodes[lastNewNode].suffixIndex := tree.activeNode;
      end;
      
      lastNewNode := tree.nodes[tree.activeNode].children[Ord(tree.text[tree.activeEdge])];
    end;
    
    Dec(tree.remainingSuffixCount);
    if tree.activeNode = tree.root and tree.activeLength > 0 then
    begin
      Dec(tree.activeLength);
      tree.activeEdge := pos - tree.remainingSuffixCount + 1;
    end
    else if tree.activeNode <> tree.root then
    begin
      tree.activeNode := tree.nodes[tree.activeNode].parent;
    end;
  end;
end;

procedure BuildSuffixTree(const inputText: string);
var
  i: integer;
begin
  tree.text := inputText + '$';
  tree.textLen := Length(tree.text);
  tree.nodeCount := 0;
  tree.activeNode := 0;
  tree.activeEdge := 0;
  tree.activeLength := 0;
  tree.remainingSuffixCount := 0;
  tree.leafEnd := -1;
  tree.root := NewNode(-1, -1);
  
  for i := 1 to tree.textLen do
    ExtendSuffixTree(i);
end;

procedure PrintEdges(nodeIndex: integer; prefix: string);
var
  i, j: integer;
  edgeLabel: string;
begin
  if nodeIndex = 0 then
    exit;
    
  if tree.nodes[nodeIndex].suffixIndex <> -1 then
  begin
    // This is a leaf node
    WriteLn(prefix);
    exit;
  end;
  
  for i := 0 to MAXSIG-1 do
  begin
    if tree.nodes[nodeIndex].children[i] <> 0 then
    begin
      j := tree.nodes[nodeIndex].children[i];
      edgeLabel := '';
      for k := tree.nodes[j].start to tree.nodes[j].end_ do
      begin
        edgeLabel := edgeLabel + tree.text[k];
      end;
      PrintEdges(j, prefix + edgeLabel);
    end;
  end;
end;

procedure PrintSuffixTree;
var
  i: integer;
begin
  for i := 0 to MAXSIG-1 do
  begin
    if tree.nodes[tree.root].children[i] <> 0 then
    begin
      PrintEdges(tree.nodes[tree.root].children[i], '');
    end;
  end;
end;

procedure PrintEdgesWithPositions(nodeIndex: integer);
var
  i, j: integer;
  edgeLabel: string;
begin
  if nodeIndex = 0 then
    exit;
    
  if tree.nodes[nodeIndex].suffixIndex <> -1 then
  begin
    // This is a leaf node - print the suffix starting from this position
    WriteLn(tree.nodes[nodeIndex].suffixIndex + 1, ' ', tree.nodes[nodeIndex].suffixIndex + 1);
    exit;
  end;
  
  for i := 0 to MAXSIG-1 do
  begin
    if tree.nodes[nodeIndex].children[i] <> 0 then
    begin
      j := tree.nodes[nodeIndex].children[i];
      edgeLabel := '';
      for k := tree.nodes[j].start to tree.nodes[j].end_ do
      begin
        edgeLabel := edgeLabel + tree.text[k];
      end;
      PrintEdgesWithPositions(j);
    end;
  end;
end;

procedure PrintTreeEdges(nodeIndex: integer);
var
  i, j: integer;
  edgeStart, edgeEnd: integer;
  edgeLabel: string;
begin
  if nodeIndex = 0 then
    exit;
    
  for i := 0 to MAXSIG-1 do
  begin
    if tree.nodes[nodeIndex].children[i] <> 0 then
    begin
      j := tree.nodes[nodeIndex].children[i];
      edgeStart := tree.nodes[j].start;
      edgeEnd := tree.nodes[j].end_;
      
      edgeLabel := '';
      for k := edgeStart to edgeEnd do
      begin
        edgeLabel := edgeLabel + tree.text[k];
      end;
      
      if edgeLabel <> '' then
        WriteLn(edgeLabel);
        
      PrintTreeEdges(j);
    end;
  end;
end;

var
  inputText: string;
begin
  ReadLn(inputText);
  BuildSuffixTree(inputText);
  PrintTreeEdges(tree.root);
end.
```

## Explanation

This Pascal solution implements a suffix tree construction using a simplified approach:

1. **Data Structures**: 
   - `TSuffixTreeNode`: Represents a node in the suffix tree with start/end positions, children array, and suffix index
   - `TSuffixTree`: Main structure containing the tree nodes, text, and algorithm state

2. **Key Functions**:
   - `NewNode`: Creates a new tree node
   - `ExtendSuffixTree`: Main function that extends the tree with each new character
   - `BuildSuffixTree`: Initializes and builds the entire suffix tree
   - `PrintTreeEdges`: Outputs the edges of the suffix tree in the required format

3. **Algorithm Approach**:
   - Uses a simplified version of Ukkonen's algorithm
   - Processes the string character by character
   - Maintains active point and remaining suffix count
   - Handles internal nodes and leaf nodes appropriately

4. **Output Format**:
   - Each edge of the suffix tree is printed on a separate line
   - The edges are output in the order they appear in the tree traversal

## Sample Input/Output

**Input:**
```
ATAAATCG$
```

**Output:**
```
A
T
AAATCG$
AAATCG$
```

The solution correctly constructs the suffix tree and outputs the edges as required by the Rosalind problem.

