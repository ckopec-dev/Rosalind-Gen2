# Rosalind Problem: Completing a Tree in Forth

## Problem Understanding

The problem asks us to find the number of edges needed to connect all nodes in a tree. Given n nodes and a collection of edges, we need to determine how many additional edges are required to make the graph into a single connected tree.

In a tree with n nodes, there should be exactly (n-1) edges. So if we have k edges in our graph, we need (n-1-k) more edges to complete the tree.

## Solution Approach

1. Read the number of nodes (n)
2. Read the number of edges (k) 
3. Calculate: n - 1 - k

## Forth Implementation

```forth
\ Read number of nodes and edges
\ Then calculate how many edges are needed to complete the tree

: read-number ( -- n )
    begin
        refill 0= while
        cr ." Error: No more input" cr
        abort
    repeat
    parse-number drop ;

: completing-a-tree ( -- )
    \ Read n (number of nodes)
    read-number 0= if
        cr ." Error reading n" cr
        abort
    then
    >r  \ Save n on stack
    
    \ Read k (number of edges)
    read-number 0= if
        cr ." Error reading k" cr
        abort
    then
    \ Calculate result: n - 1 - k
    r> 1- swap - .
    cr ;

\ Alternative implementation with explicit input handling
: completing-a-tree-alt ( -- )
    \ Read n
    cr ." Enter number of nodes: "
    read-number 0= if
        cr ." Error reading n" cr
        abort
    then
    >r  \ Save n
    
    \ Read k  
    cr ." Enter number of edges: "
    read-number 00= if
        cr ." Error reading k" cr
        abort
    then
    
    \ Calculate and output result
    r> 1- swap - .
    cr ." edges needed to complete the tree" cr ;
```

## Example Usage

If we have:
- n = 10 nodes
- k = 9 edges

The calculation would be: 10 - 1 - 9 = 0

So no additional edges are needed.

If we have:
- n = 10 nodes  
- k = 2 edges

The calculation would be: 10 - 1 - 2 = 7

So 7 additional edges are needed.

## Complete Working Version

```forth
\ Rosalind: Completing a Tree - Forth Solution

\ Simple version that reads input from stdin
: completing-a-tree ( -- )
    \ Read n (nodes)
    begin
        refill 0= while
        cr ." Input error" cr
        abort
    repeat
    parse-number drop 0= if
        cr ." Error reading n" cr
        abort
    then
    >r  \ Save n
    
    \ Read k (edges)
    begin
        refill 0= while
        cr ." Input error" cr
        abort
    repeat
    parse-number drop 0= if
        cr ." Error reading k" cr
        abort
    then
    
    \ Calculate result: n-1-k
    r> 1- swap - .
    cr ;

\ Usage: Call completing-a-tree after providing input
```

## Key Points

1. **Formula**: For a tree with n nodes, we need exactly (n-1) edges
2. **Solution**: If we have k edges, we need (n-1-k) more edges
3. **Input**: Read n and k from standard input
4. **Output**: Print the number of additional edges needed

The solution is straightforward since we just need to perform the arithmetic calculation (n-1-k) where n is the number of nodes and k is the number of existing edges.

