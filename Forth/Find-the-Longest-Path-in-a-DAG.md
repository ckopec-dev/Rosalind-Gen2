# Find the Longest Path in a DAG - Forth Solution

## Problem Understanding

We need to find the longest path in a directed acyclic graph (DAG). This is a classic dynamic programming problem that can be solved using topological sorting.

## Approach

1. **Topological Sort**: Process nodes in topological order
2. **Dynamic Programming**: For each node, calculate the longest path ending at that node
3. **Track Maximum**: Keep track of the maximum path length found

## Forth Implementation

```forth
\ Find the Longest Path in a DAG
\ Using topological sorting and dynamic programming

\ Constants
: MAX_NODES 1000 ;
: INF 1000000 ;

\ Global arrays
create in-degrees MAX_NODES cells allot
create dist MAX_NODES cells allot
create visited MAX_NODES cells allot

\ Graph representation
create graph MAX_NODES MAX_NODES * cells allot
create weights MAX_NODES MAX_NODES * cells allot

\ Initialize arrays
: init-arrays ( -- )
    0 do
        0 in-degrees i cells + !
        0 dist i cells + !
        0 visited i cells + !
    loop ;

\ Add edge to graph
: add-edge ( from to weight -- )
    >r
    r@ graph 2dup * cells + + !
    r> weights 2dup * cells + + ! ;

\ Topological sort using Kahn's algorithm
: topological-sort ( -- )
    \ Initialize queue with nodes having in-degree 0
    create queue MAX_NODES cells allot
    0 queue 0 cells + !
    0 queue 1 cells + !
    0 queue 2 cells + !
    0 queue 3 cells + !
    0 queue 4 cells + !
    
    \ Find nodes with in-degree 0
    0 do
        in-degrees i cells + @ 0= if
            1 queue 0 cells + @ + queue 0 cells + !
            i queue queue 0 cells + @ cells + !
        then
    loop ;

\ Find longest path
: longest-path ( n -- max-length )
    dup >r
    init-arrays
    
    \ Initialize distances to negative infinity
    0 do
        -INF dist i cells + !
    loop
    
    \ Set starting node distance to 0
    0 dist 0 cells + ! 0 dist 0 cells + ! 0 dist 0 cells + !
    
    \ Process nodes in topological order
    0 do
        \ Get current node
        i queue queue 0 cells + @ cells + @
        
        \ Update neighbors
        0 do
            \ Check if there's an edge
            graph i cells + j cells + @ 0> if
                \ Update distance if longer path found
                dist i cells + @
                weights i cells + j cells + @
                + dist j cells + @ >
                if
                    dist i cells + @ weights i cells + j cells + @ + dist j cells + !
                then
            then
        loop
    loop ;
    
    \ Find maximum distance
    0 do
        dist i cells + @ max
    loop ;

\ Example usage
: solve-example ( -- )
    \ Initialize
    0 init-arrays
    
    \ Add edges (from to weight)
    1 2 3 add-edge
    1 3 2 add-edge
    2 4 4 add-edge
    2 5 1 add-edge
    3 4 5 add-edge
    3 5 6 add-edge
    4 6 2 add-edge
    5 6 3 add-edge
    
    \ Find longest path
    6 longest-path . ;
```

## Alternative Implementation

Here's a cleaner version focusing on the core algorithm:

```forth
\ Simplified longest path in DAG
: longest-path-dag ( n edges -- max-length )
    \ n = number of nodes
    \ edges = array of edges (from to weight)
    
    \ Initialize
    0 do
        0 dist i cells + !
        0 in-degrees i cells + !
    loop
    
    \ Count in-degrees
    0 do
        \ Process each edge
        i 3 * cells + 1 cells + @ in-degrees 1 cells + !
    loop
    
    \ Topological processing
    0 do
        \ Find node with in-degree 0
        in-degrees i cells + @ 0= if
            \ Process neighbors
            0 do
                \ Check if edge exists
                graph i cells + j cells + @ 0> if
                    \ Update distance
                    dist i cells + @ weights i cells + j cells + @ + dist j cells + @ max
                    dist j cells + !
                then
            loop
            \ Decrease in-degrees of neighbors
            0 do
                graph i cells + j cells + @ 0> if
                    in-degrees j cells + @ 1- in-degrees j cells + !
                then
            loop
        then
    loop ;
```

## Key Concepts

1. **Topological Sort**: Process nodes in order where all dependencies are handled first
2. **Dynamic Programming**: For each node, maintain the longest path to that node
3. **In-degree Tracking**: Keep track of how many incoming edges each node has
4. **Edge Relaxation**: Update path lengths when better paths are found

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing the graph and auxiliary arrays

## Usage Notes
This implementation assumes:
- Nodes are numbered from 0 to n-1
- Input is provided as edges with weights
- The graph is guaranteed to be acyclic
- We want the longest path from node 0 to all other nodes

The solution uses topological sorting to ensure we process nodes in the correct order, then applies dynamic programming to find the maximum path lengths.

