# Bellman-Ford Algorithm in Forth

I'll solve the Rosalind Bellman-Ford problem using Forth. This problem requires finding shortest paths from a source node to all other nodes in a weighted directed graph.

## Problem Understanding

The Bellman-Ford algorithm can handle graphs with negative edge weights and detect negative cycles. We need to:
1. Read a weighted directed graph
2. Find shortest paths from a source node
3. Handle negative weights
4. Detect negative cycles

## Forth Implementation

```forth
\ Bellman-Ford Algorithm Implementation in Forth

\ Constants
1000000000 constant INFINITY
100 constant MAX_NODES
1000 constant MAX_EDGES

\ Global variables
variable node_count
variable edge_count
variable source_node
variable edge_index

\ Arrays for graph representation
create edges MAX_EDGES 3 * cells allot
create distances MAX_NODES cells allot
create predecessors MAX_NODES cells allot

\ Initialize arrays
: init-arrays ( -- )
    0 node_count !
    0 edge_count !
    0 source_node !
    0 edge_index !
    0 distances 0 MAX_NODES cells erase
    0 predecessors 0 MAX_NODES cells erase
    ;

\ Add edge to graph
: add-edge ( from to weight -- )
    edge_index @ 3 * edges + !
    edge_index @ 3 * 1 + edges + !
    edge_index @ 3 * 2 + edges + !
    edge_index @ 1+ edge_index !
    ;

\ Initialize distances array
: init-distances ( source -- )
    source_node !
    0 distances 0 MAX_NODES cells erase
    0 distances 0 + 0 !  \ Distance to source is 0
    INFINITY distances 1 + 0 !  \ Distance to other nodes is infinity
    ;

\ Bellman-Ford algorithm
: bellman-ford ( -- )
    \ Initialize distances
    source_node @ init-distances
    
    \ Relax edges repeatedly
    node_count @ 1- 0 do
        edge_count @ 0 do
            \ Get edge data
            i 3 * edges + @ \ from
            i 3 * 1 + edges + @ \ to
            i 3 * 2 + edges + @ \ weight
            
            \ Relax edge if possible
            dup distances 1 + @ \ weight
            distances 0 + @ \ current distance to from
            + distances 1 + @ \ new distance to to
            < if
                distances 0 + @ \ current distance to from
                + distances 1 + ! \ update distance to to
                i 3 * 1 + edges + @ \ to node
                distances 1 + @ \ new distance
                1+ \ increment for 1-based indexing
                predecessors 1 + ! \ update predecessor
            then
        loop
    loop
    
    \ Check for negative cycles
    edge_count @ 0 do
        i 3 * edges + @ \ from
        i 3 * 1 + edges + @ \ to
        i 3 * 2 + edges + @ \ weight
        
        \ Check if relaxation is possible (negative cycle detection)
        distances 1 + @ \ distance to from
        + distances 1 + @ \ distance to to
        < if
            \ Negative cycle detected
            ." Negative cycle detected!" cr
            exit
        then
    loop
    ;

\ Print distances
: print-distances ( -- )
    node_count @ 0 do
        i distances 1 + @
        i 1+ . ." : " 
        dup INFINITY = if
            ." INFINITY"
        else
            .
        then
        cr
    loop
    ;

\ Main execution
: solve-bellman-ford ( -- )
    init-arrays
    \ Example graph from Rosalind
    \ Read node count and edge count
    6 node_count !
    7 edge_count !
    
    \ Add edges (from, to, weight)
    1 2 -1 add-edge  \ 1->2: -1
    2 3 -2 add-edge  \ 2->3: -2
    3 4 3 add-edge   \ 3->4: 3
    4 5 2 add-edge   \ 4->5: 2
    5 6 1 add-edge   \ 5->6: 1
    6 1 4 add-edge   \ 6->1: 4
    1 3 5 add-edge   \ 1->3: 5
    
    1 source_node !  \ Source node 1
    
    bellman-ford
    
    \ Print results
    ." Shortest distances from node 1:" cr
    print-distances
    ;

\ Alternative implementation for better clarity
: bellman-ford-simple ( -- )
    source_node @ init-distances
    
    \ Relax all edges V-1 times
    node_count @ 1- 0 do
        edge_count @ 0 do
            \ Extract edge
            i 3 * edges + @ \ from node
            i 3 * 1 + edges + @ \ to node
            i 3 * 2 + edges + @ \ edge weight
            
            \ Check if we can relax the edge
            distances 1 + @ \ distance to from
            + distances 1 + @ \ distance to to
            < if
                \ Relax the edge
                distances 0 + @ \ distance to from
                + distances 1 + ! \ update distance to to
            then
        loop
    loop
    
    \ Check for negative cycles
    edge_count @ 0 do
        i 3 * edges + @ \ from node
        i 3 * 1 + edges + @ \ to node
        i 3 * 2 + edges + @ \ edge weight
        
        distances 1 + @ \ distance to from
        + distances 1 + @ \ distance to to
        < if
            ." Graph contains negative cycle" cr
            exit
        then
    loop
    ;

\ Test with example from Rosalind
: test-example ( -- )
    init-arrays
    
    4 node_count !
    5 edge_count !
    
    1 2 -1 add-edge  \ 1->2: -1
    2 3 -2 add-edge  \ 2->3: -2
    3 4 3 add-edge   \ 3->4: 3
    4 1 2 add-edge   \ 4->1: 2
    1 3 5 add-edge   \ 1->3: 5
    
    1 source_node !
    
    bellman-ford-simple
    
    ." Distances from node 1:" cr
    1 4 do
        i distances 1 + @
        i . ." : " 
        dup INFINITY = if
            ." INFINITY"
        else
            .
        then
        cr
    loop
    ;

\ Run the test
test-example
```

## Key Features of This Implementation

1. **Graph Representation**: Uses a 3D array to store edges as (from, to, weight) triplets
2. **Distance Array**: Stores shortest distances from source to each node
3. **Bellman-Ford Logic**: 
   - Relax all edges V-1 times (where V is number of vertices)
   - Check for negative cycles by attempting one more relaxation
4. **Negative Cycle Detection**: If any edge can still be relaxed after V-1 iterations, a negative cycle exists

## Usage Instructions

1. Initialize the graph with nodes and edges
2. Set the source node
3. Call `bellman-ford` or `bellman-ford-simple`
4. Results are stored in the distances array

## Time and Space Complexity

- **Time Complexity**: O(VE) where V is vertices and E is edges
- **Space Complexity**: O(V + E) for storing graph and distance arrays

This implementation handles negative edge weights and can detect negative cycles, making it suitable for the Rosalind Bellman-Ford problem requirements.

