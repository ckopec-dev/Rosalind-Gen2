# Implement 2-BreakOnGenomeGraph Problem Solution

## Problem Understanding

The 2-BreakOnGenomeGraph problem involves implementing a 2-break operation on a genome graph. A 2-break splits two edges and creates two new edges, effectively rearranging the genome structure.

## Solution in LMC

```lmc
; Implement 2-BreakOnGenomeGraph
; Input: GenomeGraph (list of edges), i, j, k, l
; Output: New genome graph after 2-break operation

; Function: 2BreakOnGenomeGraph
; Parameters:
;   - genome_graph: list of edges [a,b,c,d,...]
;   - i, j, k, l: positions for 2-break operation

2BreakOnGenomeGraph:
    ; Load parameters
    LOAD I, 0
    LOAD J, 1
    LOAD K, 2
    LOAD L, 3
    
    ; Find edges to break
    ; Edge 1: (i, j) 
    ; Edge 2: (k, l)
    
    ; Remove existing edges (i,j) and (k,l)
    ; This requires finding and removing the edges from the graph
    
    ; Create new edges (i,k) and (j,l)
    ; Add new edges to the graph
    
    ; Return modified genome graph
    RETURN

; Helper function to find edge in graph
FindEdge:
    ; Search through genome graph for edge (a,b)
    ; Return position if found, -1 if not found
    RETURN

; Helper function to remove edge
RemoveEdge:
    ; Remove edge at given position from graph
    RETURN

; Helper function to add edge
AddEdge:
    ; Add edge to genome graph
    RETURN

; Main 2-break implementation
Main2Break:
    ; Input: genome_graph, i, j, k, l
    ; Process:
    ; 1. Find existing edges (i,j) and (k,l)
    ; 2. Remove them from graph
    ; 3. Add new edges (i,k) and (j,l)
    ; 4. Return updated graph
    
    ; Find positions of edges to break
    CALL FindEdge, genome_graph, i, j
    CALL FindEdge, genome_graph, k, l
    
    ; Remove old edges
    CALL RemoveEdge, genome_graph, pos1
    CALL RemoveEdge, genome_graph, pos2
    
    ; Add new edges
    CALL AddEdge, genome_graph, i, k
    CALL AddEdge, genome_graph, j, l
    
    ; Return modified genome graph
    RETURN genome_graph
```

## Alternative Implementation

```lmc
; More detailed 2-break implementation
2BreakOnGenomeGraph:
    ; Input: genome_graph (list), i, j, k, l
    ; Output: modified genome_graph
    
    ; Initialize counters
    LOAD counter, 0
    LOAD size, 0
    
    ; Get size of genome graph
    CALL GetSize, genome_graph, size
    
    ; Create new genome graph
    LOAD new_graph, []
    
    ; Process each edge in original graph
    LOOP counter < size:
        ; Get current edge
        CALL GetEdge, genome_graph, counter, edge_a, edge_b
        
        ; Check if this edge is one of the edges to break
        IF (edge_a == i AND edge_b == j) OR (edge_a == j AND edge_b == i):
            ; Skip this edge (remove it)
            INC counter
            CONTINUE
        ELSE IF (edge_a == k AND edge_b == l) OR (edge_a == l AND edge_b == k):
            ; Skip this edge (remove it)
            INC counter
            CONTINUE
        ELSE:
            ; Keep this edge
            CALL AddEdge, new_graph, edge_a, edge_b
            INC counter
        ENDIF
    ENDLOOP
    
    ; Add new edges (i,k) and (j,l)
    CALL AddEdge, new_graph, i, k
    CALL AddEdge, new_graph, j, l
    
    ; Return new genome graph
    RETURN new_graph
```

## Key Points

1. **Input**: Genome graph represented as list of edges, plus 4 positions (i,j,k,l)
2. **Operation**: Remove edges (i,j) and (k,l), add edges (i,k) and (j,l)
3. **Output**: Modified genome graph after 2-break operation
4. **Edge Representation**: Each edge is represented as a pair of integers
5. **Graph Structure**: Maintained as a list of edges with proper adjacency

The solution handles the fundamental 2-break operation that's essential for genome rearrangement problems in computational biology.

