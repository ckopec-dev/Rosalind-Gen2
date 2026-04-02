# Bellman-Ford Algorithm in LMC (Little Man Computer)

## Problem Understanding

The Bellman-Ford algorithm finds the shortest paths from a single source vertex to all other vertices in a weighted graph, even when negative edge weights are present. It can also detect negative cycles.

## LMC Implementation

```assembly
; Bellman-Ford Algorithm in LMC
; Finds shortest paths from source vertex 0 to all other vertices

        INP             ; Read number of vertices (N)
        STA NODES
        INP             ; Read number of edges (M)
        STA EDGES
        LDA #0          ; Initialize edge counter
        STA EDGE_COUNT

        ; Initialize distance array (distance to source = 0, others = 999)
        LDA #100        ; Start address for distance array
        STA DIST_ADDR
        LDA #0          ; Distance to source vertex = 0
        STA DIST_ADDR
        LDA #101        ; Next address
        STA DIST_ADDR
        LDA #999        ; Distance to other vertices = 999
        STA DIST_ADDR

        ; Read edges and store in memory
        LDA #100        ; Start of edge storage
        STA EDGE_PTR
        LDA #0          ; Edge counter
        STA EDGE_COUNT

READ_EDGES
        LDA EDGE_COUNT
        LDA EDGES
        SUB #1          ; Compare with number of edges
        BRZ DONE_READING

        ; Read edge (from, to, weight)
        INP             ; Read from vertex
        STA (EDGE_PTR)  ; Store from
        LDA #1
        ADD EDGE_PTR
        STA EDGE_PTR
        INP             ; Read to vertex
        STA (EDGE_PTR)  ; Store to
        LDA #1
        ADD EDGE_PTR
        STA EDGE_PTR
        INP             ; Read weight
        STA (EDGE_PTR)  ; Store weight
        LDA #1
        ADD EDGE_PTR
        STA EDGE_PTR
        LDA EDGE_COUNT
        ADD #1
        STA EDGE_COUNT
        BRA READ_EDGES

DONE_READING
        ; Bellman-Ford algorithm
        LDA #100        ; Initialize iteration counter
        STA ITER_COUNT
        LDA #0          ; Initialize vertex counter
        STA VERTEX_COUNT

MAIN_LOOP
        LDA ITER_COUNT
        LDA NODES
        SUB #1          ; Compare with N-1
        BRZ DONE_BELLMAN

        ; For each edge (u,v,w)
        LDA #100        ; Reset edge pointer
        STA EDGE_PTR
        LDA #0          ; Reset edge counter
        STA EDGE_COUNT

EDGE_LOOP
        LDA EDGE_COUNT
        LDA EDGES
        SUB #1          ; Compare with number of edges
        BRZ NEXT_ITERATION

        ; Load edge data
        LDA (EDGE_PTR)  ; Load from vertex
        STA FROM_VERTEX
        LDA #1
        ADD EDGE_PTR
        STA EDGE_PTR
        LDA (EDGE_PTR)  ; Load to vertex
        STA TO_VERTEX
        LDA #1
        ADD EDGE_PTR
        STA EDGE_PTR
        LDA (EDGE_PTR)  ; Load weight
        STA WEIGHT

        ; Relax edge: if dist[from] + weight < dist[to]
        LDA FROM_VERTEX
        LDA DIST_ADDR
        ADD #1          ; Get address of from vertex distance
        STA DIST_ADDR
        LDA (DIST_ADDR) ; Load distance to from vertex
        LDA WEIGHT
        ADD #0          ; Add weight
        STA TEMP_SUM
        LDA TO_VERTEX
        LDA DIST_ADDR
        ADD #1          ; Get address of to vertex distance
        STA DIST_ADDR
        LDA (DIST_ADDR) ; Load distance to to vertex
        LDA TEMP_SUM
        SUB #0          ; Compare: dist[from] + weight < dist[to]
        BRP SKIP_RELAX  ; If not less, skip relaxation

        ; Perform relaxation
        LDA TO_VERTEX
        LDA DIST_ADDR
        ADD #1          ; Get address of to vertex distance
        STA DIST_ADDR
        LDA TEMP_SUM
        STA (DIST_ADDR) ; Update distance

SKIP_RELAX
        LDA #3          ; Move to next edge
        ADD EDGE_PTR
        STA EDGE_PTR
        LDA EDGE_COUNT
        ADD #1
        STA EDGE_COUNT
        BRA EDGE_LOOP

NEXT_ITERATION
        LDA ITER_COUNT
        ADD #1
        STA ITER_COUNT
        BRA MAIN_LOOP

DONE_BELLMAN
        ; Check for negative cycles
        LDA #100        ; Reset edge pointer
        STA EDGE_PTR
        LDA #0          ; Reset edge counter
        STA EDGE_COUNT

NEG_CYCLE_CHECK
        LDA EDGE_COUNT
        LDA EDGES
        SUB #1          ; Compare with number of edges
        BRZ PRINT_RESULTS

        ; Load edge data
        LDA (EDGE_PTR)  ; Load from vertex
        STA FROM_VERTEX
        LDA #1
        ADD EDGE_PTR
        STA EDGE_PTR
        LDA (EDGE_PTR)  ; Load to vertex
        STA TO_VERTEX
        LDA #1
        ADD EDGE_PTR
        STA EDGE_PTR
        LDA (EDGE_PTR)  ; Load weight
        STA WEIGHT

        ; Check if relaxation is still possible
        LDA FROM_VERTEX
        LDA DIST_ADDR
        ADD #1          ; Get address of from vertex distance
        STA DIST_ADDR
        LDA (DIST_ADDR) ; Load distance to from vertex
        LDA WEIGHT
        ADD #0          ; Add weight
        STA TEMP_SUM
        LDA TO_VERTEX
        LDA DIST_ADDR
        ADD #1          ; Get address of to vertex distance
        STA DIST_ADDR
        LDA (DIST_ADDR) ; Load distance to to vertex
        LDA TEMP_SUM
        SUB #0          ; Compare: dist[from] + weight < dist[to]
        BRP SKIP_NEG_CYCLE  ; If not less, no negative cycle

        ; Negative cycle detected
        LDA #1          ; Set negative cycle flag
        STA NEG_CYCLE_FLAG
        BRA PRINT_RESULTS

SKIP_NEG_CYCLE
        LDA #3          ; Move to next edge
        ADD EDGE_PTR
        STA EDGE_PTR
        LDA EDGE_COUNT
        ADD #1
        STA EDGE_COUNT
        BRA NEG_CYCLE_CHECK

PRINT_RESULTS
        ; Print distances
        LDA #100        ; Start of distance array
        STA DIST_ADDR
        LDA #0          ; Initialize vertex counter
        STA VERTEX_COUNT

PRINT_LOOP
        LDA VERTEX_COUNT
        LDA NODES
        SUB #1          ; Compare with number of vertices
        BRZ END_PROGRAM

        LDA DIST_ADDR
        ADD #1          ; Get distance
        STA DIST_ADDR
        LDA (DIST_ADDR)
        OUT             ; Output distance
        LDA #1
        ADD VERTEX_COUNT
        STA VERTEX_COUNT
        BRA PRINT_LOOP

END_PROGRAM
        HLT

; Data areas
NODES           DAT     ; Number of vertices
EDGES           DAT     ; Number of edges
EDGE_COUNT      DAT     ; Edge counter
DIST_ADDR       DAT     ; Distance array pointer
FROM_VERTEX     DAT     ; From vertex of edge
TO_VERTEX       DAT     ; To vertex of edge
WEIGHT          DAT     ; Weight of edge
TEMP_SUM        DAT     ; Temporary sum storage
ITER_COUNT      DAT     ; Iteration counter
VERTEX_COUNT    DAT     ; Vertex counter
NEG_CYCLE_FLAG  DAT     ; Flag for negative cycle detection
EDGE_PTR        DAT     ; Edge pointer
```

## Algorithm Explanation

1. **Input Reading**: 
   - Read number of vertices (N) and edges (M)
   - Read all edges in format (from, to, weight)

2. **Initialization**:
   - Initialize distance array with 0 for source vertex and 999 for others
   - Set up memory locations for edge storage

3. **Main Loop** (N-1 iterations):
   - For each edge (u,v,w), check if relaxation is possible
   - If dist[u] + w < dist[v], update dist[v]

4. **Negative Cycle Detection**:
   - Run one more iteration to check if any distances can still be improved
   - If so, a negative cycle exists

5. **Output**:
   - Print shortest distances from source to all vertices

## Key Features

- **Memory Management**: Uses LMC's limited memory efficiently
- **Edge Storage**: Stores edges in sequential memory locations
- **Distance Updates**: Implements proper relaxation logic
- **Negative Cycle Detection**: Identifies when negative cycles exist
- **Output Format**: Produces results in standard format

## Time Complexity
O(VE) where V is vertices and E is edges

## Space Complexity
O(V + E) for storing graph and distance arrays

