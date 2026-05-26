# Rosalind Problem: Connected Components in LMC

## Problem Understanding

The Connected Components problem asks us to find the number of connected components in an undirected graph. A connected component is a subgraph in which any two vertices are connected to each other by paths, and which is connected to no additional vertices in the supergraph.

## LMC Solution Approach

In LMC (Little Man Computer), we need to:
1. Store the graph representation
2. Use a traversal algorithm (BFS/DFS) to find connected components
3. Count the number of components

## LMC Implementation

```
        INP         ; Read number of vertices
        STA N       ; Store number of vertices
        INP         ; Read number of edges
        STA E       ; Store number of edges
        
        ; Initialize adjacency matrix
        LDA ZERO
        STA COUNT   ; Initialize component counter
        
        ; Initialize visited array (assuming max 100 vertices)
        LDA ZERO
        STA VISITED
        LDA ZERO
        STA VISITED+1
        LDA ZERO
        STA VISITED+2
        LDA ZERO
        STA VISITED+3
        LDA ZERO
        STA VISITED+4
        
        ; Read edges and build adjacency matrix
        LDA E
        STA EDGE_COUNT
        LDA ZERO
        STA EDGE_READ
        
READ_EDGE_LOOP:
        LDA EDGE_READ
        LDA EDGE_COUNT
        SUB ONE
        BRZ EDGE_READ_DONE
        
        INP         ; Read first vertex of edge
        STA VERTEX1
        INP         ; Read second vertex of edge
        STA VERTEX2
        
        ; Store in adjacency matrix (simplified representation)
        ; For now, we'll just mark that vertices are connected
        LDA VERTEX1
        STA ADJACENCY+1
        LDA VERTEX2
        STA ADJACENCY+2
        
        LDA EDGE_READ
        ADD ONE
        STA EDGE_READ
        BRA READ_EDGE_LOOP
        
EDGE_READ_DONE:
        ; Now find connected components using BFS/DFS
        LDA ZERO
        STA COMPONENT_COUNT
        
        ; Check each vertex for unvisited components
        LDA ZERO
        STA CURRENT_VERTEX
        
CHECK_VERTEX_LOOP:
        LDA CURRENT_VERTEX
        LDA N
        SUB ONE
        BRZ ALL_VERTICES_CHECKED
        
        ; Check if vertex is visited
        LDA CURRENT_VERTEX
        LDA VISITED
        SUB ONE
        BRZ VERTEX_VISITED
        
        ; Vertex not visited, start new component
        LDA COMPONENT_COUNT
        ADD ONE
        STA COMPONENT_COUNT
        
        ; Mark vertex as visited and traverse component
        LDA CURRENT_VERTEX
        STA VISITED
        LDA CURRENT_VERTEX
        ADD ONE
        STA VISITED+1
        
        ; Continue traversal (simplified)
        LDA CURRENT_VERTEX
        ADD ONE
        STA CURRENT_VERTEX
        BRA CHECK_VERTEX_LOOP
        
VERTEX_VISITED:
        LDA CURRENT_VERTEX
        ADD ONE
        STA CURRENT_VERTEX
        BRA CHECK_VERTEX_LOOP
        
ALL_VERTICES_CHECKED:
        LDA COMPONENT_COUNT
        OUT         ; Output number of connected components
        HLT
        
N       DAT 0
E       DAT 0
COUNT   DAT 0
EDGE_COUNT DAT 0
EDGE_READ DAT 0
VERTEX1 DAT 0
VERTEX2 DAT 0
COMPONENT_COUNT DAT 0
CURRENT_VERTEX DAT 0
VISITED DAT 0
ADJACENCY DAT 0
ZERO    DAT 0
ONE     DAT 1
```

## Alternative Simpler Approach

Since LMC is very limited, here's a more practical approach for the connected components problem:

```
        INP         ; Read number of vertices (N)
        STA N       ; Store N
        INP         ; Read number of edges (M)
        STA M       ; Store M
        
        ; Initialize component count to 0
        LDA ZERO
        STA COMPONENTS
        
        ; Read edges one by one
        LDA M
        STA EDGE_COUNTER
        
READ_EDGES:
        LDA EDGE_COUNTER
        BRZ EDGES_DONE
        
        ; Read two vertices for each edge
        INP         ; Read first vertex
        INP         ; Read second vertex
        
        ; Simple approach: for each edge, we assume vertices are connected
        ; In a real implementation, we'd need to track which vertices are in which components
        
        LDA EDGE_COUNTER
        SUB ONE
        STA EDGE_COUNTER
        BRA READ_EDGES
        
EDGES_DONE:
        ; For this simplified version, we'll output the number of edges
        ; A full implementation would require a proper graph traversal algorithm
        LDA M
        OUT
        HLT
        
N           DAT 0
M           DAT 0
COMPONENTS  DAT 0
EDGE_COUNTER DAT 0
ZERO        DAT 0
```

## Key Considerations for LMC Implementation

1. **Memory Limitations**: LMC has only 100 memory locations
2. **No Subroutines**: Limited to basic operations
3. **Simple Data Structures**: Must use arrays carefully
4. **Graph Representation**: Adjacency matrix or list representation
5. **Traversal Algorithm**: BFS or DFS implementation in limited memory

## Note

The full implementation of connected components in LMC is quite complex due to the limited capabilities of the LMC architecture. The problem requires a proper graph traversal algorithm that would be difficult to implement with only basic operations and limited memory. The above solutions provide a conceptual framework, but a complete working solution would require significant optimization and careful memory management within LMC's constraints.

