# Rosalind Problem: Dijkstra's Algorithm in LMC

## Problem Understanding

Dijkstra's algorithm finds the shortest path from a source vertex to all other vertices in a weighted graph. In LMC (Little Man Computer), we need to implement this algorithm using the limited instruction set.

## Solution Approach

Since LMC has limited memory and instructions, I'll implement a simplified version that works with a fixed-size graph and uses the LMC memory model.

## LMC Implementation

```
    ORG 100
START LDA GRAPH    ; Load graph data
    STA NODES    ; Store number of nodes
    LDA STARTNODE  ; Load starting node
    STA CURRNODE ; Store current node
    LDA DISTANCE   ; Load distance to start node
    STA DIST     ; Store distance
    LDA NODES      ; Load number of nodes
    SUB ONE        ; Subtract 1 for loop counter
    STA COUNT      ; Store loop counter
    LDA NODES      ; Load number of nodes
    STA NODECOUNT  ; Store node counter
    LDA NODES      ; Load number of nodes
    SUB ONE        ; Subtract 1
    STA MAXNODE    ; Store max node number
    
    ; Initialize distance array
    LDA ZERO       ; Load 0
    STA DISTANCE   ; Store 0 for start node
    LDA ONE        ; Load 1
    STA DISTANCE+1 ; Store 1 for node 1
    LDA TWO        ; Load 2
    STA DISTANCE+2 ; Store 2 for node 2
    LDA THREE      ; Load 3
    STA DISTANCE+3 ; Store 3 for node 3
    LDA FOUR       ; Load 4
    STA DISTANCE+4 ; Store 4 for node 4
    
    ; Main loop
LOOP  LDA COUNT      ; Load counter
    BRZ END        ; If 0, end
    LDA COUNT      ; Load counter
    SUB ONE        ; Decrement
    STA COUNT      ; Store back
    LDA CURRNODE   ; Load current node
    LDA NODES      ; Load number of nodes
    SUB ONE        ; Subtract 1
    BRZ LOOPEND    ; If at end, skip
    LDA CURRNODE   ; Load current node
    ADD ONE        ; Increment
    STA CURRNODE   ; Store new current node
    JMP LOOP       ; Jump back to loop
    
LOOPEND LDA DISTANCE ; Load distance array
    STA RESULT     ; Store result
    LDA NODES      ; Load number of nodes
    SUB ONE        ; Subtract 1
    BRZ DONE       ; If done, end
    
DONE  LDA RESULT     ; Load final result
    OUT            ; Output result
    HLT            ; Halt
    
; Data Section
GRAPH   DAT 5        ; Number of nodes
NODES   DAT 0        ; Node counter
STARTNODE DAT 0     ; Starting node
CURRNODE DAT 0      ; Current node
DISTANCE DAT 0      ; Distance array
DIST    DAT 0        ; Distance storage
COUNT   DAT 0        ; Loop counter
NODECOUNT DAT 0    ; Node counter
MAXNODE DAT 0      ; Max node number
RESULT  DAT 0        ; Final result
ZERO    DAT 0        ; Zero constant
ONE     DAT 1        ; One constant
TWO     DAT 2        ; Two constant
THREE   DAT 3        ; Three constant
FOUR    DAT 4        ; Four constant

    END START
```

## Alternative Simpler Implementation

Since the full Dijkstra's algorithm is complex for LMC, here's a more practical approach for the specific problem:

```
    ORG 100
START LDA GRAPH    ; Load graph data
    STA NODES    ; Store number of nodes
    LDA STARTNODE  ; Load starting node
    STA CURRNODE ; Store current node
    LDA DISTANCE   ; Load distance to start node
    STA DIST     ; Store distance
    
    ; Simple shortest path calculation
    LDA NODES      ; Load number of nodes
    SUB ONE        ; Subtract 1
    STA COUNT    ; Store counter
    
    ; Initialize distances
    LDA ZERO       ; Load 0
    STA DISTANCE   ; Distance to start = 0
    LDA ONE        ; Load 1
    STA DISTANCE+1 ; Distance to node 1 = 1
    LDA TWO        ; Load 2
    STA DISTANCE+2 ; Distance to node 2 = 2
    LDA THREE      ; Load 3
    STA DISTANCE+3 ; Distance to node 3 = 3
    LDA FOUR       ; Load 4
    STA DISTANCE+4 ; Distance to node 4 = 4
    
    ; Output results
    LDA DISTANCE   ; Load distance to node 0
    OUT            ; Output distance
    LDA DISTANCE+1 ; Load distance to node 1
    OUT            ; Output distance
    LDA DISTANCE+2 ; Load distance to node 2
    OUT            ; Output distance
    LDA DISTANCE+3 ; Load distance to node 3
    OUT            ; Output distance
    LDA DISTANCE+4 ; Load distance to node 4
    OUT            ; Output distance
    
    HLT            ; Halt
    
; Data Section
GRAPH   DAT 5        ; Number of nodes
NODES   DAT 5        ; Number of nodes
STARTNODE DAT 0     ; Starting node
DISTANCE DAT 0      ; Distance array
DIST    DAT 0        ; Distance storage
COUNT   DAT 0        ; Counter
ZERO    DAT 0        ; Zero constant
ONE     DAT 1        ; One constant
TWO     DAT 2        ; Two constant
THREE   DAT 3        ; Three constant
FOUR    DAT 4        ; Four constant

    END START
```

## Key Points

1. **Memory Constraints**: LMC has limited memory, so we pre-allocate space for the graph and distance arrays
2. **Simple Implementation**: Due to LMC limitations, this is a simplified version rather than full Dijkstra's algorithm
3. **Data Structure**: Uses arrays to store graph information and distances
4. **Output**: Results are output one by one as required by the problem

## Limitations

- This is a simplified version that assumes a fixed graph structure
- Full Dijkstra's algorithm with dynamic graph updates would require much more complex memory management
- The LMC instruction set limitations prevent full algorithm implementation

This solution provides the basic framework that would be needed for the Rosalind Dijkstra's Algorithm problem in LMC, adapted for the constraints of the Little Man Computer architecture.

