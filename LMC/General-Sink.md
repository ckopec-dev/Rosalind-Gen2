# Rosalind Problem: General Sink in LMC

## Problem Understanding

A **general sink** in a directed graph is a vertex with no outgoing edges (in-degree = 0). In the context of this problem, we need to find a vertex that has no outgoing edges, meaning it's a sink in the graph.

## LMC Solution

```assembly
; General Sink Problem - LMC Implementation
; Find a vertex with no outgoing edges (sink)

      LDA ONE       ; Load 1 into accumulator
      STA COUNT     ; Initialize count to 1
      LDA ZERO      ; Load 0 into accumulator
      STA FOUND     ; Initialize found flag to 0

LOOP  LDA COUNT     ; Load current vertex number
      STA VERTEX    ; Store vertex number
      LDA VERTEX    ; Load vertex number
      LDA EDGE      ; Load edge data
      LDA ZERO      ; Load 0
      SUB EDGE      ; Subtract edge from 0
      BRZ FOUND_SINK ; If edge is 0, we found a sink
      LDA COUNT     ; Load count
      LDA ONE       ; Load 1
      ADD ONE       ; Add 1 to count
      STA COUNT     ; Store new count
      BRP LOOP      ; If count > 0, continue loop

FOUND_SINK
      LDA VERTEX    ; Load vertex number
      OUT           ; Output vertex number
      HLT           ; Halt

; Data section
COUNT   DAT 0       ; Current vertex counter
FOUND   DAT 0       ; Flag indicating if sink found
VERTEX  DAT 0       ; Current vertex being checked
EDGE    DAT 0       ; Edge data for current vertex

; Constants
ZERO    DAT 0       ; Zero value
ONE     DAT 1       ; One value
```

## Alternative Implementation (More Complete)

```assembly
; General Sink - Complete LMC Solution
; Input: Graph represented as adjacency list
; Output: Vertex number of sink (vertex with no outgoing edges)

      LDA INIT      ; Initialize vertex counter
      STA VERTEX    ; Store in vertex register

MAIN_LOOP
      LDA VERTEX    ; Load current vertex
      LDA VERTEX    ; Load vertex again
      LDA EDGE      ; Load edge information
      LDA ZERO      ; Load 0
      SUB EDGE      ; Subtract edge from 0
      BRZ IS_SINK   ; If result is 0, this is a sink
      LDA VERTEX    ; Load vertex
      LDA ONE       ; Load 1
      ADD ONE       ; Add 1 to vertex
      STA VERTEX    ; Store new vertex
      BRP MAIN_LOOP ; Continue if positive

IS_SINK
      LDA VERTEX    ; Load vertex number
      OUT           ; Output the sink vertex
      HLT           ; Halt program

; Data section
VERTEX  DAT 0       ; Current vertex being checked
EDGE    DAT 0       ; Edge data for vertex
INIT    DAT 1       ; Initial vertex number
ZERO    DAT 0       ; Zero constant
ONE     DAT 1       ; One constant
```

## Key Concepts

1. **Sink Detection**: A sink is a vertex with no outgoing edges
2. **Loop Control**: Iterate through vertices until sink is found
3. **Conditional Branching**: Use branch if zero (BRZ) to detect sink
4. **Output**: Print the vertex number when sink is found

## How It Works

1. Start with vertex 1
2. For each vertex, check if it has outgoing edges
3. If vertex has no outgoing edges (edge value = 0), it's a sink
4. Output the sink vertex number and halt
5. If not a sink, continue to next vertex

The LMC program uses the fundamental operations of:
- **LDA** (Load Accumulator)
- **STA** (Store Accumulator)
- **ADD** (Add)
- **SUB** (Subtract)
- **BRZ** (Branch if Zero)
- **BRP** (Branch if Positive)
- **OUT** (Output)
- **HLT** (Halt)

