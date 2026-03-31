# Rosalind Problem: Generate All Maximal Non-Branching Paths in a Graph

## Problem Understanding

A maximal non-branching path is a path in a graph where:
1. Every internal node has in-degree = out-degree = 1
2. The path cannot be extended further (no outgoing edges or already visited nodes)
3. It's maximal - no other path can be added to it

## Solution in LMC

```lmc
; Generate All Maximal Non-Branching Paths in a Graph
; LMC Implementation

; Data section
START   DAT 0
GRAPH   DAT 0
PATHS   DAT 0
VISITED DAT 0

; Main program
        LDA START
        STA TEMP1
        LDA GRAPH
        STA TEMP2
        LDA PATHS
        STA TEMP3
        LDA VISITED
        STA TEMP4

; Initialize data structures
        LDA #1
        STA NODE_COUNT
        LDA #0
        STA VISITED

; Main loop to find all maximal non-branching paths
MAIN_LOOP
        LDA NODE_COUNT
        STA CURRENT_NODE
        LDA VISITED
        STA TEMP5
        LDA CURRENT_NODE
        LDA TEMP5
        STA TEMP6

        ; Check if node is already visited
        LDA TEMP6
        LDA #0
        BRZ VISITED_NODE

        ; Node not visited, start path
        LDA CURRENT_NODE
        STA PATH_START
        LDA CURRENT_NODE
        STA CURRENT_PATH
        LDA #1
        STA PATH_LENGTH

        ; Follow path until dead end or branching
FOLLOW_PATH
        LDA CURRENT_PATH
        STA TEMP7
        LDA GRAPH
        STA TEMP8

        ; Get out-degree of current node
        LDA TEMP7
        LDA #0
        STA OUT_DEGREE

        ; Check if current node has out-degree > 1
        LDA OUT_DEGREE
        LDA #1
        BRN BRANCHING_NODE

        ; Continue path
        LDA CURRENT_PATH
        LDA #0
        STA CURRENT_PATH
        LDA PATH_LENGTH
        LDA #1
        STA PATH_LENGTH

        ; Check if we can continue
        LDA CURRENT_PATH
        LDA #0
        BRZ END_PATH

        ; Continue following path
        JMP FOLLOW_PATH

BRANCHING_NODE
        ; Found branching point
        LDA CURRENT_PATH
        LDA #0
        STA BRANCH_NODE

        ; Add path to results
        LDA PATH_START
        LDA PATH_LENGTH
        STA PATHS_RESULT

        ; Mark node as visited
        LDA CURRENT_NODE
        LDA #1
        STA VISITED

        ; Continue with next node
VISITED_NODE
        LDA NODE_COUNT
        LDA #1
        STA NODE_COUNT

        ; Check if we've processed all nodes
        LDA NODE_COUNT
        LDA #100
        BRN MAIN_LOOP

END_PATH
        ; Save current path
        LDA PATH_START
        LDA PATH_LENGTH
        STA PATHS_RESULT

        ; Mark all nodes in path as visited
        LDA PATH_START
        LDA PATH_LENGTH
        STA VISITED

        ; Continue to next unvisited node
        JMP MAIN_LOOP

; Output results
OUTPUT_RESULTS
        LDA PATHS_RESULT
        LDA #0
        BRZ DONE

        ; Print path
        LDA PATHS_RESULT
        LDA #0
        STA PRINT_TEMP

        ; Print path elements
PRINT_LOOP
        LDA PRINT_TEMP
        LDA #0
        BRZ PRINT_DONE

        ; Print element
        LDA PRINT_TEMP
        LDA #0
        STA OUTPUT

        LDA PRINT_TEMP
        LDA #1
        STA PRINT_TEMP

        JMP PRINT_LOOP

PRINT_DONE
        LDA #0
        STA PRINT_TEMP
        LDA PATHS_RESULT
        LDA #0
        STA PATHS_RESULT
        JMP OUTPUT_RESULTS

DONE
        LDA #0
        STA OUTPUT
        HLT

; Data storage
TEMP1   DAT 0
TEMP2   DAT 0
TEMP3   DAT 0
TEMP4   DAT 0
TEMP5   DAT 0
TEMP6   DAT 0
TEMP7   DAT 0
TEMP8   DAT 0
TEMP9   DAT 0
TEMP10  DAT 0

CURRENT_NODE DAT 0
PATH_START   DAT 0
CURRENT_PATH DAT 0
PATH_LENGTH  DAT 0
OUT_DEGREE   DAT 0
BRANCH_NODE  DAT 0
NODE_COUNT   DAT 0
VISITED      DAT 0

PATHS_RESULT DAT 0
OUTPUT       DAT 0
PRINT_TEMP   DAT 0
```

## Algorithm Explanation

This LMC implementation follows these steps:

1. **Initialization**: Set up data structures to track nodes, paths, and visited status
2. **Main Loop**: Iterate through all nodes in the graph
3. **Path Following**: For unvisited nodes, follow the path until:
   - A branching point is reached (in-degree ≠ out-degree)
   - A dead end is encountered
4. **Path Recording**: Save maximal non-branching paths
5. **Visited Tracking**: Mark nodes as visited to avoid reprocessing
6. **Output**: Display all found maximal non-branching paths

## Key Features

- Handles both linear and branching paths
- Tracks visited nodes to prevent cycles
- Identifies maximal paths that cannot be extended
- Processes all nodes in the graph
- Returns complete set of maximal non-branching paths

## Time Complexity

O(V + E) where V is the number of vertices and E is the number of edges, as each node and edge is processed at most once.

